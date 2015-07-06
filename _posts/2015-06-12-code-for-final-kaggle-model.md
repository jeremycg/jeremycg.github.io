---
layout: post
title: Code for final Kaggle model.
author: Jeremy
tags:
 - datascience
 - kaggle
 - R
---

This is (more or less) the code that got me my best answer. After reading through a couple of other peoples code, I should have looked more carefully at the time variables, and figured out times etc. Given I got 0.92341 compared to the winners 0.94254, it's not super important, but it is the small differences that matter.

It's in R, written up in knitr -> HTML.


Let's read in some libraries for analysis:


{% highlight r %}
library(data.table)
library(plyr)
library(dplyr)
library(caret)
library(knitr)
library(bit64)
library(doSNOW)
library(igraph)
{% endhighlight %}

And now let's read in the data. Im using fread from the data.table package, as it is a large file.


{% highlight r %}
setwd("C:/Users/jeremy/Desktop/kaggle/kaggle/fbbid/")
train<-fread("train.csv")
bids<-fread("bids.csv")
{% endhighlight %}


First, how many total bids did each user make? We can assume bots are making a ton.

We group_by bidder_id and then use n() to take the length. n() is a dplyr specific function implemented in rcpp, so is much faster than length or nrow.


{% highlight r %}
totalbids<-function(dataframe){
  dataframe%>%group_by(bidder_id)%>%summarise(totalbids=n())
}
{% endhighlight %}

Next, we can take the number of bids per auction, on average. Again, I would guess bots are very high.

First, group the data by bidder_id and auction, take the number of bids each user had on each auction.
Then we group it again by user, and find the average for each. mean() is again very fast in dplyr.


{% highlight r %}
meanbids<-function(dataframe){
  dataframe%>%group_by(bidder_id,auction)%>%summarise(numberbids=n())%>%
    group_by(bidder_id)%>%summarise(meanbids=mean(numberbids))
}
{% endhighlight %}

Next, number of distinct auctions bid on - This will depend on bot coding - if a bot is used to push up prices on specific auctions, maybe it will have a low number, otherwsie it might be high.

First, let's group the data into users and auctions, then regroup into users and use n().


{% highlight r %}
numauctions<-function(dataframe){
  dataframe%>%group_by(bidder_id,auction)%>%summarise(numberbids=n())%>%
    group_by(bidder_id)%>%summarise(numauctions=n())
}
{% endhighlight %}

Now maybe bots use a user agent switcher so they appear to use different devices? All user agents in this are classed as unique phone models, so if a user uses 8000 phones, they are probably a bot.

again, using pylr we group and and use n().


{% highlight r %}
numdevices<-function(dataframe){
  dataframe%>%group_by(bidder_id,device)%>%summarise(timesused=n())%>%
    group_by(bidder_id)%>%summarise(numdevices=n())
}
{% endhighlight %}

A bot might only be going after one specific type of merchandise - let's check


{% highlight r %}
numtypes<-function(dataframe){
  dataframe%>%group_by(bidder_id,merchandise)%>%summarise(timeseachtype=n())%>%
    group_by(bidder_id)%>%summarise(numtypes=n())
}
{% endhighlight %}

After running this predictor, we get no users with more than 1 type of merchandise! Weird.

So, bots most common (or only!) merchandise might be of a particular type - ie home goods might not be worth coding a bot for. Let's check - it's easy as we know there is no replication


{% highlight r %}
commonmerch<-function(dataframe){
  dataframe%>%group_by(bidder_id)%>%summarise(commonmerch=merchandise[1])
}
{% endhighlight %}

Now let's try country number - someone bidding from multiple countries is up to something.


{% highlight r %}
numcountry<-function(dataframe){
  dataframe%>%group_by(bidder_id,country)%>%summarise(n())%>%
    group_by(bidder_id)%>%summarise(numcountry=n())
}
{% endhighlight %}

Now find the most common country - some countries might be more susceptible to bots than others.


{% highlight r %}
commoncountry<-function(dataframe){
  dataframe%>%group_by(bidder_id,country)%>%summarise(number=n())%>%
    group_by(bidder_id)%>%summarise(commoncountry=country[which.max(number)])
}
{% endhighlight %}

I'm going to turn on multiple cores here - dplyr is a little parallelised, so it might help. It definitely will help when it comes to training.


{% highlight r %}
cl <- makeCluster(4)
registerDoSNOW(cl)
{% endhighlight %}

Now let's do some time series stuff. If the bots are poorly coded, they will always bid exactly, say, 10 seconds after the last bid. So, if we can calculate the diffs for each, we will get a picture of anyone doing something weird.

This is a little trickier, and very slow as it is harder to vectorise. I'll use diff and add a 0 at the start. We will only run this once, and then use the data frame for multiple analyses.


{% highlight r %}
differences<-function(df){
  df<-df[order(df$time), ]
  df$time<-c(0,diff(df$time))
  df
}
timediffbids<-bids%>%group_by(auction)%>%do(.,differences(.))
{% endhighlight %}

Now let's use that data to get mean time since last bid, and the percentage of bids in which the user was the first, and the percentage of bids in which the user was the first. I'm getting mutliple predictors here as they are quicker when gathered at once. call on timediffbids.


{% highlight r %}
bidtimes<-function(dataframe){
  dataframe%>%group_by(bidder_id)%>%
    summarise(averagetimetobid=mean(time),percentfirst=sum(.[["time"]]==0)/n())
}
{% endhighlight %}

Now depending on how the site works, a bid against yourself might be smart (if there is a reserve), or a way of bidding up the price (bot). This one again is slow. Either way, lets make it a feature. Call this on timediffbids.


{% highlight r %}
bidself<-function(df){
  df$self<-0
  if(nrow(df)==1){
    return(df)}
  df$prevbidder<-c("none",df$bidder_id[-nrow(df)])
  df$self[df$bidder_id==df$prevbidder]<-1
  df$prevbidder<-NULL
  df
}

bidagainstself<-function(dataframe){
  dataframe%>%group_by(auction)%>%do(.,bidself(.))%>%
    group_by(bidder_id)%>%summarise(percentself=sum(.[["self"]]==1)/n())
}
{% endhighlight %}

Now let's add in the number of final bids. We can assume bots either are trying to jack up prices but lose, or win lots of auctions. Either way there should be a signal here. Let's get proportion final bids, and total number. Again, run this on the timediffbids.


{% highlight r %}
numfinal<-function(dataframe){
  dataframe%>%group_by(auction)%>%mutate(last = c(rep(0,n()-1), 1))%>%
    group_by(bidder_id)%>%summarise(percentfinal=sum(last),percentfinal=mean(last))
}
{% endhighlight %}

Now we have a percentage of mismatched bids - It is possible users are more likely to find something in a category noone else used to find it, whereas bots know exactly what they want. We know from above that noone switches type midstream, which simplifies analysis.


{% highlight r %}
#pretty brutal function!
percentmismatch<-function(dataframe){
  mostcommoninauction<-dataframe%>%group_by(auction,merchandise)%>%
    summarise(number=n())%>%group_by(auction)%>%
      summarise(mostcommoninauction=merchandise[which.max(number)])
  merge(mostcommoninauction,dataframe,by="auction")%>%group_by(bidder_id,auction)%>%
    summarise(mismatch=sum(mostcommoninauction[1]==merchandise[1]))%>%group_by(bidder_id)%>%
      summarise(percentmismatch=mean(mismatch))
}
{% endhighlight %}

Let's check now for using multiple ipaddresses. This is probably highly correlated with number of countries:


{% highlight r %}
numipaddress<-function(dataframe){
  dataframe%>%group_by(bidder_id,ip)%>%summarise(n())%>%
    group_by(bidder_id)%>%summarise(numipaddress=n())
}
{% endhighlight %}

As the phones are based on models, it is possible the bot software is only using a small subset - lets get the most common phone type used for each user.


{% highlight r %}
commonphone<-function(dataframe){
  dataframe%>%group_by(bidder_id,device)%>%summarise(number=n())%>%
    group_by(bidder_id)%>%summarise(commonphone=device[which.max(number)])
}
{% endhighlight %}

Shared use of the same ip could be an indication of cheating - let's find a value for number of shared addresses.


{% highlight r %}
percentsharedip<-function(dataframe){
  shareips<-dataframe%>%group_by(ip,bidder_id)%>%summarise(usesofaddress=n())%>%
    group_by(ip)%>%summarise(totalbidders=sum(n()>1))
  merge(shareips,dataframe,by="ip")%>%group_by(bidder_id)%>%
    summarise(percentsharedip=mean(totalbidders))
}
{% endhighlight %}

Now let' do some network analysis. I'm going to connect every user by auctions bid on together (each auction = 2 connections) and get page rank etc from the resulting network. This stuff is memory intensive, I did them both in a seperate session, then added them onto my features csv.


{% highlight r %}
groupbyauction<-bids%>%group_by(bidder_id,auction)%>%summarise(bidsperauction=n())
removesingleauction<-bids%>%group_by(auction)%>%summarise(totalbidders=length(unique(.[["bidder_id"]])))
removesingleauction<-removesingleauction[removesingleauction$totalbidders==1,]
groupbyauction<-groupbyauction[!(groupbyauction$auction %in% removesingleauction$auction),]%>%as.data.frame()


groupbyauctioncombs<-groupbyauction%>%group_by(auction)%>%do(as.data.frame(t(combn(.[["bidder_id"]],2))))
auctiongraphs<-graph.data.frame(groupbyauctioncombs[,2:3])
auctionpage<-page.rank(auctiongraphs)
auctiondegree<-degree(auctiongraphs)
auctioncloseness<-closeness(auctiongraphs)
auctionbetweenness<-betweenness(auctiongraphs)
auctioneigen<-evcent(auctiongraphs)

auctiongraphvalues<-data.frame(names(auctionpage$vector))
auctiongraphvalues$auctionpagerank<-unname(auctionpage$vector)
auctiongraphvalues$auctiondegree<-unname(auctiondegree)
auctiongraphvalues$auctioncloseness<-unname(auctioncloseness)
auctiongraphvalues$auctionbetweenness<-unname(auctionbetweenness)
auctiongraphvalues$auctioneigen<-unname(auctioneigen$vector)
{% endhighlight %}

And the same, but for ip addresses. If two users share an ip address, they get a connectedness of two. Again, memory intensive, so not eval here.


{% highlight r %}
#groupperip
groupbyip<-bids%>%group_by(bidder_id,ip)%>%summarise(bidsperip=n())
removesingleip<-bids%>%group_by(ip)%>%summarise(totalbidders=length(unique(.[["bidder_id"]])))
removesingleip<-removesingleip[removesingleip$totalbidders==1,]
groupbyip<-groupbyip[!(groupbyip$ip %in% removesingleip$ip),]%>%as.data.frame()


groupbyipcombs<-groupbyip%>%group_by(ip)%>%do(as.data.frame(t(combn(.[["bidder_id"]],2))))
ipgraphs<-graph.data.frame(groupbyipcombs[,2:3])
ippage<-page.rank(ipgraphs)
ipdegree<-degree(ipgraphs)
ipcloseness<-closeness(ipgraphs)
ipbetweenness<-betweenness(ipgraphs)
ipeigen<-evcent(ipgraphs)

ipgraphvalues<-data.frame(names(ippage$vector))
ipgraphvalues$ippagerank<-unname(ippage$vector)
ipgraphvalues$ipdegree<-unname(ipdegree)
ipgraphvalues$ipcloseness<-unname(ipcloseness)
ipgraphvalues$ipbetweenness<-unname(ipbetweenness)
ipgraphvalues$ipeigen<-unname(ipeigen$vector)
write.csv(ipgraphvalues,file="ipgraphvalues.csv")
{% endhighlight %}

share ip with a bot?


{% highlight r %}
shareipwithbot<-function(dataframe,trainingset){
  knownbots<-train[train$outcome==1,]
  botips<-unique(merge(bids,knownbots,by="bidder_id",all.x=FALSE)$ip)
  dataframe%>%group_by(bidder_id,ip)%>%summarise(num=n())%>%group_by(bidder_id)%>%
    summarise(share=sum(any(ip %in% botips)))
}
{% endhighlight %}

How many devices per auction? mean and sd


{% highlight r %}
biddistribution<-function(df){
  df %>% group_by(bidder_id, auction, device) %>%
      summarise(n()) %>%
      group_by(bidder_id, auction) %>%
      summarise(perdevice = n()) %>%
      group_by(bidder_id) %>%
      summarise(meandevicesperauction = mean(perdevice), sddevicesperauction = sd(perdevice))
}
{% endhighlight %}


Now run everthing


{% highlight r %}
fulldata<-join_all(list(totalbids(bids),meanbids(bids),numauctions(bids),
                        numdevices(bids),commonmerch(bids),numcountry(bids),
                        commoncountry(bids),bidtimes(timediffbids),
                        bidagainstself(timediffbids),numfinal(timediffbids),
                        percentmismatch(bids),numipaddress(bids),
                        commonphone(bids),percentsharedip(bids),shareipwithbot(bids,train),
                        biddistribution(bids)),
                   by='bidder_id',type='full')
write.csv(fulldata,file="fulldata.csv",row.names=FALSE)
{% endhighlight %}

Let's read in the file, and make a couple of composite ones.


{% highlight r %}
fulldata<-read.csv("fulldata.csv")
fulldata$ippercountry<-fulldata$numipaddress/fulldata$numcountry
fulldata$wonauctions<-fulldata$percentfinal/fulldata$numauctions
{% endhighlight %}

Add in the network analysis:


{% highlight r %}
ipgraphvalues<-read.csv("ipgraphvalues.csv")
ipgraphvalues<-ipgraphvalues[2:7]
names(ipgraphvalues)<-c("bidder_id","ippagerank","ipdegree","ipcloseness","ipbetweeness","ipeigen")

auctiongraphvalues<-read.csv("auctiongraphvalues.csv")
auctiongraphvalues<-auctiongraphvalues[2:7]
names(auctiongraphvalues)<-c("bidder_id","auctionpagerank","auctiondegree","auctioncloseness","auctionbetweeness","auctioneigen")

fulldata<-join_all(list(fulldata,ipgraphvalues,auctiongraphvalues),by="bidder_id")
fulldata[16:29][is.na(fulldata[16:29])]<-0
{% endhighlight %}

First, let's set our fitting parameters. I'm using 10 fold crossvalidation


{% highlight r %}
fitControl <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 10,
  savePred=T)
{% endhighlight %}

Now we need to merge and partition our data.
First we merge to make sure everything is in a sensible order. We then split out predictors and outcomes.


{% highlight r %}
mergeddata<-merge(fulldata,train,by="bidder_id")
mergeddata<-as.data.frame(mergeddata)
predictors<-mergeddata[,-c(1,(ncol(mergeddata)-2):(ncol(mergeddata)))]
outcomes<-mergeddata[,ncol(mergeddata)]
{% endhighlight %}

Random Forest (not run)


{% highlight r %}
rffit1 <- train(mergeddata[,ncol(mergeddata)]~.,data=mergeddata[,-c(1,(ncol(mergeddata)-2):(ncol(mergeddata)))],method="rf",trControl = fitControl,verbose=FALSE,importance = TRUE)
print(rffit1)
{% endhighlight %}

stop cluster


{% highlight r %}
stopCluster(cl)
{% endhighlight %}

Now we can predict!

First read in the test data, and then merge it.
We have 70 bidders in the test data, but not in the bids sheet. Let's give them 0 as a prediction.


{% highlight r %}
test<-fread("test.csv")
mergeddata<-merge(fulldata,test,by="bidder_id")
mergeddata<-as.data.frame(mergeddata)
predictors<-mergeddata[,-c(1,(ncol(mergeddata)-1):(ncol(mergeddata)))]
{% endhighlight %}

Now let's predict.


{% highlight r %}
x=cbind(as.data.frame(mergeddata$bidder_id),predict(gbmFit1,predictors))
x$`predict(gbmFit1, predictors)`[x$`predict(gbmFit1, predictors)`<0]<-0
togiveempty<-mean(x$`predict(gbmFit1, predictors)`)
empty<-cbind(as.data.frame(
  test[!(test$bidder_id %in% fulldata$bidder_id)]$bidder_id),0)
names(empty)<-names(x)
x=rbind(x,empty)
write.csv(x,"outputgbm.csv")
{% endhighlight %}


{% highlight r %}
x=cbind(as.data.frame(mergeddata$bidder_id),predict(rffit1,predictors,na.action=na.roughfix))
names(x)<-c("bidder_id","prediction")
x$prediction[x$prediction<0]<-0
x$prediction[x$prediction>0.5]<-1
empty<-cbind(as.data.frame(
  test[!(test$bidder_id %in% fulldata$bidder_id)]$bidder_id),0)
names(empty)<-names(x)
x=rbind(x,empty)
write.csv(x,"outputrf.csv")
{% endhighlight %}
