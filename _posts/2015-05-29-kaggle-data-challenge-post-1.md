---
layout: post
title: Kaggle data challenge post 1.
author: Jeremy
tags:
 - datascience
 - kaggle
 - R
comments: true
---

  This is the first post of an occasional series on R, data science, Kaggle and my quest to make it through the book "The elements of statistical learning".

For a first post, here is a write up of my current kaggle problem. This knitr is an archived version - it wont hit my best score, and I haven't done too much data processing other than making predictors. At the time of writing it was enough to hit about the 50th percentile, and was about 0.1 lower than my best score.

Here's the kaggle challenge "Facebook Recruiting IV: Human or Robot?" - [link](https://www.kaggle.com/c/facebook-recruiting-iv-human-or-bot).

The challenge presents as an auction website, which has been overtaken by bots outbidding real people.
The user is to determine which users are bots and which are real. Presumably the owner will then ban the bots.

This code is available as r markdown on github at www.github.com/jeremycg/kaggle

Let's read in some libraries for analysis:


{% highlight r %}
library(data.table)
library(plyr)
library(dplyr)
library(caret)
library(knitr)
library(bit64)
{% endhighlight %}

And now let's read in the data. Im using fread from the data.table package, as it is a large file.


{% highlight r %}
setwd("C:/Users/jeremy/Desktop/kaggle/kaggle/fbbid/")
train<-fread("train.csv")
bids<-fread("bids.csv")
{% endhighlight %}



{% highlight text %}
##Read 0.0% of 7656334 rowsRead 8.6% of 7656334 rowsRead 17.0% of 7656334 rowsRead 25.5% of 7656334 rowsRead 34.2% of 7656334 rowsRead 43.0% of 7656334 rowsRead 51.5% of 7656334 rowsRead 60.0% of 7656334 rowsRead 68.3% of 7656334 rowsRead 76.3% of 7656334 rowsRead 84.2% of 7656334 rowsRead 92.5% of 7656334 rowsRead 7656334 rows and 9 (of 9) columns from 0.862 GB file in 00:00:19
{% endhighlight %}

Lets take a look:


  {% highlight r %}
  kable(head(train))
  {% endhighlight %}



  |bidder_id                             |payment_account                       |address                               | outcome|
  |:-------------------------------------|:-------------------------------------|:-------------------------------------|-------:|
  |91a3c57b13234af24875c56fb7e2b2f4rb56a |a3d2de7675556553a5f08e4c88d2c228754av |a3d2de7675556553a5f08e4c88d2c228vt0u4 |       0|
  |624f258b49e77713fc34034560f93fb3hu3jo |a3d2de7675556553a5f08e4c88d2c228v1sga |ae87054e5a97a8f840a3991d12611fdcrfbq3 |       0|
  |1c5f4fc669099bfbfac515cd26997bd12ruaj |a3d2de7675556553a5f08e4c88d2c2280cybl |92520288b50f03907041887884ba49c0cl0pd |       0|
  |4bee9aba2abda51bf43d639013d6efe12iycd |51d80e233f7b6a7dfdee484a3c120f3b2ita8 |4cb9717c8ad7e88a9a284989dd79b98dbevyi |       0|
  |4ab12bc61c82ddd9c2d65e60555808acqgos1 |a3d2de7675556553a5f08e4c88d2c22857ddh |2a96c3ce94b3be921e0296097b88b56a7x1ji |       0|
  |7eaefc97fbf6af12e930528151f86eb91bafh |a3d2de7675556553a5f08e4c88d2c228yory1 |5a1d8f28bc31aa6d72bef2d8fbf48b967hra3 |       0|



  {% highlight r %}
  kable(head(bids))
  {% endhighlight %}



  | bid_id|bidder_id                             |auction |merchandise |device |             time|country |ip             |url             |
  |------:|:-------------------------------------|:-------|:-----------|:------|----------------:|:-------|:--------------|:---------------|
  |      0|8dac2b259fd1c6d1120e519fb1ac14fbqvax8 |ewmzr   |jewelry     |phone0 | 9759243157894736|us      |69.166.231.58  |vasstdc27m7nks3 |
  |      1|668d393e858e8126275433046bbd35c6tywop |aeqok   |furniture   |phone1 | 9759243157894736|in      |50.201.125.84  |jmqlhflrzwuay9c |
  |      2|aa5f360084278b35d746fa6af3a7a1a5ra3xe |wa00e   |home goods  |phone2 | 9759243157894736|py      |112.54.208.157 |vasstdc27m7nks3 |
  |      3|3939ac3ef7d472a59a9c5f893dd3e39fh9ofi |jefix   |jewelry     |phone4 | 9759243157894736|in      |18.99.175.133  |vasstdc27m7nks3 |
  |      4|8393c48eaf4b8fa96886edc7cf27b372dsibi |jefix   |jewelry     |phone5 | 9759243157894736|in      |145.138.5.37   |vasstdc27m7nks3 |
  |      5|e8291466de91b0eb4e1515143c7f74dexy2yr |3vi4t   |mobile      |phone7 | 9759243157894736|ru      |91.107.221.27  |vasstdc27m7nks3 |



  {% highlight r %}
  nrow(train)
  {% endhighlight %}



  {% highlight text %}
  ## [1] 2013
  {% endhighlight %}



  {% highlight r %}
  nrow(bids)
  {% endhighlight %}



  {% highlight text %}
  ## [1] 7656334
  {% endhighlight %}

So, we have 2013 users to train on, with a little over 7.6 million bids.

The user data has obfuscated rows for bidder id, payment account and address, as well as the outcome - was it a bot?

The bid data has a bid id, bidder id (to link to the training data), an auction id, mechandise type (this can change per auction depending on how the user found it), device used to bid, time, country, ip, and referring url.

We dont really need to merge the datasets - we can add it on at the end.

Now we want predictors!

  First let's double check that there are no shared addresses or payment details in our training set:


{% highlight r %}
anyDuplicated(train$payment_account)
{% endhighlight %}



{% highlight text %}
## [1] 0
{% endhighlight %}



{% highlight r %}
anyDuplicated(train$address)
{% endhighlight %}



{% highlight text %}
## [1] 0
{% endhighlight %}

Nope! So we can put this to the side and not use it in any predictors.

I'm going to use plyr on this data, as it is really large - data.table is also nice.

Lets Slowly build up functions to make our table of predictors.

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

Let's check nor for using multiple ipaddresses. This is probably highly correlated with number of countries:


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

We could do some fun graph stuff with shared ip addresses - ie see who is connected and call whole networks scams - I'll not do that here. This might be the key to getting over 0.9?

For now, we have a ton of predictors, and a few more to come that are easier once they are all made. I'm going to call it all at once to make a data frame with them all in.

This will take a while! I reccomend saving it as a file so you don't have to run it again. In the raw knitr file, I've set to to not evaluate, so change it if you are playing along.


{% highlight r %}
fulldata<-join_all(list(totalbids(bids),meanbids(bids),numauctions(bids),
numdevices(bids),commonmerch(bids),numcountry(bids),
commoncountry(bids),bidtimes(timediffbids),
bidagainstself(timediffbids),numfinal(timediffbids),
percentmismatch(bids),numipaddress(bids),
commonphone(bids),percentsharedip(bids)),
by='bidder_id',type='full')
write.csv(fulldata,file="fulldata.csv",row.names=FALSE)
{% endhighlight %}

We now have 15 predictors! Let's read in the file, and make a couple of composite ones.


{% highlight r %}
fulldata<-read.csv("fulldata.csv")
fulldata$ippercountry<-fulldata$numipaddress/fulldata$numcountry
fulldata$wonauctions<-fulldata$percentfinal/fulldata$numauctions
{% endhighlight %}

Now comes the actual machine learning! We can do fun things like scale and turn categories into dummy variables. For now, I'll just leave it, and hope the fitting package takes care of it.

I'm going to use gradient boosting - as it is fast. Random Forest will probably be better, but it will take forever (about an hour on my laptop).

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

Now gradient boosting


{% highlight r %}
gbmFit1 <- train(x=predictors,y=outcomes,
                 method = "gbm",
                 trControl = fitControl,
                 verbose = FALSE)
print(gbmFit1)
{% endhighlight %}

Random Forest (not run)


{% highlight r %}
#rffit1 <- train(fulldata[,4]~.,data=fulldata[,5:18],method="rf",trControl = #fitControl,verbose=FALSE)
#print(rffit1)
{% endhighlight %}

Now we can predict!

  First read in the test data, and then merge it.
We have 70 bidders in the test data, but not in the bids sheet. Let's give them the average value of our prediction.


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
  test[!(test$bidder_id %in% fulldata$bidder_id)]$bidder_id),togiveempty)
names(empty)<-names(x)
x=rbind(x,empty)
write.csv(x,"output.csv")
{% endhighlight %}

This gave me a score of 0.76567 - about 0.1 worse than my best answer! So we need to do some data clean up and more sensible feature choice. It's a good start for a walkthrough though.
