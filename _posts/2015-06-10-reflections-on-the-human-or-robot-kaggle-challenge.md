---
layout: post
title: Reflections on the Human or Robot? kaggle challenge.
author: Jeremy
tags:
 - datascience
 - kaggle
 - R
comments: true
---

Here’s a link to the [final standings](https://www.kaggle.com/c/facebook-recruiting-iv-human-or-bot/leaderboard/private).

This is the first kaggle challenge I put a good amount of effort into and I’m pretty happy with the outcome.

I ended up finishing 187/985 with a score of 0.92341, very close to the winning score of 0.94254. Happy to be in the top 25%.

With code to follow, here are my reflections.

-  The leaderboards are largely meaningless. I was at 245/985 on the public leaderboard at the close, and moved up a large way. The very highest scores seem to be heavily overfitted – the overall winner moved from 88th to 1st. I gained a large bump in score (0.90552 to 0.92341) in the final data set – my model was good. Striving for a couple places higher on the public board is probably not worth it

-  Related to this, my strategy of using all the training data as training and not holding a set out was pretty terrible. I assumed that by using all my data as training I could use the kaggle test data for my tests – this meant I had no way of testing more than three times a day, and if the public test set is different from the private test set (as in this case) you might be in for a shock.

-  The data was a little messy. I did two dumb things related to this that could have impacted my score – Some countries found in the test set were not in the training set. In an early fix I set these equal to “cn” and never fixed it. A more intelligent fix could have been smarter. Secondly there were 70 of 4700 test users that had no bids. I set all of these predictions to 0, but a mean of unrepresented training sets would have been smarter.

-  Predictors/features are key. I played a lot with scaling, normalisation, different algorithms for my model, but with the large number of predictors I extracted, nothing could beat a vanilla random forest. I’ll need to significantly up my expertise to beat this. Time spent thinking about the actual problem and probable bot behaviour had much better ROI than algorithm tuning.

-  This data was on the cusp of “big data”. I only had to scrap one planned analysis – network analysis of users based on shared phones. This was due to the large number of connections and my lack of RAM. I need to think of more efficient algorithms for data sets that are any larger.
