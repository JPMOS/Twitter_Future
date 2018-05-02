# Twitter_Future
Future Orientation Tweets.
Replication Guide.

Unfortunately, due to Twitter code it is not allowed to share data sets anymore. However, if you wish to repicalate other researchers you can request the User ID's used but you would just have to run a search for the specific User ID's. 

FIRST. Get a twitter developer account it's easy. Just follow this. 

We used the package rtweet. Unfortunately, we can only provide the ID of the tweets due to the [twitter developer policy] (https://gwu-libraries.github.io/sfm-ui/posts/2017-09-14-twitter-data), not the tweets themselves. Unfortunately we deleted these on accident. 

When starting to scrape data from Twitter, there are two things that need to be done. First is to set up a Twitter account and second is to request a Twitter token to get access to the API through this link https://developer.twitter.com/en/docs/basics/authentication/guides/access-tokens.

A request is simple and Twitter responds very quickly. Simply fill out what your purpose is for the token and the turn around time is very quick. 

After the Twitter Tokens are gathered you can save them as an R object (.RDS) to be able to call the tokens in the RScript. 

Once the token is saved install the R package tweetR, the tweetR package easily interacts with the Twitter API to allow users to scrape Twitter data. 

After that to automate the process we accessed a remote server to avoid having to run the code on our personal machines. 

The R scipt and the Twitter Token were then exported to the remote server. The R script will call the Twitter token for authentification. 

Using the Rbatch command to run the script, a command called (nohup.out) will continue to run the script after closing the server session. It is important to remember to close the access to the remote access server with (end) as there could be dissruptions if you terminate the access via any other method. 








