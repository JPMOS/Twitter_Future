# Twitter_Future
## Future Orientation Tweets Replication Guide.

#### Data

We used the package rtweet. Unfortunately, we can only provide the ID of the tweets due to the [twitter developer policy] (https://gwu-libraries.github.io/sfm-ui/posts/2017-09-14-twitter-data), not the tweets themselves. Unfortunately we deleted these on accident. Future collection will include IDs and thus a corresponding dataframe of IDs will be available as soon as possible. 

- [ ] Change code to collect IDs. 

If one desires to simply replicate the process of tweet collection, we may have some tools to help you. 

1. Get a Twitter Developer Account to Access API

When scraping data from Twitter, there are two things that need to be done. First is to set up a Twitter account and second is to request a Twitter token to get access to the API through this [link](https://developer.twitter.com/en/docs/basics/authentication/guides/access-tokens). If you already have a twitter account, all you need to do is to request access. There are plenty of tutorials out there to describe how this is done. A request is simple and Twitter responds very quickly. Simply fill out what your purpose is for the token and the turn around time is very quick. 

2. Choose an API. 

The two most popular twitter APIs are [rtweet](http://rtweet.info) for R and [tweepy](http://www.tweepy.org) for python. Tweepy is much more flexible, and is currently going to be your best best if you have a premium account or have access to tweets outside of the normal time restrictions of the past 7 days. If you're simply wanting the friendliest way, I suggest rtweet which is what we've used so far for this project. 

3. Save your twitter token. 

This chunk is borrowed from [Michael W. Kearney] (http://rtweet.info/articles/auth.html) 
``` R
## whatever name you assigned to your created app
appname <- "rtweet_token"

## api key (example below is not a real key)
key <- "XYznzPFOFZR2a39FwWKN1Jp41"

## api secret (example below is not a real key)
secret <- "CtkGEWmSevZqJuKl6HHrBxbCybxI1xGLqrD5ynPd9jG0SoHZbD"

## create token named "twitter_token"
twitter_token <- create_token(
  app = appname,
  consumer_key = key,
  consumer_secret = secret)
```
But I feel like his reccomendation for saving and loading it may be a tad confusing, so I suggest this method. 

``` R
setwd("/Users/jmcguire/Downloads/OU_Economics/Projects/TWEETSTORE") # Set your working directory
saveRDS(twitter_token, "the_token.rds") # It'll save to your working directory, which if you're in...
twitter_token <- readRDS("the_token.rds") # This will work perfectly fine. 
```
4. Get collecting! 

Now there's nothing stopping you from scraping the web, the rest is just adjusting parameters to suit the purpose of your search. Assuming you want a lot of tweets ( more than a million), you want to edit the function I made for your own purpose.

``` R
################
#    COLLECT
################

tweet_collection <- function(how_many_times) {
  j <- readRDS("count.Rds")
  
  for (i in seq_along(1:how_many_times)) {
    # idea we have j = 0
    tweets <- search_tweets( "lang:en", geocode = lookup_coords("usa"), n = 100, # Number can be edited.  
                             include_rts = FALSE, token = twitter_token, retryonratelimit = TRUE )
    
    tweets <- dplyr::select(tweets, screen_name, text, country, lang, geo_coords, coords_coords, bbox_coords) %>% 
      lat_lng() %>% # This tidies up the three different mercurial variables involving coords into two, just lat and lng.
      filter(!is.na(lng)) %>%
      dplyr::select(-geo_coords, -coords_coords, -bbox_coords) # %>% filter(lang == "en") 
    
    k <- ifelse(i == j, i, j )
    saveRDS(tweets, paste("test", k, ".rds", sep = ""))
    j <- j + 1 #... then save j, and have it called at the next and then for name have it be j + i. 
    # if i == j then do nothing but if i < j then add i + j
    saveRDS(j, "count.Rds")
  }
}

tweet_collection(2)
tweet_collection(2)
```

This function will automatically write each iteration to a file. This avoids memory constraints as all R objects must live in the RAM. It's not a great idea to try and collect all your tweets in one batch for this reason, it's better to break up the collection and concantenate them back together to do your analysis. You're much less likely to run into problems with 

#### Brief aside concerning 


To read the potentially hundreds of files into a single dataframe. 

```r 
#########################################
#  READ AND CONCATENATE COLLECTED TWEETS
#########################################

read_tweets <- function(from_N, to_N) { # could I also say which... and say 2:8. Would that work? 
  tweetlist = list()
  setwd("/Users/jmcguire/Downloads/OU_Economics/Projects/TWEETSTORE")
  
  for (i in seq_along(from_N:to_N)) {
    tweets <- read_rds(paste("test", i, ".rds", sep = ""))
    
    tweets$i <- i #  keep track of which iteration produced it
    tweetlist[[i]] <- tweets
  }
  
  big_tweets <- dplyr::bind_rows(tweetlist)
  return(big_tweets)
}
```

After that to automate the process we accessed a remote server to avoid having to run the code on our personal machines. 

The R scipt and the Twitter Token were then exported to the remote server. The R script will call the Twitter token for authentification. 

Using the Rbatch command to run the script, a command called (nohup.out) will continue to run the script after closing the server session. It is important to remember to close the access to the remote access server with (end) as there could be dissruptions if you terminate the access via any other method. 

From there once the data has been gathered you can easily transferred back into your own personal machine either via command line controls or a file transfer system. 

Then each section of the tweets will be concatenated together for ease of analysis. 








