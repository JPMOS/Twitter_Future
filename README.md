# Twitter_Future
## Future Orientation Tweets Replication Guide.

#### Packages Needed
``` r
# R twitter APIs
library(rtweet)
#library(twitteR)
#library(streamR)
library(readr)

# clean
library(dplyr)
library(tidytext)
library(tidyr)
#library(VIM)

# visualize / sanity check
library(ggplot2)
library(ggmap)

#geocode
library(sp)
library(rgdal)
library(maps)
library(maptools)
library(USAboundaries)
```


# Data

We used the package rtweet. Unfortunately, we can only provide the ID of the tweets due to the [twitter developer policy](https://gwu-libraries.github.io/sfm-ui/posts/2017-09-14-twitter-data), not the tweets themselves. Unfortunately we deleted these on accident. Future collection will include IDs and thus a corresponding dataframe of IDs will be available as soon as possible. 

- [ ] Change code to collect IDs. 

If one desires to simply replicate the process of tweet collection, we may have some tools to help you. 

1. Get a Twitter Developer Account to Access API

When scraping data from Twitter, there are two things that need to be done. First is to set up a Twitter account and second is to request a Twitter token to get access to the API through this [link](https://developer.twitter.com/en/docs/basics/authentication/guides/access-tokens). If you already have a twitter account, all you need to do is to request access. There are plenty of tutorials out there to describe how this is done. A request is simple and Twitter responds very quickly. Simply fill out what your purpose is for the token and the turn around time is very quick. 

2. Choose an API. 

The two most popular twitter APIs are [rtweet](http://rtweet.info) for R and [tweepy](http://www.tweepy.org) for python. Tweepy is much more flexible, and is currently going to be your best best if you have a premium account or have access to tweets outside of the normal time restrictions of the past 7 days. If you're simply wanting the friendliest way, I suggest rtweet which is what we've used so far for this project. 

3. Save your twitter token. 

This chunk is borrowed from [Michael W. Kearney](http://rtweet.info/articles/auth.html) 
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
If his reccomendation for saving and loading is a tad confusing to those who are still wondering what a filepath is, I suggest this method. 

``` R
setwd("/Users/jmcguire/Downloads/OU_Economics/Projects/TWEETSTORE") # Set your working directory
          # You can find out the filepath to your desired wd by dragging to file or folder into your terminal screen on a mac. 
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

```

This function will automatically write each iteration to a file. This avoids memory constraints as all R objects must live in the RAM. It's not a great idea to try and collect all your tweets in one batch for this reason, it's better to break up the collection and concantenate them back together to do your analysis. You're much less likely to run into problems by cutting it into chunks, it'll also allow you to do simple parrelization if you need to. 

It's important to note there's an alternative to `search_tweets` which is `stream_tweets`. If you're simply wanting to collect as many tweets as possible, streaming is going to sample 1% of all tweets for you. We chose to use the search function for two reasons. 
1. Using stream you can't filter for a language except indirectly by filtering for up to 400 keywords.
We considered filtering by the words in the dictionaries we were going to use for sentiment analysis (429 words), only 362 of which showed up in a sample of 1 million tweets we collected to find out the frequency of the words in the LIWC time lexicon. (To access the LIWC dictionary you need a paid account, the monthly rate is cheap). Why we didn't use this method ... 
2. The json format. 
When streaming tweets you have the option to set `parse = FALSE`. This will automatically save your tweets to a JSON file of whatever you set the name to with the option of `file_name = big_tweets.json`. Doing so may suit your needs as a researcher but the parsing of the json may take a non trivial amount of time if the json is any larger than 1 GB. Using `search_tweets` doesn't get around parsing the tweets but it parses the tweets while you wait the 15 minutes in between collections, so in my experience I've found it more efficient as tweet collection given I'm filtering for a language (english).

#### Brief aside concerning running on a remote server. 
To run this script continuously, in the background, without interruption, our best method is to run the whole script as below.
``` linux
nohup Rscript tweetcollection.R & 
```
The ampersand does something important, but I don't remember quite what at the moment. You need to have it. It's also imporant to disconnect from your server by typing "exit", It's a quirk of the nohup command. There are many other ways, some of them more robust. This is addmittedly the quick and dirty way. 




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

tweets_reloaded <- read_tweets(from_N = 102, to_N = 123) # Call function

saveRDS(tweets_reloaded, "Tweets102_123.rds") # You may want to save your new df as an rds, which loads much more quickly. 

tweets <- readRDS("WhateverIsavedmytweetsas.rds")
```

# NLP

#### Geocoding
Since we're looking at the future orientation of each state, we need to take the latitude and longitude coordinates and turn them into states. As of this moment we don't have access to encoding Alaska and Hawaii. 

``` r
coords <- select(tweets, lng, lat)

latlong2state <- function(pointsDF) {
  # Prepare SpatialPolygons object with one SpatialPolygon
  # per state (plus DC, minus HI & AK)
  states <- map('state', fill=TRUE, col="transparent", plot=FALSE)
  IDs <- sapply(strsplit(states$names, ":"), function(x) x[1])
  states_sp <- map2SpatialPolygons(states, IDs=IDs,
                                   proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Convert pointsDF to a SpatialPoints object 
  pointsSP <- SpatialPoints(pointsDF, 
                            proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Use 'over' to get _indices_ of the Polygons object containing each point 
  indices <- over(pointsSP, states_sp)
  
  # Return the state names of the Polygons object containing each point
  stateNames <- sapply(states_sp@polygons, function(x) x@ID)
  stateNames[indices]
}

tweets <- tweets %>% mutate(state = latlong2state(coords)) %>% filter(!is.na(state)) # Now we have a state column!
```

#### Tokenization. 
To analyze a corpus of words, they need to be separated into units or tokenized. This is simple with the tidyverse tool [tidytext](https://cran.r-project.org/web/packages/tidytext/vignettes/tidytext.html).

``` r
cleaned <- unnest_tokens(tweets, input = text, output = word) %>% 
            anti_join(stop_words) # Note how it used dplyrs join functionality. It behaves similarly to SQL. 
```

## Not Future Oriented Index. 
#### It's a working title. 
``` r
####################################################
#  LOAD LIWC TIME LEXICON 
###################################################

setwd("/Users/jmcguire/Downloads/OU_Economics/Projects/")
LIWC2015_Future_Cleaned <- read_csv("Data/LIWC2015_Future_Cleaned.csv")

head(LIWC2015_Future_Cleaned)

TimeLexicon <- gather(LIWC2015_Future_Cleaned, key = "focus", value = "word")

####################################################
#  CREATE INDEX
###################################################

cleaned <- cleaned %>% group_by(state) %>% mutate(total_words = n())

#future_index <- cleaned %>%
#                  inner_join(filter(TimeLexicon, focus == "FocusFuture")) %>%
#                  group_by(state, focus, total_words) %>%
#                  count(word, sort = TRUE) %>%
#                  spread(focus, n, fill = 0) %>%
#                  mutate(future_orient = FocusFuture / total_words) %>%
#                  summarize(index = mean(future_orient)) %>%
#                  arrange(desc(state)) 
```

Above and below you'll see too different methods of constructing the index for future orientation. We're currently going with the one below as it captures all three ways frame of time to focus on that are clearly captured by grammar.  
``` r
unfuture_index <- cleaned %>%
  inner_join(filter(TimeLexicon)) %>%
  group_by(state, focus, total_words) %>%
  count(word, sort = TRUE) %>%
  spread(focus, n, fill = 0) %>%
  mutate(words_future_state = sum(FocusFuture),
         words_present_state = sum(FocusPresent),
         words_past_state = sum(FocusPast)) %>% 
  mutate(future_orient = ((( words_present_state - words_future_state) / total_words) * 100 ),
         future_orient2 = ((( words_past_state - words_future_state) / total_words) * 100 ),
         future_orient3 = ((( words_present_state + words_past_state - words_future_state) / total_words) * 100 )) %>%
  summarize(index = mean(future_orient),
            index2 = mean(future_orient2),
            index3 = mean(future_orient3)) %>%
  arrange(desc(state)) 
````

