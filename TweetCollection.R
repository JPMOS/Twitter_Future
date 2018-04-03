library(readr)
library(rtweet)
library(dplyr)
library(tidytext)
library(tidyr)

setwd("/Users/jmcguire/Downloads/OU_Economics/Projects/")
twitter_token <- readRDS("twitter_token.rds")


####################
setwd("/Users/jmcguire/Downloads/OU_Economics/Projects/")


###########################################################################
#                        A U T O M A T I O N 
###########################################################################

#j <- 0
#saveRDS(j, "count.Rds")

################
#    COLLECT
################

tweet_collection <- function(how_many_times) {
  j <- readRDS("count.Rds")

  for (i in seq_along(1:how_many_times)) {
 # idea we have j = 0
  tweets <- search_tweets( "lang:en", geocode = lookup_coords("usa"), n = 100, 
                                          include_rts = FALSE, token = twitter_token, retryonratelimit = TRUE )
  
  tweets <- dplyr::select(tweets, screen_name, text, country, lang, geo_coords, coords_coords, bbox_coords) %>% 
    lat_lng() %>% 
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

################
#  CONCATENATE
################

read_tweets <- function(from_N, to_N) { # could I also say which... and say 2:8. Would that work? 
  tweetlist = list()
  setwd("/Users/jmcguire/Downloads/OU_Economics/Projects/")
  
  for (i in seq_along(from_N:to_N)) {
    tweets <- read_rds(paste("big_tweets", i, ".rds", sep = ""))
    
    tweets$i <- i #  keep track of which iteration produced it
    tweetlist[[i]] <- tweets
  }
  
  big_tweets <- dplyr::bind_rows(tweetlist)
  return(big_tweets)
}

tweets_reloaded <- read_tweets(from_N = 1, to_N = 7)










