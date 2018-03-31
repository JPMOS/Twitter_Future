library(readr)
library(rtweet)
library(dplyr)
library(tidytext)
library(tidyr)

setwd("/Users/jmcguire/Downloads/OU_Economics/Projects/")
twitter_token <- readRDS("twitter_token.rds")


big_tweets7 <- search_tweets( "lang:en", geocode = lookup_coords("usa"), n = 250000, 
                             include_rts = FALSE, token = twitter_token, retryonratelimit = TRUE )

tweets1 <- select(tweets1, text)
tweets2 <- select(tweets2, text)
tweets3 <- select(tweets3, text)
tweets4 <- select(tweets4, text)
tweets5 <- select(tweets5, text)
tweets6 <- select(tweets6, text)


tweets6 <- select(big_tweets6, text)
tweets7 <- select(big_tweets7, text)

tweets <- bind_rows(tweets1, tweets2, tweets3, tweets4, tweets5, tweets6, tweets7)
nrow(tweets)
head(tweets)

big_tweets5 <- large_tweets1

saveRDS(tweets, "2miltweetstext.rds")


tweets <- `2miltweetstext`
tweets1 <- select(tweets1, text)


####################
setwd("/Users/jmcguire/Downloads/OU_Economics/Projects/")
LIWC2015_Future_Cleaned <- read_csv("LIWC2015_Future_Cleaned.csv")

head(LIWC2015_Future_Cleaned)

TimeLexicon <- gather(LIWC2015_Future_Cleaned, key = "focus", value = "word")

######################

cleaned <- unnest_tokens(tweets, input = text, output = word) %>% 
  anti_join(stop_words)

frequency <- cleaned %>%
  inner_join(filter(TimeLexicon, focus == "FocusFuture" | focus == "FocusPast")) %>%
  group_by(word) %>%
  count(word, sort = TRUE) 

TimeLexicon %>% na.omit() %>% filter(focus == "FocusFuture" | focus == "FocusPast")
head(TimeLexicon)

words_to_filter_by <- frequency$word

nrow(na.omit(TimeLexicon))
omitNA
nrow(frequency)


low_frequency <- filter(TimeLexicon, focus != "FocusPresent" & !is.na(word)) %>% anti_join(frequency)

View(low_frequency) # Choose what 36 not to add. 

nrow(low_frequency) + nrow(frequency)

View(frequency)

###########################################################################
#                        A U T O M A T I O N 
###########################################################################






tweet_collection <- function(how_many_times) {
  
for (i in seq_along(1:how_many_times)) {
 # idea we have j = 0
  tweets <- search_tweets( "lang:en", geocode = lookup_coords("usa"), n = 250000, 
                                          include_rts = FALSE, token = twitter_token, retryonratelimit = TRUE )
  
  tweets <- select(tweets, screen_name, text, country, lang, geo_coords, coords_coords, bbox_coords) %>% 
    lat_lng() %>% 
    filter(!is.na(lng)) %>%
    select(-geo_coords, -coords_coords, -bbox_coords) # %>% filter(lang == "en") 
  # j += 1 ... then save j, and have it called at the next and then for name have it be j + i. 
  saveRDS(tweets, paste("test", i + 40, ".rds", sep = ""))
  }
}

tweet_collection(5)
.


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

saveRDS(tweets_reloaded, "geo_tweets1.rds")
geo_tweets <- readRDS("geo_tweets1.rds")













