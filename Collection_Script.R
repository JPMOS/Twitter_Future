
```R
# R twitter APIs
library(rtweet)
#library(twitteR)
library(streamR)

# clean
library(dplyr)
library(tidytext)
library(tidyr)
library(VIM)

# visualize / sanity check
library(ggplot2)
library(ggmap)

#geocode
library(sp)
library(rgdal)
library(maps)
library(maptools)
library(USAboundaries)


######################################################################################################
#                   DATA RETRIEVAL FROM TWITTER API FOR ENGLISH & GEOLOCATED TWEETS
######################################################################################################

############################ AUTHENTICATION ##############################
# I think this may be browser based authentication, which won't work on OSCER.
twitter_token <- create_token( 
  app = "HappinessTimeTest",
  consumer_key = "TSnEtnWm6eftruxQoCCvcIDTs",
  consumer_secret = "cuNZEqNhA8m4IJ0SWfJikXFRQMbKPtpYTeyrEGSF3968VUZtMF"
)

api_key = 'TSnEtnWm6eftruxQoCCvcIDTs'
api_secret = 'cuNZEqNhA8m4IJ0SWfJikXFRQMbKPtpYTeyrEGSF3968VUZtMF'
access_token = '598079173-WAOpBdJVyvNVyGO6q5M3jekHyiwYZ1eWWJmZGQUA'
access_token_secret = 'NFWgRXpfElrKBZ4gOsxgTXSlpmW9SLyQFfTALSWywfTMC'

setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)
# and make sure the Callback URL equal : http://127.0.0.1:1410/ says internet.

setwd("~/Dropbox")
twitter_token <- readRDS("the_toke.rds")
########################### STREAM OR SEARCH #############################




big_tweets <- search_tweets( "lang:en", geocode = lookup_coords("usa"), n = 10000, 
                     include_rts = FALSE, token = twitter_token, retryonratelimit = TRUE )

unique(rt$lang)



tweets <- stream_tweets(
  "",
  # "a, the, i, you, u, and, is, in, it, of, for, on, my, that, at, with, me, do, have",
  # language = "en",# empty brackets pulls random sample...   COMPLETELY INEFECTIVE 
  #timeout = 60 * 60 * 24 * 7, Two weeks
  #lookup_coords("usa"), # streams from USA... may be lower 48 only. 
        # We may want to compare english speaking countries. 
  timeout = 60,
  #file_name = "tweetstest.json",
  parse = TRUE, # avoids the length process of converting JSON to R data type. 
  #token = twitter_token,
  verbose = FALSE,
  include_rts = FALSE
)

?stream_tweets
View(tweets)

saveRDS(big_tweets, "big_tweets.rds")
tweets <- readRDS("big_tweets.rds")


############################# SELECT & FILTER FOR PARAMETERS ################

large_tweets1 <- parse_stream("tweetstest2.json")## read in the data as a tidy tbl data frame
large_tweets2 <- parse_stream("tweetstest3.json")

#large_tweets <- parseTweets("/Users/jmcguire/Downloads/OU_Economics/Projects/tweetstest3.json")

large_tweets <- load("/Users/jmcguire/Downloads/OU_Economics/Projects/Global_environ_withTweets.RData")
big_tweets <- load("/Users/jmcguire/Downloads/OU_Economics/Projects/large_tweets1.RData")


unique(large_tweets$lang)

max(length())


head(tweets)

# Select only text, coordinates, country, and language.
tweets <- select(large_tweets, screen_name, text, country, lang, geo_coords, coords_coords, bbox_coords) %>% 
                lat_lng() %>% 
                filter(!is.na(lng)) %>%
                filter(lang == "en") %>% select(-geo_coords, -coords_coords, -bbox_coords)

tweets <- select(tweets, text)
tweets2 <- select(large_tweets1, text) 

tweets <- bind_rows(tweets, tweets2)

  lat_lng() %>% 
  filter(!is.na(lng)) %>%
  filter(lang == "en") %>% select(-geo_coords, -coords_coords, -bbox_coords)f

# keep working memory as trim as possible. 
remove(large_tweets)
remove(big_tweets)

tweets$lang
View(tweets)


######################################################################################################
# Geocoding detour ... come back to it later.
######################################################################################################

users <- lookup_users(tweets$screen_name) %>% # Needs to iterate 90k per 15 minutes to avoid limit. 
                transmute(users, location = ifelse(location == "", NA, location))


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

tweets <- tweets %>% mutate(state = latlong2state(coords)) %>% filter(!is.na(state))
states <- latlong2state(coords)

countNA(states) # You can select other countries later. 


# locations <- geocode(users$location, output = "latlon", source = "dsk") # limit error even when not google. 
                      # Check back and see if issue is resolved. 

########################################## VISUALIZE ########################

worldMap <- map_data("world")  # Easiest way to grab a world map shapefile

zp1 <- ggplot(worldMap) +
           geom_path(aes(x = long, y = lat, group = group),  # Draw map
                       colour = gray(2/3), lwd = 1/3) +
           geom_point(data = tweets,  # Add points indicating users
                        aes(x = lng, y = lat),
                        colour = "RED", alpha = 1/2, size = 1) +
           coord_equal() +  # Better projections are left for a future post
           theme_minimal()  # Drop background annotations
print(zp1)


######################################################################################################
# Let's get to cleaning! 
######################################################################################################
#tweets %>%
##   mutate_at(c("stripped_text"), gsub("http.*","",.))


cleaned <- unnest_tokens(tweets, input = text, output = word) %>% 
            anti_join(stop_words)
stop_words

dim(cleaned)
head(cleaned)


######################################################################################################
# Future Orientation Analysis
######################################################################################################

####################################################
#  LOAD LIWC TIME LEXICON 
###################################################
# first need to extract information from pdf.
Serial_number_LIWC <- "LIWC2015-Q6YK-6BT5-WRSD-M1LF-N5L9"

library(readr)
setwd("~/Dropbox")
LIWC2015_Future_Cleaned <- read_csv("~/Downloads/LIWC2015_Future_Cleaned.csv")

head(LIWC2015_Future_Cleaned)

TimeLexicon <- gather(LIWC2015_Future_Cleaned, key = "focus", value = "word")

head(TimeLexicon)
unique(TimeLexicon$focus)

####################################################
#  LIWC TIME LEXICON 
###################################################

nrow(filter(TimeLexicon, focus == "FocusPast" & !is.na(word) | focus == "FocusFuture" & !is.na(word)))
countNA(TimeLexicon)

cleaned %>%
  inner_join(filter(TimeLexicon, focus == "FocusFuture" | focus == "FocusPast")) %>%
  group_by(state) %>%
  count(word, sort = TRUE) %>%
  #summarise(sentiment = )

focus_index <- cleaned %>%
                  inner_join(filter(TimeLexicon, focus != "FocusPresent")) %>%
                  group_by(state, focus) %>%
                  count(word, sort = TRUE) %>%
                  spread(focus, n, fill = 0) %>%
                  mutate(orientation = FocusFuture - FocusPast) %>% 
                  summarize(index = sum(orientation)) %>% 
                  arrange(desc(index)) 

futuresentiment <- tweets %>%
  inner_join(TimeLexicon) %>%
  count(book, index = linenumber %/% 80, Time) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)



frequency <- cleaned %>%
                inner_join(filter(TimeLexicon, focus == "FocusFuture" | focus == "FocusPast")) %>%
                group_by(word) %>%
                count(word, sort = TRUE) 

words_to_filter_by <- frequency$word

nrow(frequency)
View(frequency)

low_frequency <- filter(TimeLexicon, focus != "FocusPresent" & !is.na(word)) %>% anti_join(frequency)
low_frequency
nrow(low_frequency) + nrow(frequency)

######################################################################################################
#                            MAPPING & VISUALIZATIONS
######################################################################################################

choro <- left_join(
              map_data("state"), 
              USArrests %>% 
              add_rownames("region") %>% 
              mutate(region=tolower(region))
)

ggplot(choro, aes(long, lat)) +
  geom_polygon(aes(group = group, fill = Assault)) + 
  coord_quickmap()

library(fiftystater)

data("fifty_states")


p <- ggplot(focus_index, aes(map_id = state)) + 
  # map points to the fifty_states shape data
  geom_map(aes(fill = index), map = fifty_states) + 
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_x_continuous(breaks = NULL) + 
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "") +
  theme(legend.position = "bottom", 
        panel.background = element_blank())
print(p)



```
