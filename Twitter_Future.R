
``` R
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


######################################################################################################
#                   DATA RETRIEVAL FROM TWITTER API FOR ENGLISH & GEOLOCATED TWEETS
######################################################################################################

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

tweets_reloaded <- read_tweets(from_N = 102, to_N = 123)


saveRDS(tweets_reloaded, "Tweets102_123.rds")

head(tweets_reloaded)
tweets <- readRDS("Tweets1_101.rds")
tweets2 <- readRDS("Tweets102_123.rds")
tweets3 <- readRDS("Donald_Tweets1_90.rds")

tweets <- bind_rows(tweets, tweets2, tweets3)


######################################################################################################
# Geocoding detour AND latlong2state
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


countNA(states) # You can select other countries later. 


# locations <- geocode(users$location, output = "latlon", source = "dsk") # limit error even when not google. 
                      # Check back and see if issue is resolved. 

#######################
#      World Map
#######################

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

cleaned <- unnest_tokens(tweets, input = text, output = word) %>% 
            anti_join(stop_words)

dim(cleaned)
head(tweets)

######################################################################################################
# Future Orientation Analysis
######################################################################################################

####################################################
#  LOAD LIWC TIME LEXICON 
###################################################
# first need to extract information from pdf.
# Serial_number_LIWC <- "LIWC2015-Q6YK-6BT5-WRSD-M1LF-N5L9"

setwd("/Users/jmcguire/Downloads/OU_Economics/Projects/")
LIWC2015_Future_Cleaned <- read_csv("Data/LIWC2015_Future_Cleaned.csv")

head(LIWC2015_Future_Cleaned)

TimeLexicon <- gather(LIWC2015_Future_Cleaned, key = "focus", value = "word")

head(TimeLexicon)
unique(TimeLexicon$focus)

####################################################
#  CREATE INDEX
###################################################
head(cleaned)

cleaned <- cleaned %>% group_by(state) %>% mutate(total_words = n())
head(cleaned)


future_index <- cleaned %>%
                  inner_join(filter(TimeLexicon, focus == "FocusFuture")) %>%
                  group_by(state, focus, total_words) %>%
                  count(word, sort = TRUE) %>%
                  spread(focus, n, fill = 0) %>%
                  mutate(future_orient = FocusFuture / total_words) %>%
                  summarize(index = mean(future_orient)) %>%
                  arrange(desc(state)) 

past_index <- cleaned %>%
  inner_join(filter(TimeLexicon, focus == "FocusPast")) %>%
  group_by(state, focus, total_words) %>%
  count(word, sort = TRUE) %>%
  spread(focus, n, fill = 0) %>%
  mutate(past_orient = FocusPast / total_words) %>%
  summarize(index = mean(past_orient)) %>%
  arrange(desc(state)) 


present_index <-  cleaned %>%
  inner_join(filter(TimeLexicon, focus == "FocusPresent")) %>%
  group_by(state, focus, total_words) %>%
  count(word, sort = TRUE) %>%
  spread(focus, n, fill = 0) %>%
  mutate(present_orient = FocusPresent / total_words) %>%
  summarize(index = mean(present_orient)) %>%
  arrange(desc(state)) 

plot(past_index$index, future_index$index) # A state with a higher future orientation is likely to score higher
# ON PAST INDEX AS WELL FSDFLSDJFSJDFLSKDJflSJGLSHLGJS
plot(present_index$index, past_index$index)
plot(present_index$index, future_index$index)

future_index
past_index
present_index

focus_index <- future_index
focus_index$past_index <- past_index$index
focus_index$present_index <- present_index$index

focus_index <- focus_index %>% mutate(ratio = past_index, index)
arrange(focus_index, desc(present_index))

saveRDS(focus_index, "Time_index.rds")

##########
# Other ways? 
#########

focus_index <- cleaned %>%
    inner_join(filter(TimeLexicon, focus == "FocusFuture")) %>%
    group_by(state, focus, total_words) %>%
    count(word, sort = TRUE) %>%
    spread(focus, n, fill = 0) %>%
    mutate(wordsByState = sum(FocusFuture)) %>% 
    group_by(state) %>% mutate(index = (wordsByState / total_words)) %>% 
            summarise(index = mean(index)) %>% 
            arrange(desc(mean))

focus_index$normalized <- as.vector(scale(focus_index$index))


#############
#  word F R E Q 
############

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
  geom_map(aes(fill = normalized), map = fifty_states) + 
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_x_continuous(breaks = NULL) + 
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "") +
  theme(legend.position = "bottom", 
        panel.background = element_blank(),
        axis.text.x=element_text(angle = -90, hjust = 0))
print(p)

#########################################
# Bring in State GDP Growth. 
#########################################

stateGDP <- read_csv("~/Downloads/OU_Economics/Projects/Data/state_gdp_growth.csv")
stateGDP <- mutate(stateGDP, Q3_2017 = as.double(Q3_2017),
                             Q2_2017 = as.double(Q2_2017),
                             Q1_2017 = as.double(Q1_2017),
                             state = tolower(state))

stateGDP <- stateGDP %>% filter(IndCode == 1)

joined <- inner_join(focus_index, stateGDP)

head(joined)
fit1 <- lm(Q3_2017 ~ index, data = joined)
summary(fit1) # Basically nothing. Nada. No effect. 


p2 <- ggplot(joined, aes(x = index, y = as.numeric(Q3_2017))) + geom_point()
p2 <- p2 + geom_smooth(aes(color = "red"))
print(p2)

#########################################
# Bring in State Test SAT Math Scores
#########################################

stateSAT <- read_csv("~/Downloads/OU_Economics/Projects/Data/SAT_AVG_2017.csv")
stateSAT <- mutate(stateSAT, state = tolower(State),
                   Participation = as.numeric(sub("%","",Participation))/100)

joined <- inner_join(focus_index, stateSAT)

head(joined)
as.numeric(stateSAT$Participation)


fit2 <- lm(Math ~ index ,  data = filter(joined, state != "delaware"))
summary(fit2)

focus_index
filter(joined, state != "delaware") %>% 
  ggplot(aes(x = ratio, y = as.numeric(Math))) + geom_point() +
    geom_smooth(aes(color = "red"))

#########################################
# Bring in State Test ACT Math Scores
#########################################

stateACT <- read_csv("~/Downloads/OU_Economics/Projects/Data/ACT_AVG_2017.csv")
stateACT <- mutate(stateACT, state = tolower(State),
                   Participation = as.numeric(sub("%","",Participation))/100)

joined2 <- inner_join(focus_index, stateACT)

head(joined2)

fit3 <- lm(Math ~ index + past_index + Participation , data = joined2)
summary(fit3)



focus_index
joined2 %>% 
  ggplot(aes(x = index, y = as.numeric(Math))) + geom_point() +
  geom_smooth(aes(color = "red"))


tests <- bind_cols(stateACT, stateSAT)

head(tests)

tests %>% 
  ggplot(aes(x = Math, y = Math1)) + geom_point() 


#########################################
# Bring in State Savings Search
#########################################

state_savesearch <- read_csv("~/Downloads/OU_Economics/Projects/Data/google_savings_search.csv")
state_medinc <- read_csv("~/Downloads/OU_Economics/Projects/Data/State_median_inc_2016.csv")
head(state_medinc)

state_savesearch <- mutate(state_savesearch, state = tolower(Region))
state_medinc <- mutate(state_medinc, state = tolower(State))

joined3 <- inner_join(focus_index, state_savesearch)
joined3 <- inner_join(joined3, state_medinc)


head(joined3)
dim(joined3)

fit4 <- lm(savings_account ~   past_index + index + Haus_med_inc , data = joined3)
summary(fit4)

joined3 %>% 
  ggplot(aes(x = present_index, y = savings_account)) + geom_point() +
  geom_smooth(aes(color = "red"), method ="lm")




```
