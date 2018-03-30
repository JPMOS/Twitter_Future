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

  tweets <- stream_tweets(
    "hope, gonna, tonight, coming, tomorrow, wanna, left, remember, found, lost, gotta, ago, added, told, called, heard, started, past, plan, happened, posted, played, missed, paid, won, voted, bought, forgot, broke, woke, ahead, met, changed, supposed, expect, sold, died, looked, plans, passed, meant, fucked, learned, spent, brought, pray, finished, created, stopped, born, hoping, caught, decided, led, sat, planning, stuck, lit, ima, written, helped, wrote, held, earned, potential, ran, named, earlier, realized, imma, fell, moved, joined, picked, grown, upcoming, ate, shared, gon, didnt, taught, entered, lived, talked, walked, praying, noticed, fed, finna, arrived, prayer, slept, eventually, grew, listened, included, wishing, spoke, lied, cried, funded, wont, hated, threw, previous, stole, wishes, deleted, stayed, supported, kicked, tested, accepted, shook, stood, completed, provided, organized, fought, hired, ranked, prior, remembered, informed, affected, laid, thrown, believed, explained, would've, understood, fate, someday, wore, drove, sooner, waited, fallen, cared, admitted, donated, jumped, attended, trusted, remembering, washed, visited, gunna, sang, traveled, denied, sucked, drank, carried, hopeful, solved, should've, prospect, pressed, wondered, remembers, flew, promising, eaten, predict, approaching, suffered, worn, cleaned, appeared, yelled, could've, founded, texted, fooled, that'll, wished, impending, studied, prayed, hoped, disappeared, planner, attracted, sought, viewed, typed, expired, guessed, searched, woken, emailed, questioned, rode, obtained, contacted, healed, harmed, anticipation, approached, lowered, mastered, danced, tricked, stumbled, slowed, youll, anticipate, weighed, googled, messaged, imminent, practiced, tasted, clapped, flown, rubbed, invaded, shoved, bounced, begged, slain, slid, sung, looming, stocked, mocked, eventual, fled, forthcoming, owed, learnt, sunk, thanked, onward, protested, tripped, wouldve, disliked, lacked, must've, spat, youve, warmed, remembrance, taxed, spun, werent, perfected, sped, swung, destin, departed, i'mma, shouldve, feasible, depended, where'd, dared, itll, might've, who'll, foresee, weakened, couldve, wept, what'd, foresight, stirred, fates, mapped, overcame, weve, braved, descended, forwarded, theyve, tumbled, sank, sobbed, grossed, oncoming, sighed, complied, attainable, overdosed, theyll, unfriended, worsen, graced, lucked, narrowed, swam, fixin, stunk, excelled, headin, costed, flowed, futur, tended, craved, thatll, weirded, hadnt, flirted, heeded, lapsed, henceforth, howd, obeyed, obtainable, swerved, trotted, what'll, overeat, soonest, undid, dined, fated, frequented, okayed, prepar, recollect, revolved, sensed, loosed, mated, mustve, destructed, pitied, skied, surfed, wobbled, differed, i'd've, neared, twitched, wagged, whatd, confided, foreshadow, forseeable, shant, thinned, wagered, warred, yester, already, asked, became, been, began, did, didn't, done, ended, felt, followed, former, gave, given, gone, got, gotten, had, hadn't, ignored, kept, knew, known, liked, made, needed, said, saw, seemed, seen, sent, showed, taken, took, tried, turned, used, wanted, was, went, were, worked, fixin, going, hopefully, sometime, soon, then, wants, will, wish, mightve",
    # language = "en",# empty brackets pulls random sample...   COMPLETELY INEFECTIVE 
    #timeout = 60 * 60 * 24 * 7, Two weeks
    #lookup_coords("usa"), # streams from USA... may be lower 48 only. 
    # We may want to compare english speaking countries. 
    timeout = 60 * 60 * 2,
    #file_name = "tweetstest.json",
    parse = TRUE, # avoids the length process of converting JSON to R data type. 
    #token = twitter_token,
    verbose = FALSE,
    include_rts = FALSE
  )
  
  tweets <- select(tweets, screen_name, text, country, lang, geo_coords, coords_coords, bbox_coords) %>% 
    lat_lng() %>% 
    filter(!is.na(lng)) %>%
    select(-geo_coords, -coords_coords, -bbox_coords) # %>% filter(lang == "en") 
  
  saveRDS(tweets, paste("testtweets", i, ".rds", sep = ""))
  }
  
}

tweet_collection(2)



read_tweets <- function(from_N, to_N) { # could I also say which... and say 2:8. Would that work? 
  tweetlist = list()
  setwd("/Users/jmcguire/Downloads/OU_Economics/Projects/")
  
  for (i in seq_along(from_N:to_N)) {
    tweets <- read_rds(paste("testtweets", i, ".rds", sep = ""))
    tweets$i <- i #  keep track of which iteration produced it
    tweetlist[[i]] <- tweets
  }
  
  big_tweets <- dplyr::bind_rows(tweetlist)
  return(big_tweets)
}
tweets_reloaded <- read_tweets(from_N = 2, to_N = 3)






