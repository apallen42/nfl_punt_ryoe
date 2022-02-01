
library(dplyr)
library(stringr)
library(ggplot2)
library(purr)


#Read in PFF and play datasets
plays <- read.csv("~/Documents/Data Science - Side Projects/Big Data Bowl/plays.csv")
pff <- read.csv("~/Documents/Data Science - Side Projects/Big Data Bowl/PFFScoutingData.csv")
games <- read.csv("~/Documents/Data Science - Side Projects/Big Data Bowl/games.csv")


#Filtering for just punts; Remove punts where the return team committed a penalty
#Create unique game/play ID

plays_punts <- plays %>%
  filter(specialTeamsPlayType == "Punt",
         specialTeamsResult != "Out of Bounds") %>%
  mutate(penalty_team = substr(penaltyJerseyNumbers, 1, 3),
         penalty_receiving = case_when(penalty_team != possessionTeam ~ 1, #Was there a receiving team penalty?
                                       TRUE ~ 0),
         penalty_kicking = case_when(penalty_team == possessionTeam ~ 1, #Kicking team penalty?
                                     TRUE ~ 0),
         game_play_id = paste(gameId, playId, sep = "_")) %>%
  filter(penalty_receiving == 0) #Remove receiving team penalties

plays_punts$kickReturnYardage[is.na(plays_punts$kickReturnYardage)] <- 0 #Turn Fair Catches into 0?

#Join w/game data, create presnap margin var for the return team
plays_punts <- plays_punts %>%
  inner_join(games, by = "gameId") %>%
  mutate(return_team_home = case_when(possessionTeam != homeTeamAbbr ~ "home",
                                      TRUE ~ "away"),
         presnap_score_margin_returnteam = case_when(return_team_home == "home" ~ preSnapHomeScore - preSnapVisitorScore,
                                                     TRUE ~ preSnapVisitorScore - preSnapHomeScore)) 

#Create indicator for unintended kick/return directions, game_play_id in pff
pff <- pff %>%
  mutate(kick_direction_wrong = case_when(((kickDirectionIntended != kickDirectionActual) & 
                                             !is.na(kickDirectionActual)) ~ 1,
                                          T ~ 0),
         return_direction_wrong = case_when(((returnDirectionIntended != returnDirectionActual) & 
                                               !is.na(returnDirectionActual)) ~ 1,
                                            T ~ 0),
         game_play_id = paste(gameId, playId, sep = "_")) %>%
  select(-c(gameId, playId)) %>%
  filter(!is.na(hangTime))


#Join pff data to main data
colnames_plays <- colnames(plays_punts)
colnames_pff <- colnames(pff)
colnames_pff <- colnames_pff[1:length(colnames_pff)-1]

plays_punts <- plays_punts %>%
  inner_join(pff, by = "game_play_id")

colnames(plays_punts) <- c(colnames_plays, colnames_pff)

#Create variable for numberes of gunners, rushers, safeties, vises
plays_punts <- plays_punts %>%
  rowwise() %>%
  mutate(num_gunners = case_when(!is.na(gunners) ~ length(str_split(gunners, ";", simplify = T)),
                                 TRUE ~ NA_integer_),
         num_rushers = case_when(!is.na(puntRushers) ~ length(str_split(puntRushers, ";", simplify = T)),
                                 TRUE ~ NA_integer_),
         num_safeties = case_when(!is.na(specialTeamsSafeties) ~ length(str_split(specialTeamsSafeties, ";", simplify = T)),
                                  TRUE ~ NA_integer_),
         num_vises = case_when(!is.na(vises) ~ length(str_split(vises, ";", simplify = T)),
                               TRUE ~ NA_integer_)) 

#############
##Incorporating Tracking Data
############


possible_end_actions = c("punt_land", "punt_received",
                         "punt_muffed", "fair_catch", "touchback", "punt_downed")

tracking2018 <- read.csv("~/Documents/Data Science - Side Projects/Big Data Bowl/tracking2018.csv")
tracking2019 <- read.csv("~/Documents/Data Science - Side Projects/Big Data Bowl/tracking2019.csv")
tracking2020 <- read.csv("~/Documents/Data Science - Side Projects/Big Data Bowl/tracking2020.csv")

tracking2018_subset <- tracking2018 %>%
  filter(event == "punt"|event %in% possible_end_actions)

tracking2019_subset <- tracking2019 %>%
  filter(event == "punt"|event %in% possible_end_actions)

tracking2020_subset <- tracking2020 %>%
  filter(event == "punt"|event %in% possible_end_actions)

tracking_all <- tracking2018 %>%
  full_join(tracking2019) %>%
  full_join(tracking2020) %>%
  mutate(game_play_id = paste(gameId, playId, sep = "_"))

plays_punts$track <- plays_punts$game_play_id %in% tracking_all$game_play_id
plays_punts <- plays_punts %>%
  filter(track == T)
plays_punts$muff <- ifelse(plays_punts$specialTeamsResult == "Muffed",1,0)

#Filter tracking data for non out-of-bounds, penalty punts
tracking_all <- tracking_all %>%
  filter(game_play_id %in% plays_punts$game_play_id)


#Create a dataframe of the FIRST time point, to later get the returner ID 
tracking_subset_first <- tracking_all %>%
  arrange(game_play_id, time, displayName) %>%
  mutate(team == case_when(displayName == "football" ~ "None",
                           TRUE ~ team),
         keep = case_when(game_play_id != lag(game_play_id) ~ 1,
                          is.na(lag(game_play_id)) ~ 1,
                          TRUE ~ 0)) 



#Using indexing to get the first 23 (22 players + 1 ball) of each unique play
keep_indexes <- which(tracking_subset_first$keep == 1)
keep_indexes_updated <- lapply(keep_indexes, function(x) (x):(x+22))
tracking_subset_first <- tracking_subset_first[unlist(keep_indexes_updated),]


#Subset:Tracking Data for end result
tracking_subset_end <- tracking_all %>%
  filter(event %in% possible_end_actions) %>%
  arrange(game_play_id, displayName, time) %>%
  mutate(team == case_when(displayName == "football" ~ "None",
                           TRUE ~ team),
         duplicate = case_when((lag(game_play_id) == game_play_id) & 
                                 (lag(displayName) == displayName) ~ 1,
                               TRUE ~ 0),
         ToLeft = playDirection == "left",
         x_std = ifelse(ToLeft, 120-x, x) - 10, ## Standardizes X
         y_std = ifelse(ToLeft, 160/3-y, y)) %>% #Is the play to the left
  filter(duplicate == 0)  #Only have one end play per subset


#Finding the returner for each play

for(i in 1:length(tracking_subset_end$game_play_id)) {
  temp_df <- tracking_subset_first %>%
    filter(game_play_id == tracking_subset_end$game_play_id[i])
  temp_index <- which.max(abs(temp_df$x - temp_df$x[temp_df$team=="football"]))
  temp_return_team <- temp_df$team[temp_index]
  tracking_subset_end$returnerId[i] <- temp_df$nflId[temp_index]
}

tracking_subset_end$returner <- tracking_subset_end$nflId == tracking_subset_end$returnerId


