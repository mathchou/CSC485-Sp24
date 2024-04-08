#Assuming working directory is correct
library(tidyverse)
library(nflfastR)

#Loading data so that I have specific seasons
pbp99 <- load_pbp(1999)
pbp00 <- load_pbp(2000)
pbp01 <- load_pbp(2001)
pbp02 <- load_pbp(2002)
pbp03 <- load_pbp(2003)
pbp04 <- load_pbp(2004)
pbp05 <- load_pbp(2005)
pbp06 <- load_pbp(2006)
pbp07 <- load_pbp(2007)
pbp08 <- load_pbp(2008)
pbp09 <- load_pbp(2009)
pbp10 <- load_pbp(2010)
pbp11 <- load_pbp(2011)
pbp12 <- load_pbp(2012)
pbp13 <- load_pbp(2013)
pbp14 <- load_pbp(2014)
pbp15 <- load_pbp(2015)
pbp16 <- load_pbp(2016)
pbp17 <- load_pbp(2017)
pbp18 <- load_pbp(2018)
pbp19 <- load_pbp(2019)
pbp20 <- load_pbp(2020)
pbp21 <- load_pbp(2021)
pbp22 <- load_pbp(2022)
pbp23 <- load_pbp(2023)

pbp <- rbind(pbp99, pbp00, pbp01, pbp02, pbp03,
             pbp04, pbp05, pbp06, pbp07, pbp08,
             pbp09, pbp10, pbp11, pbp12, pbp13,
             pbp14, pbp15, pbp16, pbp17, pbp18,
             pbp19, pbp20, pbp21, pbp22, pbp23)
#Over 1 million total plays!


## ANALYSIS OF A SPECIFIC YEAR

#Cleaning out some NAs
for (i in 1:nrow(pbp23)) {
  if (is.na(pbp23$field_goal_result[[i]])) {
    pbp23$field_goal_result[[i]] <- "NA"
  }
  if (is.na(pbp23$extra_point_result[[i]])) {
    pbp23$extra_point_result[[i]] <- "NA"
  }
  if (is.na(pbp23$two_point_conv_result[[i]])) {
    pbp23$two_point_conv_result[[i]] <- "NA"
  }
  if (is.na(pbp23$safety[[i]])) {
    pbp23$safety[[i]] <- 0
  }
  if (is.na(pbp23$touchdown[[i]])) {
    pbp23$touchdown[[i]] <- 0
  }
}

#Adding points scored on play
pbp23 %>% mutate(play_pts = 0) -> pbp23

for (i in 1:nrow(pbp23)) {
  if (pbp23$field_goal_result[[i]] == "made") {
    pbp23$play_pts[[i]] <- 3
  }
  if (pbp23$extra_point_result[[i]] == "good") {
    pbp23$play_pts[[i]] <- 1
  }
  if (pbp23$two_point_conv_result[[i]] == "success") {
    pbp23$play_pts[[i]] <- 2
  }
  if (pbp23$safety[[i]] == 1) {
    pbp23$play_pts[[i]] <- 2
  }
  if (pbp23$touchdown[[i]] == 1) {
    pbp23$play_pts[[i]] <- 6
  }
}

#Removing kickoffs and timeouts from play-by-play data
pbp23 %>%
  filter(play_type != "kickoff") %>%
  filter(!is.na(ydsnet)) -> pbp23_1

#Finding out starting and ending field position for each drive
pbp23_1 %>%
  group_by(game_id, drive) %>%
  dplyr::summarize(starting_field = first(yardline_100),
                   total_yds = first(ydsnet),
                   drive_pts = sum(play_pts)) -> drive_data

#Summarizing drive data for each starting field position
drive_data %>%
  group_by(starting_field) %>%
  dplyr::summarize(avg_yds = mean(total_yds),
                   avg_pts = mean(drive_pts),
                   count = n()) -> field_pos

#Removing the NA drives (not sure why they're NAs but it's only a small sample)
field_pos %>% filter(!is.na(starting_field)) -> field_pos


##Initial visualizations for the 2023 season

#Yds per drive for each field position
ggplot(field_pos, aes(starting_field, avg_yds)) + geom_point() +
  labs(x = "Starting Field Position", y = "Avg Yards per Drive") #+ geom_abline(slope = 1, intercept = 0)
cor(field_pos$starting_field, field_pos$avg_yds)
#0.9093092 (strong, as expected)

#Pts per drive for each field position
ggplot(field_pos, aes(starting_field, avg_pts)) + geom_point() +
  labs(x = "Starting Field Position", y = "Avg Yards per Drive")
cor(field_pos$starting_field, field_pos$avg_pts)
#-0.8324272 (negatively strong, but not as strong as yards)





## ANALYSIS OF PAST 25 SEASONS

#Cleaning out some NAs
for (i in 1:nrow(pbp)) {
  if (is.na(pbp$field_goal_result[[i]])) {
    pbp$field_goal_result[[i]] <- "NA"
  }
  if (is.na(pbp$extra_point_result[[i]])) {
    pbp$extra_point_result[[i]] <- "NA"
  }
  if (is.na(pbp$two_point_conv_result[[i]])) {
    pbp$two_point_conv_result[[i]] <- "NA"
  }
  if (is.na(pbp$safety[[i]])) {
    pbp$safety[[i]] <- 0
  }
  if (is.na(pbp$touchdown[[i]])) {
    pbp$touchdown[[i]] <- 0
  }
}

#Adding points scored on play
pbp %>% mutate(play_pts = 0) -> pbp

for (i in 1:nrow(pbp)) {
  if (pbp$field_goal_result[[i]] == "made") {
    pbp$play_pts[[i]] <- 3
  }
  if (pbp$extra_point_result[[i]] == "good") {
    pbp$play_pts[[i]] <- 1
  }
  if (pbp$two_point_conv_result[[i]] == "success") {
    pbp$play_pts[[i]] <- 2
  }
  if (pbp$safety[[i]] == 1) {
    pbp$play_pts[[i]] <- 2
  }
  if (pbp$touchdown[[i]] == 1) {
    pbp$play_pts[[i]] <- 6
  }
}

#Removing kickoffs and timeouts from play-by-play data
pbp %>%
  filter(play_type != "kickoff") %>%
  filter(!is.na(ydsnet)) -> pbp_1

#Finding out starting and ending field position for each drive
pbp_1 %>%
  group_by(game_id, drive) %>%
  dplyr::summarize(starting_field = first(yardline_100),
                   total_yds = first(ydsnet),
                   drive_pts = sum(play_pts)) -> drive_data

#Summarizing drive data for each starting field position
drive_data %>%
  group_by(starting_field) %>%
  dplyr::summarize(avg_yds = mean(total_yds),
                   avg_pts = mean(drive_pts),
                   count = n()) -> field_pos

#Removing the NA drives (not sure why they're NAs but it's only a small sample)
field_pos %>% filter(!is.na(starting_field)) -> field_pos


##Initial visualizations for past 25 seasons

#Yds per drive for each field position
ggplot(field_pos, aes(starting_field, avg_yds)) + geom_point() +
  labs(x = "Starting Field Position", y = "Avg Yards per Drive") #+ geom_abline(slope = 1, intercept = 0)
cor(field_pos$starting_field, field_pos$avg_yds)
#0.9093092 (strong, as expected)

#Pts per drive for each field position
ggplot(field_pos, aes(starting_field, avg_pts)) + geom_point() +
  labs(x = "Starting Field Position", y = "Avg Yards per Drive")
cor(field_pos$starting_field, field_pos$avg_pts)
#-0.8324272 (negatively strong, but not as strong as yards)



## ISOLATING PUNT PLAYS FOR 2023 SEASON
pbp23 %>% filter(play_type == "punt" & punt_blocked == 0) -> pbp23_punts

#Figuring out stuff (coding notes that I don't want to keep here)
game_ex <- pbp00 %>% filter(game_id == "2000_01_ARI_NYG")
field_descriptions -> desc

ggplot(pbp23_punts, aes(kick_distance)) + geom_histogram()
#kick distance does not incorporate return yardage!!!