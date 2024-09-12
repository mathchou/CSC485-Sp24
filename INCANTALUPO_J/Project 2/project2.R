#Assuming working directory is correct
library(tidyverse)
library(nflfastR)
library(GGally)
library(corrplot)

#Loading play-by-play data
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

# write.csv(pbp, "pbp.csv", row.names = FALSE)


#Adding points scored on play
pbp %>% mutate(play_pts = 0) -> pbp

for (i in 1:nrow(pbp)) {
  if (pbp$field_goal_result[[i]] == "made" & !is.na(pbp$field_goal_result[[i]])) {
    pbp$play_pts[[i]] <- 3
  }
  if (pbp$extra_point_result[[i]] == "good" & !is.na(pbp$extra_point_result[[i]])) {
    pbp$play_pts[[i]] <- 1
  }
  if (pbp$two_point_conv_result[[i]] == "success" & !is.na(pbp$two_point_conv_result[[i]])) {
    pbp$play_pts[[i]] <- 2
  }
  if (pbp$safety[[i]] == 1 & !is.na(pbp$safety[[i]])) {
    pbp$play_pts[[i]] <- 2
  }
  if (pbp$touchdown[[i]] == 1 & !is.na(pbp$touchdown[[i]])) {
    pbp$play_pts[[i]] <- 6
  }
} #Shortened runtime to 6 minutes!!!

write.csv(pbp, "pbp.csv", row.names = FALSE)

pbp <- read.csv("pbp.csv") #3.5 minute runtime
#Now including play pts


## ANALYSIS OF A SPECIFIC YEAR

#Adding points scored on play
pbp23 %>% mutate(play_pts = 0) -> pbp23

for (i in 1:nrow(pbp23)) {
  if (pbp23$field_goal_result[[i]] == "made" & !is.na(pbp23$field_goal_result[[i]])) {
    pbp23$play_pts[[i]] <- 3
  }
  if (pbp23$extra_point_result[[i]] == "good" & !is.na(pbp23$extra_point_result[[i]])) {
    pbp23$play_pts[[i]] <- 1
  }
  if (pbp23$two_point_conv_result[[i]] == "success" & !is.na(pbp23$two_point_conv_result[[i]])) {
    pbp23$play_pts[[i]] <- 2
  }
  if (pbp23$safety[[i]] == 1 & !is.na(pbp23$safety[[i]])) {
    pbp23$play_pts[[i]] <- 2
  }
  if (pbp23$touchdown[[i]] == 1 & !is.na(pbp23$touchdown[[i]])) {
    pbp23$play_pts[[i]] <- 6
  }
}

#Removing kickoffs and timeouts from play-by-play data
pbp23 %>%
  filter(play_type != "kickoff") %>%
  filter(!is.na(ydsnet)) -> pbp23_1

#Finding out starting and ending field position for each drive
pbp23_1 %>%
  group_by(game_id, drive, posteam) %>%
  dplyr::summarize(starting_field = first(yardline_100),
                   total_yds = first(ydsnet),
                   drive_pts = sum(play_pts)) -> drive_data

#Summarizing drive data for each starting field position
drive_data %>%
  group_by(starting_field) %>%
  dplyr::summarize(avg_yds = mean(total_yds),
                   avg_pts = mean(drive_pts),
                   count = n()) -> field_pos23

#Removing the NA drives (not sure why they're NAs but it's only a small sample)
field_pos23 %>% filter(!is.na(starting_field)) -> field_pos23


#Figure 5
ggplot(field_pos23, aes(starting_field, avg_pts)) + geom_point() +
  labs(x = "Starting Field Position", y = "Avg Points per Drive") +
  ggtitle("NFL Drives by Field Position (2023 season)")
cor(field_pos23$starting_field, field_pos23$avg_pts)
#-0.8267012 (negatively strong, but not as strong as yards)


## ANALYSIS OF PAST 25 SEASONS

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

#Figure 1
ggplot(field_pos, aes(starting_field, avg_pts)) + geom_point() +
  labs(x = "Starting Field Position", y = "Avg Pts per Drive") +
  ggtitle("NFL Drives by Field Position (1999-2023 seasons)")
cor(field_pos$starting_field, field_pos$avg_pts)
#-0.900323 (negatively strong, but not as strong as yards)
#Once again stronger than the 2023 season
#We also see an outlier for drives starting at the opposing team's 2 yard line


##Calculating PAA without Seasonal Adjustments

#Creating punter data function for a specific season
punter_data_func <- function(pbpxx, year) {
  
  pbpxx %>% filter(play_type == "punt" & punt_blocked == 0 &
                     safety == 0) -> pbp_punts
  
  #Finding out avg yds per punt for each yard line
  pbp_punts %>%
    group_by(yardline_100) %>%
    dplyr::summarize(avg_dist = mean(kick_distance) - 20 * mean(touchback),
                     count = n()) -> punt_data
  
  pbp_punts %>% mutate(exp_dist = 0,
                       obs_ydline = 100 - (yardline_100 - kick_distance) - 20 * touchback,
                       exp_ydline = 0,
                       obs_pts = 0,
                       exp_pts = 0) -> pbp_punts
  
  for (i in 1:nrow(pbp_punts)) {
    if(pbp_punts$obs_ydline[[i]] >= 100) {
      pbp_punts$obs_ydline[[i]] <- 80 #For punts returned from the end zone (very rare)
    }
    pbp_punts$exp_dist[[i]] <- punt_data$avg_dist[which(punt_data$yardline_100 == pbp_punts$yardline_100[[i]])]
    pbp_punts$exp_ydline[[i]] <- round(100 - (pbp_punts$yardline_100[[i]] - pbp_punts$exp_dist[[i]]))
    pbp_punts$obs_pts[[i]] <- field_pos$avg_pts[which(field_pos$starting_field == pbp_punts$obs_ydline[[i]])]
    pbp_punts$exp_pts[[i]] <- field_pos$avg_pts[which(field_pos$starting_field == pbp_punts$exp_ydline[[i]])]
  }
  
  pbp_punts %>% mutate(net_pts = exp_pts - obs_pts) -> pbp_punts
  
  #Group by punter
  pbp_punts %>%
    group_by(punter_player_id) %>%
    dplyr::summarize(player = first(punter_player_name),
                     season = year,
                     punts = n(),
                     avg_kick_dist = mean(kick_distance),
                     long_kick = max(kick_distance),
                     touchbacks = sum(touchback),
                     PAA = sum(net_pts),
                     avg_PAA = mean(net_pts)) -> punter_data
}

#Punter data for each season
punter_data99 <- punter_data_func(pbp99, 1999)
punter_data00 <- punter_data_func(pbp00, 2000)
punter_data01 <- punter_data_func(pbp01, 2001)
punter_data02 <- punter_data_func(pbp02, 2002)
punter_data03 <- punter_data_func(pbp03, 2003)
punter_data04 <- punter_data_func(pbp04, 2004)
punter_data05 <- punter_data_func(pbp05, 2005)
punter_data06 <- punter_data_func(pbp06, 2006)
punter_data07 <- punter_data_func(pbp07, 2007)
punter_data08 <- punter_data_func(pbp08, 2008)
punter_data09 <- punter_data_func(pbp09, 2009)
punter_data10 <- punter_data_func(pbp10, 2010)
punter_data11 <- punter_data_func(pbp11, 2011)
punter_data12 <- punter_data_func(pbp12, 2012)
punter_data13 <- punter_data_func(pbp13, 2013)
punter_data14 <- punter_data_func(pbp14, 2014)
punter_data15 <- punter_data_func(pbp15, 2015)
punter_data16 <- punter_data_func(pbp16, 2016)
punter_data17 <- punter_data_func(pbp17, 2017)
punter_data18 <- punter_data_func(pbp18, 2018)
punter_data19 <- punter_data_func(pbp19, 2019)
punter_data20 <- punter_data_func(pbp20, 2020)
punter_data21 <- punter_data_func(pbp21, 2021)
punter_data22 <- punter_data_func(pbp22, 2022)
punter_data23 <- punter_data_func(pbp23, 2023)

#Punter data for every season
punter_data_wo_seasonal <- rbind(punter_data99, punter_data00,
                                 punter_data01, punter_data02,
                                 punter_data03, punter_data04,
                                 punter_data05, punter_data06,
                                 punter_data07, punter_data08,
                                 punter_data09, punter_data10,
                                 punter_data11, punter_data12,
                                 punter_data13, punter_data14,
                                 punter_data15, punter_data16,
                                 punter_data17, punter_data18,
                                 punter_data19, punter_data20,
                                 punter_data21, punter_data22,
                                 punter_data23)

write.csv(punter_data_wo_seasonal, "punter_data_wo_seasonal.csv", row.names = FALSE)

#Creating total punter stats w/o seasonal adjustments
punter_data_wo_seasonal %>%
  group_by(punter_player_id) %>%
  dplyr::summarize(player = first(player),
                   first_season = min(season),
                   last_season = max(season),
                   total_punts = sum(punts),
                   avg_kick_dist = weighted.mean(avg_kick_dist, punts),
                   long_kick = max(long_kick),
                   touchbacks = sum(touchbacks),
                   total_PAA = sum(PAA),
                   avg_PAA_season = mean(avg_PAA),
                   avg_PAA_punt = weighted.mean(avg_PAA, punts),
                   high_PAA = max(PAA)) -> punter_agg_wo_seasonal

write.csv(punter_agg_wo_seasonal, "punter_agg_wo_seasonal.csv", row.names = FALSE)

#Correlation between kick distance and points gained
cor(punter_data_wo_seasonal$avg_kick_dist, punter_data_wo_seasonal$avg_PAA) #0.2525592
cor(punter_data_wo_seasonal$avg_kick_dist, punter_data_wo_seasonal$PAA) #0.294802
cor(punter_agg_wo_seasonal$avg_kick_dist, punter_agg_wo_seasonal$avg_PAA_punt) #0.1879161
cor(punter_agg_wo_seasonal$avg_kick_dist, punter_agg_wo_seasonal$total_PAA) #0.1579866



##Calculating PAA with Seasonal Adjustments
field_pos_func <- function(pbp) {
  
  pbp %>% mutate(play_pts = 0) -> pbp
  
  for (i in 1:nrow(pbp)) {
    if (pbp$field_goal_result[[i]] == "made" & !is.na(pbp$field_goal_result[[i]])) {
      pbp$play_pts[[i]] <- 3
    }
    if (pbp$extra_point_result[[i]] == "good" & !is.na(pbp$extra_point_result[[i]])) {
      pbp$play_pts[[i]] <- 1
    }
    if (pbp$two_point_conv_result[[i]] == "success" & !is.na(pbp$two_point_conv_result[[i]])) {
      pbp$play_pts[[i]] <- 2
    }
    if (pbp$safety[[i]] == 1 & !is.na(pbp$safety[[i]])) {
      pbp$play_pts[[i]] <- 2
    }
    if (pbp$touchdown[[i]] == 1 & !is.na(pbp$touchdown[[i]])) {
      pbp$play_pts[[i]] <- 6
    }
  }
  
  #Removing kickoffs and timeouts from play-by-play data
  pbp %>%
    filter(play_type != "kickoff") %>%
    filter(!is.na(ydsnet)) -> pbp
  
  #Finding out starting and ending field position for each drive
  pbp %>%
    group_by(game_id, drive, posteam) %>%
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
}

field_pos99 <- field_pos_func(pbp99)
field_pos00 <- field_pos_func(pbp00)
field_pos01 <- field_pos_func(pbp01)
field_pos02 <- field_pos_func(pbp02)
field_pos03 <- field_pos_func(pbp03)
field_pos04 <- field_pos_func(pbp04)
field_pos05 <- field_pos_func(pbp05)
field_pos06 <- field_pos_func(pbp06)
field_pos07 <- field_pos_func(pbp07)
field_pos08 <- field_pos_func(pbp08)
field_pos09 <- field_pos_func(pbp09)
field_pos10 <- field_pos_func(pbp10)
field_pos11 <- field_pos_func(pbp11)
field_pos12 <- field_pos_func(pbp12)
field_pos13 <- field_pos_func(pbp13)
field_pos14 <- field_pos_func(pbp14)
field_pos15 <- field_pos_func(pbp15)
field_pos16 <- field_pos_func(pbp16)
field_pos17 <- field_pos_func(pbp17)
field_pos18 <- field_pos_func(pbp18)
field_pos19 <- field_pos_func(pbp19)
field_pos20 <- field_pos_func(pbp20)
field_pos21 <- field_pos_func(pbp21)
field_pos22 <- field_pos_func(pbp22)
field_pos23 <- field_pos_func(pbp23)

punter_data_func_seasonal <- function(pbp, field_pos, year) {
  
  pbp %>% filter(play_type == "punt" & punt_blocked == 0 &
                   safety == 0) -> pbp_punts
  
  #Finding out avg yds per punt for each yard line
  pbp_punts %>%
    group_by(yardline_100) %>%
    dplyr::summarize(avg_dist = mean(kick_distance) - 20 * mean(touchback),
                     count = n()) -> punt_data
  
  pbp_punts %>% mutate(exp_dist = 0,
                       obs_ydline = 100 - (yardline_100 - kick_distance) - 20 * touchback,
                       exp_ydline = 0,
                       obs_pts = 0,
                       exp_pts = 0) -> pbp_punts
  
  for (i in 1:nrow(pbp_punts)) {
    if(pbp_punts$obs_ydline[[i]] >= 100) {
      pbp_punts$obs_ydline[[i]] <- 80 #For punts returned from the end zone (very rare)
    }
    pbp_punts$exp_dist[[i]] <- punt_data$avg_dist[which(punt_data$yardline_100 == pbp_punts$yardline_100[[i]])]
    pbp_punts$exp_ydline[[i]] <- round(100 - (pbp_punts$yardline_100[[i]] - pbp_punts$exp_dist[[i]]))
    pbp_punts$obs_pts[[i]] <- field_pos$avg_pts[which(field_pos$starting_field == pbp_punts$obs_ydline[[i]])]
    pbp_punts$exp_pts[[i]] <- field_pos$avg_pts[which(field_pos$starting_field == pbp_punts$exp_ydline[[i]])]
  }
  
  pbp_punts %>% mutate(net_pts = exp_pts - obs_pts) -> pbp_punts
  
  #Group by punter
  pbp_punts %>%
    group_by(punter_player_id) %>%
    dplyr::summarize(player = first(punter_player_name),
                     season = year,
                     punts = n(),
                     avg_kick_dist = mean(kick_distance),
                     long_kick = max(kick_distance),
                     touchbacks = sum(touchback),
                     PAA = sum(net_pts),
                     avg_PAA = mean(net_pts)) -> punter_data
}

#Punter data for each season
punter_data99 <- punter_data_func_seasonal(pbp99, field_pos99, 1999)
punter_data00 <- punter_data_func_seasonal(pbp00, field_pos00, 2000)
punter_data01 <- punter_data_func_seasonal(pbp01, field_pos01, 2001)
punter_data02 <- punter_data_func_seasonal(pbp02, field_pos02, 2002)
punter_data03 <- punter_data_func_seasonal(pbp03, field_pos03, 2003)
punter_data04 <- punter_data_func_seasonal(pbp04, field_pos04, 2004)
punter_data05 <- punter_data_func_seasonal(pbp05, field_pos05, 2005)
punter_data06 <- punter_data_func_seasonal(pbp06, field_pos06, 2006)
punter_data07 <- punter_data_func_seasonal(pbp07, field_pos07, 2007)
punter_data08 <- punter_data_func_seasonal(pbp08, field_pos08, 2008)
punter_data09 <- punter_data_func_seasonal(pbp09, field_pos09, 2009)
punter_data10 <- punter_data_func_seasonal(pbp10, field_pos10, 2010)
punter_data11 <- punter_data_func_seasonal(pbp11, field_pos11, 2011)
punter_data12 <- punter_data_func_seasonal(pbp12, field_pos12, 2012)
punter_data13 <- punter_data_func_seasonal(pbp13, field_pos13, 2013)
punter_data14 <- punter_data_func_seasonal(pbp14, field_pos14, 2014)
punter_data15 <- punter_data_func_seasonal(pbp15, field_pos15, 2015)
punter_data16 <- punter_data_func_seasonal(pbp16, field_pos16, 2016)
punter_data17 <- punter_data_func_seasonal(pbp17, field_pos17, 2017)
punter_data18 <- punter_data_func_seasonal(pbp18, field_pos18, 2018)
punter_data19 <- punter_data_func_seasonal(pbp19, field_pos19, 2019)
punter_data20 <- punter_data_func_seasonal(pbp20, field_pos20, 2020)
punter_data21 <- punter_data_func_seasonal(pbp21, field_pos21, 2021)
punter_data22 <- punter_data_func_seasonal(pbp22, field_pos22, 2022)
punter_data23 <- punter_data_func_seasonal(pbp23, field_pos23, 2023)

#Punter data for every season
punter_data_seasonal <- rbind(punter_data99, punter_data00,
                              punter_data01, punter_data02,
                              punter_data03, punter_data04,
                              punter_data05, punter_data06,
                              punter_data07, punter_data08,
                              punter_data09, punter_data10,
                              punter_data11, punter_data12,
                              punter_data13, punter_data14,
                              punter_data15, punter_data16,
                              punter_data17, punter_data18,
                              punter_data19, punter_data20,
                              punter_data21, punter_data22,
                              punter_data23)

write.csv(punter_data_seasonal, "punter_data_seasonal.csv", row.names = FALSE)

#Creating total punter stats w/ seasonal adjustments
punter_data_seasonal %>%
  group_by(punter_player_id) %>%
  dplyr::summarize(player = first(player),
                   first_season = min(season),
                   last_season = max(season),
                   total_punts = sum(punts),
                   avg_kick_dist = weighted.mean(avg_kick_dist, punts),
                   long_kick = max(long_kick),
                   touchbacks = sum(touchbacks),
                   total_PAA = sum(PAA),
                   avg_PAA_season = mean(avg_PAA),
                   avg_PAA_punt = weighted.mean(avg_PAA, punts),
                   high_PAA = max(PAA)) -> punter_agg_seasonal

write.csv(punter_agg_seasonal, "punter_agg_seasonal.csv", row.names = FALSE)

#Correlation between kick distance and points gained
cor(punter_data_seasonal$avg_kick_dist, punter_data_seasonal$avg_PAA) #0.2017573
cor(punter_data_seasonal$avg_kick_dist, punter_data_seasonal$PAA) #0.2483635
cor(punter_agg_seasonal$avg_kick_dist, punter_agg_seasonal$avg_PAA_punt) #0.1845165
cor(punter_agg_seasonal$avg_kick_dist, punter_agg_seasonal$total_PAA) #0.2581708


#PFF grades
punting_summary13 <- read.csv("punting_summary13.csv")
punting_summary14 <- read.csv("punting_summary14.csv")
punting_summary15 <- read.csv("punting_summary15.csv")
punting_summary16 <- read.csv("punting_summary16.csv")
punting_summary17 <- read.csv("punting_summary17.csv")
punting_summary18 <- read.csv("punting_summary18.csv")
punting_summary19 <- read.csv("punting_summary19.csv")
punting_summary20 <- read.csv("punting_summary20.csv")
punting_summary21 <- read.csv("punting_summary21.csv")
punting_summary22 <- read.csv("punting_summary22.csv")
punting_summary23 <- read.csv("punting_summary23.csv")


punt_summary <- function(punting_summary, year) {
  for (i in 1:nrow(punting_summary)) {
    punting_summary$player[i] <- paste(substr(strsplit(punting_summary$player[i], " ")[[1]][[1]], 1, 1),
                                       strsplit(punting_summary$player[i], " ")[[1]][[2]],
                                       sep = ".")
  }
  punting_summary %>% mutate(season = year) -> punting_summary
}

punt_summary(punting_summary13, 2013) -> punting_summary13
punt_summary(punting_summary14, 2014) -> punting_summary14
punt_summary(punting_summary15, 2015) -> punting_summary15
punt_summary(punting_summary16, 2016) -> punting_summary16
punt_summary(punting_summary17, 2017) -> punting_summary17
punt_summary(punting_summary18, 2018) -> punting_summary18
punt_summary(punting_summary19, 2019) -> punting_summary19
punt_summary(punting_summary20, 2020) -> punting_summary20
punt_summary(punting_summary21, 2021) -> punting_summary21
punt_summary(punting_summary22, 2022) -> punting_summary22
punt_summary(punting_summary23, 2023) -> punting_summary23


pff <- rbind(punting_summary13, punting_summary14,
             punting_summary15, punting_summary16,
             punting_summary17, punting_summary18,
             punting_summary19, punting_summary20,
             punting_summary21, punting_summary22,
             punting_summary23)

write.csv(pff, "pff_grades.csv", row.names = FALSE)


#Comparing all PFF grades to points above average
merge(pff, punter_data_seasonal, by = c("player", "season")) -> pff_seasonal
pff_seasonal %>% filter(!is.na(grades_punter)) -> pff_seasonal

cor(pff_seasonal$PAA, pff_seasonal$grades_punter) #0.3569815, weak correlation
cor(pff_seasonal$avg_PAA, pff_seasonal$grades_punter) #0.1571558
cor(pff_seasonal$avg_kick_dist, pff_seasonal$grades_punter) #0.3255408

#Checking rank correlation for seasonal adjustments
cor(pff_seasonal$PAA, pff_seasonal$grades_punter,
    method = "spearman") #0.3518032, the same
cor(pff_seasonal$avg_PAA, pff_seasonal$grades_punter,
    method = "spearman") #0.3185076, a lot better

#Without seasonal adjustments
merge(pff, punter_data_wo_seasonal,
      by = c("player", "season")) -> pff_wo_seasonal
pff_wo_seasonal %>% filter(!is.na(grades_punter)) -> pff_wo_seasonal

cor(pff_wo_seasonal$PAA,
    pff_wo_seasonal$grades_punter) #0.4825416, once again better than seasonal???
cor(pff_wo_seasonal$avg_PAA,
    pff_wo_seasonal$grades_punter) #0.3947054

#Checking rank correlation for no seasonal adjustments
cor(pff_wo_seasonal$PAA, pff_wo_seasonal$grades_punter,
    method = "spearman") #0.4538026, slightly worse
cor(pff_wo_seasonal$avg_PAA, pff_wo_seasonal$grades_punter,
    method = "spearman") #0.4678699, better??

#Figure 4
ggplot(pff_wo_seasonal, aes(PAA, grades_punter)) + geom_point() +
  labs(y = "Punter Grade") +
  ggtitle("PAA without Seasonal Adjustments")

#Figure 3
ggplot(pff_seasonal, aes(PAA, grades_punter)) + geom_point() +
  labs(y = "Punter Grade") +
  ggtitle("PAA with Seasonal Adjustments")


#ggpairs for PFF data (GGally package)
pff %>% filter(attempts >= 10) -> pff10
ggpairs(pff10[, c(6, 8, 9, 11, 17, 19)])

cor(pff10[, c(6, 8, 9, 11, 17, 19)]) -> cor_matrix

#Corrplot package (Figure 7)
corrplot(cor(pff10[, c(6, 8, 9, 11, 17, 19)]))
#PFF grades appear to be moderately correlated with hangtime

cor(pff10$grades_punter, pff10$average_hangtime) #0.6190872
cor(pff10$grades_punter, pff10$average_net_yards) #0.4827723


#Seeing if any specific season has a good correlation (w/ seasonal adjustments)
pff_seasonal %>% filter(attempts >= 10) -> pff_seasonal_sample
pff_wo_seasonal %>% filter(attempts >= 10) -> pff_wo_sample

pff_seasonal_sample %>%
  group_by(season) %>%
  dplyr::summarize(total_cor = cor(grades_punter, PAA),
                   avg_cor = cor(grades_punter, avg_PAA),
                   adj = "With") -> season_cors
#2013 had the best correlation with 0.5763533 for total and 0.5812799 for average

#Seeing if any specific season has a good correlation (w/o seasonal adjustments)
pff_wo_sample %>%
  group_by(season) %>%
  dplyr::summarize(total_cor = cor(grades_punter, PAA),
                   avg_cor = cor(grades_punter, avg_PAA),
                   adj = "Without") -> wo_season_cors
#2016 had the best total correlation with 0.6454418
#2023 had the best average correlation with 0.6514230

#Figure 6
ggplot(rbind(season_cors, wo_season_cors),
       aes(season, total_cor, group = adj,
           color = adj, fill = adj)) +
  geom_line() +
  labs(x = "Season", y = "Correlation", color = "Seasonal Adjustment") +
  scale_x_continuous(breaks = c(2013, 2015, 2017, 2019, 2021, 2023))


#Average punt distance over time
pbp %>% filter(play_type == "punt" & punt_blocked == 0 &
                 safety == 0) -> pbp_punts
pbp_punts %>%
  group_by(season) %>%
  dplyr::summarize(avg_kick_dist = mean(kick_distance)) -> punt_dist

ggplot(punt_dist, aes(season, avg_kick_dist)) + geom_line() +
  labs(x = "Year", y = "Avg Punt Distance (yds)") +
  ggtitle("Average NFL Punt Distance (1999-2023)")

#Avg punt distance
pbp %>% filter(play_type == "punt" & punt_blocked == 0 &
                 safety == 0) -> pbp_punts

#Finding out avg yds per punt for each yard line
pbp_punts %>%
  group_by(yardline_100) %>%
  dplyr::summarize(avg_dist = mean(kick_distance) - 20 * mean(touchback),
                   count = n()) -> punt_data

#Figure 2
ggplot(punt_data, aes(yardline_100, avg_dist)) + geom_point() +
  labs(x = "Line of Scrimmage", y = "Avg Distance (yds)") +
  ggtitle("Average Punt Distance by Field Position (1999-2023 seasons)")

cor(punt_data$yardline_100, punt_data$avg_dist) #0.8947715, honestly higher than I thought