library(tidyverse)

#Feb 1
school_scores <- read.csv("school_scores.csv")
SID <- read.csv("StateIndicatorsDatabase_2024.csv")

summary(SID)
summary(SID$effort)
summary(SID$necm_fundinggap_state)

summary(school_scores)

ggplot(school_scores, aes(Total.Math, Total.Verbal)) + geom_point()
cor(school_scores$Total.Math, school_scores$Total.Verbal) #0.9786387

#Feb 5
school_scores %>%
  mutate(Total.Score = Total.Math + Total.Verbal) -> school_scores

weighted.mean(school_scores$Total.Math, school_scores$Total.Test.takers) #511.286
weighted.mean(school_scores$Total.Verbal, school_scores$Total.Test.takers) #500.7743



school_scores %>%
  filter(State.Code != "PR" & State.Code != "VI") %>%
  filter(Year >= 2007) -> school_scores #Due to incomplete data for Puerto Rico and the Virgin Islands

SID %>% filter(year >= 2007 & year <= 2015) -> SID07

#Merge the two datasets
merge(school_scores, SID07,
      by.x = c("Year", "State.Code"),
      by.y = c("year", "stabbr")) -> test_data
test_data[ , -c(101)] -> test_data #Removing duplicate column

ggplot(test_data, aes(Total.Score, effort)) + geom_point()
cor(test_data$Total.Score, test_data$effort) #0.04445599

ggplot(test_data, aes(Total.Score, inc_effort)) + geom_point()
cor(test_data$Total.Score, test_data$inc_effort) #-0.08580547


#Feb 6

#This was already done on Feb 5, but now I'm updating it since we've merged data
test_data %>%
  group_by(State.Code) %>%
  summarize(State.Name = first(State.Name),
            Total.Math = weighted.mean(Total.Math, Total.Test.takers),
            Total.Verbal = weighted.mean(Total.Verbal, Total.Test.takers),
            Total.Test.takers = sum(Total.Test.takers),
            effort = mean(effort),
            inc_effort = mean(inc_effort),
            avg_predcost_state = mean(necm_predcost_state),
            avg_ppcstot_state = mean(necm_ppcstot_state),
            avg_outcomegap_state = mean(necm_outcomegap_state),
            avg_fundinggap_state = mean(necm_fundinggap_state)) %>%
  mutate(Total.Score = Total.Math + Total.Verbal) -> state
#Change to 2009-2015???

test_data %>%
  group_by(Year) %>%
  summarize(Total.Math = weighted.mean(Total.Math, Total.Test.takers),
            Total.Verbal = weighted.mean(Total.Verbal, Total.Test.takers),
            Total.Test.takers = sum(Total.Test.takers),
            effort = mean(effort),
            inc_effort = mean(inc_effort)) %>%
  mutate(Total.Score = Total.Math + Total.Verbal) -> year

ggplot(state, aes(Total.Math, Total.Verbal)) + geom_point()
cor(state$Total.Math, state$Total.Verbal) #0.9815697

ggplot(year, aes(Total.Math, Total.Verbal)) + geom_point()
cor(year$Total.Math, year$Total.Verbal) #0.9044226, not as good bc of small sample


ggplot(test_data, aes(Total.Score, necm_fundinggap_state)) + geom_point()

test_data %>%
  filter(!is.na(necm_fundinggap_state)) -> fund
cor(fund$Total.Score, fund$necm_fundinggap_state) #-0.01945312
