library(tidyverse)
library(plyr)
library(ggfortify) #autoplot only
library(cluster) #clustering only
library(factoextra) #more clustering only

#Feb 1
school_scores <- read.csv("school_scores.csv")
SID <- read.csv("StateIndicatorsDatabase_2024.csv")

#summary(SID)
#summary(SID$effort)
#summary(SID$necm_fundinggap_state)

#summary(school_scores)

ggplot(school_scores, aes(Total.Math, Total.Verbal)) + geom_point()
cor(school_scores$Total.Math, school_scores$Total.Verbal) #0.9786387

#Feb 5
school_scores %>%
  mutate(Total.Score = Total.Math + Total.Verbal,
         Total.Score.20k = Family.Income.Less.than.20k.Math +
           Family.Income.Less.than.20k.Verbal,
         Total.Score.20.40k = Family.Income.Between.20.40k.Math +
           Family.Income.Between.20.40k.Verbal,
         Total.Score.40.60k = Family.Income.Between.40.60k.Math +
           Family.Income.Between.40.60k.Verbal,
         Total.Score.60.80k = Family.Income.Between.60.80k.Math +
           Family.Income.Between.60.80k.Verbal,
         Total.Score.80.100k = Family.Income.Between.80.100k.Math +
           Family.Income.Between.80.100k.Verbal,
         Total.Score.100k = Family.Income.More.than.100k.Math +
           Family.Income.More.than.100k.Verbal,
         Pct.under.20k = Family.Income.Less.than.20k.Test.takers / Total.Test.takers,
         Pct.20.40k = Family.Income.Between.20.40k.Test.takers / Total.Test.takers,
         Pct.40.60k = Family.Income.Between.40.60k.Test.takers / Total.Test.takers,
         Pct.60.80k = Family.Income.Between.60.80k.Test.takers / Total.Test.takers,
         Pct.80.100k = Family.Income.Between.80.100k.Test.takers / Total.Test.takers,
         Pct.over.100k = Family.Income.More.than.100k.Test.takers / Total.Test.takers) -> school_scores

weighted.mean(school_scores$Total.Math, school_scores$Total.Test.takers) #511.286
weighted.mean(school_scores$Total.Verbal, school_scores$Total.Test.takers) #500.7743

SID %>%
  mutate(Avg.Inc = (inc_pubsch * coverage) + (inc_nonpubsch * (1 - coverage)),
         q1_pct = necm_enroll_q1 / necm_enroll_state) -> SID


school_scores %>%
  filter(State.Code != "PR" & State.Code != "VI") %>%
  filter(Year >= 2007) -> school_scores #Due to incomplete data for Puerto Rico and the Virgin Islands

SID %>% filter(year >= 2007 & year <= 2015) -> SID

#Merge the two datasets
merge(school_scores, SID,
      by.x = c("Year", "State.Code"),
      by.y = c("year", "stabbr")) -> test_data
test_data[ , -c(113, 114)] -> test_data #Removing duplicate columns


cor(test_data$Total.Score, test_data$effort) #0.04445599
cor(test_data$Total.Score, test_data$inc_effort) #-0.08580547


#Feb 6

#This was already done on Feb 5, but now I'm updating it since we've merged data
test_data %>%
  group_by(State.Code) %>%
  dplyr::summarize(State.Name = first(State.Name),
            Total.Math = weighted.mean(Total.Math, Total.Test.takers),
            Total.Verbal = weighted.mean(Total.Verbal, Total.Test.takers),
            Total.Score = Total.Math + Total.Verbal,
            Total.Test.takers = sum(Total.Test.takers),
            effort = mean(effort),
            inc_effort = mean(inc_effort),
            Test.takers.20k = sum(Family.Income.Less.than.20k.Test.takers),
            Pct.under.20k = Test.takers.20k / Total.Test.takers,
            Total.Score.20k = weighted.mean(Total.Score.20k,
                                            Family.Income.Less.than.20k.Test.takers),
            Test.takers.20.40k = sum(Family.Income.Between.20.40k.Test.takers),
            Pct.20.40k = Test.takers.20.40k / Total.Test.takers,
            Total.Score.20.40k = weighted.mean(Total.Score.20.40k,
                                               Family.Income.Between.20.40k.Test.takers),
            Test.takers.40.60k = sum(Family.Income.Between.40.60k.Test.takers),
            Pct.40.60k = Test.takers.40.60k / Total.Test.takers,
            Total.Score.40.60k = weighted.mean(Total.Score.40.60k,
                                               Family.Income.Between.40.60k.Test.takers),
            Test.takers.60.80k = sum(Family.Income.Between.60.80k.Test.takers),
            Pct.60.80k = Test.takers.60.80k / Total.Test.takers,
            Total.Score.60.80k = weighted.mean(Total.Score.60.80k,
                                               Family.Income.Between.60.80k.Test.takers),
            Test.takers.80.100k = sum(Family.Income.Between.80.100k.Test.takers),
            Pct.80.100k = Test.takers.80.100k / Total.Test.takers,
            Total.Score.80.100k = weighted.mean(Total.Score.80.100k,
                                                Family.Income.Between.80.100k.Test.takers),
            Test.takers.100k = sum(Family.Income.More.than.100k.Test.takers),
            Pct.over.100k = Test.takers.100k / Total.Test.takers,
            Total.Score.100k = weighted.mean(Total.Score.100k,
                                             Family.Income.More.than.100k.Test.takers)) -> state

test_data %>%
  group_by(Year) %>%
  dplyr::summarize(Total.Math = weighted.mean(Total.Math, Total.Test.takers),
            Total.Verbal = weighted.mean(Total.Verbal, Total.Test.takers),
            Total.Score = Total.Math + Total.Verbal,
            Total.Test.takers = sum(Total.Test.takers),
            effort = mean(effort),
            inc_effort = mean(inc_effort),
            Test.takers.20k = sum(Family.Income.Less.than.20k.Test.takers),
            Pct.under.20k = Test.takers.20k / Total.Test.takers,
            Total.Score.20k = weighted.mean(Total.Score.20k,
                                            Family.Income.Less.than.20k.Test.takers),
            Test.takers.20.40k = sum(Family.Income.Between.20.40k.Test.takers),
            Pct.20.40k = Test.takers.20.40k / Total.Test.takers,
            Total.Score.20.40k = weighted.mean(Total.Score.20.40k,
                                               Family.Income.Between.20.40k.Test.takers),
            Test.takers.40.60k = sum(Family.Income.Between.40.60k.Test.takers),
            Pct.40.60k = Test.takers.40.60k / Total.Test.takers,
            Total.Score.40.60k = weighted.mean(Total.Score.40.60k,
                                               Family.Income.Between.40.60k.Test.takers),
            Test.takers.60.80k = sum(Family.Income.Between.60.80k.Test.takers),
            Pct.60.80k = Test.takers.60.80k / Total.Test.takers,
            Total.Score.60.80k = weighted.mean(Total.Score.60.80k,
                                               Family.Income.Between.60.80k.Test.takers),
            Test.takers.80.100k = sum(Family.Income.Between.80.100k.Test.takers),
            Pct.80.100k = Test.takers.80.100k / Total.Test.takers,
            Total.Score.80.100k = weighted.mean(Total.Score.80.100k,
                                                Family.Income.Between.80.100k.Test.takers),
            Test.takers.100k = sum(Family.Income.More.than.100k.Test.takers),
            Pct.over.100k = Test.takers.100k / Total.Test.takers,
            Total.Score.100k = weighted.mean(Total.Score.100k,
                                             Family.Income.More.than.100k.Test.takers)) -> year

ggplot(state, aes(Total.Math, Total.Verbal)) + geom_point()
cor(state$Total.Math, state$Total.Verbal) #0.9815697

ggplot(year, aes(Total.Math, Total.Verbal)) + geom_point()
cor(year$Total.Math, year$Total.Verbal) #0.9044226, not as good bc of small sample


#Feb 8
test_data %>%
  group_by(Year, region4) %>%
  dplyr::summarize(Total.Math = weighted.mean(Total.Math, Total.Test.takers),
            Total.Verbal = weighted.mean(Total.Verbal, Total.Test.takers),
            Total.Score = Total.Math + Total.Verbal,
            Total.Test.takers = sum(Total.Test.takers),
            effort = mean(effort),
            inc_effort = mean(inc_effort),
            Test.takers.20k = sum(Family.Income.Less.than.20k.Test.takers),
            Pct.under.20k = Test.takers.20k / Total.Test.takers,
            Total.Score.20k = weighted.mean(Total.Score.20k,
                                            Family.Income.Less.than.20k.Test.takers),
            Test.takers.20.40k = sum(Family.Income.Between.20.40k.Test.takers),
            Pct.20.40k = Test.takers.20.40k / Total.Test.takers,
            Total.Score.20.40k = weighted.mean(Total.Score.20.40k,
                                               Family.Income.Between.20.40k.Test.takers),
            Test.takers.40.60k = sum(Family.Income.Between.40.60k.Test.takers),
            Pct.40.60k = Test.takers.40.60k / Total.Test.takers,
            Total.Score.40.60k = weighted.mean(Total.Score.40.60k,
                                               Family.Income.Between.40.60k.Test.takers),
            Test.takers.60.80k = sum(Family.Income.Between.60.80k.Test.takers),
            Pct.60.80k = Test.takers.60.80k / Total.Test.takers,
            Total.Score.60.80k = weighted.mean(Total.Score.60.80k,
                                               Family.Income.Between.60.80k.Test.takers),
            Test.takers.80.100k = sum(Family.Income.Between.80.100k.Test.takers),
            Pct.80.100k = Test.takers.80.100k / Total.Test.takers,
            Total.Score.80.100k = weighted.mean(Total.Score.80.100k,
                                                Family.Income.Between.80.100k.Test.takers),
            Test.takers.100k = sum(Family.Income.More.than.100k.Test.takers),
            Pct.over.100k = Test.takers.100k / Total.Test.takers,
            Total.Score.100k = weighted.mean(Total.Score.100k,
                                             Family.Income.More.than.100k.Test.takers)) -> region_year

#Normalized test scores by year
test_data %>%
  mutate(norm.math = NA, norm.verbal = NA, norm.total = NA) -> test_data

for (i in 1:nrow(test_data)) {
  test_data$norm.math[i] <- test_data$Total.Math[i] / year$Total.Math[which(year$Year == test_data$Year[i])]
  test_data$norm.verbal[i] <- test_data$Total.Verbal[i] / year$Total.Verbal[which(year$Year == test_data$Year[i])]
  test_data$norm.total[i] <- test_data$Total.Score[i] / year$Total.Score[which(year$Year == test_data$Year[i])]
}


#Feb 10

#Line charts
ggplot(region_year, aes(x = Year, y = Total.Score, color = region4)) +
  geom_line() + ggtitle("Average SAT Score by US Region from 2007-2015") +
  labs(y = "Avg Score", color = "Region")
#2010 Midwest is a serious outlier...

ggplot(region_year, aes(x = Total.Test.takers/1000, y = Total.Score, color = region4)) +
  geom_point() + ggtitle("Average SAT Score by US Region from 2007-2015") +
  labs(x = "# of Test Takers (in thousands)", y = "Avg Score", color = "Region")
#...probably because of smaller sample size

cor(region_year$Total.Score, region_year$Total.Test.takers) #-0.9583586, negatively strong!
cor(region_year$Total.Score, region_year$Test.takers.100k) #-0.6819052, still negative
cor(region_year$Total.Score, region_year$Pct.under.20k) #-0.1714971

cor(test_data$Total.Score, test_data$Pct.under.20k) #-0.286964
#Having a higher % of test takers from low income families does not directly affect SAT scores


#Dividing into different income brackets
data.frame(Year = test_data$Year,
           State.Code = test_data$State.Code,
           State.Name = test_data$State.Name,
           Income = "Less than 20k",
           Median.Inc = 10,
           Test.takers = test_data$Family.Income.Less.than.20k.Test.takers,
           Avg.Math = test_data$Family.Income.Less.than.20k.Math,
           Avg.Verbal = test_data$Family.Income.Less.than.20k.Verbal,
           Total.Score = test_data$Total.Score.20k) -> scores.20k
data.frame(Year = test_data$Year,
           State.Code = test_data$State.Code,
           State.Name = test_data$State.Name,
           Income = "Between 20k and 40k",
           Median.Inc = 30,
           Test.takers = test_data$Family.Income.Between.20.40k.Test.takers,
           Avg.Math = test_data$Family.Income.Between.20.40k.Math,
           Avg.Verbal = test_data$Family.Income.Between.20.40k.Verbal,
           Total.Score = test_data$Total.Score.20.40k) -> scores.20.40k
data.frame(Year = test_data$Year,
           State.Code = test_data$State.Code,
           State.Name = test_data$State.Name,
           Income = "Between 40k and 60k",
           Median.Inc = 50,
           Test.takers = test_data$Family.Income.Between.40.60k.Test.takers,
           Avg.Math = test_data$Family.Income.Between.40.60k.Math,
           Avg.Verbal = test_data$Family.Income.Between.40.60k.Verbal,
           Total.Score = test_data$Total.Score.40.60k) -> scores.40.60k
data.frame(Year = test_data$Year,
           State.Code = test_data$State.Code,
           State.Name = test_data$State.Name,
           Income = "Between 60k and 80k",
           Median.Inc = 70,
           Test.takers = test_data$Family.Income.Between.60.80k.Test.takers,
           Avg.Math = test_data$Family.Income.Between.60.80k.Math,
           Avg.Verbal = test_data$Family.Income.Between.60.80k.Verbal,
           Total.Score = test_data$Total.Score.60.80k) -> scores.60.80k
data.frame(Year = test_data$Year,
           State.Code = test_data$State.Code,
           State.Name = test_data$State.Name,
           Income = "Between 80k and 100k",
           Median.Inc = 90,
           Test.takers = test_data$Family.Income.Between.80.100k.Test.takers,
           Avg.Math = test_data$Family.Income.Between.80.100k.Math,
           Avg.Verbal = test_data$Family.Income.Between.80.100k.Verbal,
           Total.Score = test_data$Total.Score.80.100k) -> scores.80.100k
data.frame(Year = test_data$Year,
           State.Code = test_data$State.Code,
           State.Name = test_data$State.Name,
           Income = "More than 100k",
           Median.Inc = 110,
           Test.takers = test_data$Family.Income.More.than.100k.Test.takers,
           Avg.Math = test_data$Family.Income.More.than.100k.Math,
           Avg.Verbal = test_data$Family.Income.More.than.100k.Verbal,
           Total.Score = test_data$Total.Score.100k) -> scores.100k

rbind(scores.20k, scores.20.40k, scores.40.60k,
      scores.60.80k, scores.80.100k, scores.100k) -> Scores.by.Income

Scores.by.Income$Income <- factor(Scores.by.Income$Income,
                                  levels = c("Less than 20k",
                                             "Between 20k and 40k",
                                             "Between 40k and 60k",
                                             "Between 60k and 80k",
                                             "Between 80k and 100k",
                                             "More than 100k"),
                                  ordered = TRUE)

Scores.by.Income %>% filter(Test.takers >= 100) -> Scores.by.Income
#Some observations with less than 100 test takers skew the data

ggplot(Scores.by.Income, aes(x = Income, y = Total.Score)) + geom_boxplot() +
  ggtitle("Distribution of Average SAT Scores by Income Bracket") +
  labs(x = "Income Range", y = "Avg Score")
#There is a slight difference between the median scores based on income bracket, even with removing the observations with small samples
#The lowest income bracket has the highest variance

#Outliers: 2014 DC, 2007 IL, 2008 IA, 2008 MN


#Feb 19 (after adding avg income to SID data)

test_data %>% filter(Year >= 2009 & State.Code != "HI") -> funding_data
#Funding data is only available after 2009 and not in Hawaii

#Inflation correction
funding_data %>%
  mutate(predcost_state_inf = necm_predcost_state / ((1.0167)^(Year - 2009)),
         ppcstot_state_inf = necm_ppcstot_state / ((1.0167)^(Year - 2009)),
         fundinggap_state_inf = necm_fundinggap_state / ((1.0167)^(Year - 2009)),
         predcost_q1_inf = necm_predcost_q1 / ((1.0167)^(Year - 2009)),
         ppcstot_q1_inf = necm_ppcstot_q1 / ((1.0167)^(Year - 2009)),
         fundinggap_q1_inf = necm_fundinggap_q1 / ((1.0167)^(Year - 2009))) -> funding_data

#Test scores by year
ggplot(funding_data, aes(x = Year, y = Total.Score, color = region4)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap( ~ State.Name, nrow = 5) +
  labs(y = "Avg Test Score") +
  theme(legend.position = "none")

#Finding slopes of test scores per year by state
test_models <- dlply(funding_data, "State.Name", function(df) 
  lm(Total.Score ~ Year, data = df))

ldply(test_models, coef) -> state_test_lm


##FUNDING GAP ANALYSIS

#Funding gap by year
ggplot(funding_data, aes(x = Year, y = necm_fundinggap_state, color = region4)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap( ~ State.Name, nrow = 5) +
  labs(y = "Funding Gap between Predicted and Actual Spending") +
  theme(legend.position = "none")

#Finding slopes of funding gap per year by state
funding_models <- dlply(funding_data, "State.Name", function(df) 
  lm(necm_fundinggap_state ~ Year, data = df))

ldply(funding_models, coef) -> state_funding_lm

cor(state_funding_lm$Year, state_test_lm$Year)
#Correlation between the average year-to-year differences of funding gap and test scores: 0.03259532

##ACTUAL SPENDING ANALYSIS

#Actual spending by year
ggplot(funding_data, aes(x = Year, y = necm_ppcstot_state/1000, color = region4)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap( ~ State.Name, nrow = 5) +
  labs(y = "Actual Spending (in $1000s)") +
  ggtitle("U.S. Educational Spending by State (2009-2015)") +
  theme(legend.position = "none")

#Finding slopes of spending per year by state
spending_models <- dlply(funding_data, "State.Name", function(df) 
  lm(necm_ppcstot_state ~ Year, data = df))

ldply(spending_models, coef) -> state_spending_lm

cor(state_spending_lm$Year, state_test_lm$Year)
#Correlation between the average year-to-year differences of actual spending and test scores: 0.01567433

cor(state_spending_lm$Year, state_funding_lm$Year)
#Correlation between the average year-to-year differences of funding gap and actual spending: 0.7837172
#Finally, a decent correlation coefficient, except it makes sense why...

#Might be preferential to use spending rather than the gap

cor(funding_data$Total.Score, funding_data$necm_ppcstot_state) #-0.3162577????

ggplot(funding_data, aes(Total.Score, necm_ppcstot_state)) + geom_point()


#Feb 22

## SPENDING ANALYSIS WITH INFLATION

#Actual spending by year
ggplot(funding_data, aes(x = Year, y = ppcstot_state_inf/1000, color = region4)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap( ~ State.Name, nrow = 5) +
  labs(y = "Actual Spending Adjusted for Inflation (in $1000s)") +
  ggtitle("U.S. Educational Spending by State (2009-2015)") +
  theme(legend.position = "none")

#Finding slopes of spending per year by state
spending_models_inf <- dlply(funding_data, "State.Name", function(df) 
  lm(ppcstot_state_inf ~ Year, data = df))

ldply(spending_models_inf, coef) -> state_spending_inf_lm

cor(state_spending_inf_lm$Year, state_test_lm$Year)
cor(state_spending_inf_lm$Year, state_spending_lm$Year)

#Comparing slopes
data_comp <- data.frame(State.Name = state_funding_lm$State.Name,
                        funding = state_funding_lm$Year,
                        spending = state_spending_lm$Year,
                        spending_inf = state_spending_inf_lm$Year,
                        scores = state_test_lm$Year)

ggplot(data_comp, aes(spending, spending_inf)) + geom_point()

data_comp %>% filter(State.Name != "Idaho") -> data_comp_idaho
#States with a lot of spending tend to have a decrease in scores

cor(data_comp$spending, data_comp$scores) #0.01567433
cor(data_comp_idaho$spending, data_comp_idaho$scores) #-0.1653665

cor(data_comp$spending_inf, data_comp$scores) #0.01309443
cor(data_comp_idaho$spending_inf, data_comp_idaho$scores) #-0.1445576


## ACTUAL SPENDING VS TEST SCORES ANALYSIS

ggplot(funding_data, aes(x = ppcstot_state_inf/1000,
                         y = Total.Score,
                         color = region4)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap( ~ State.Name, nrow = 5) +
  labs(x = "Actual Spending Adjusted for Inflation (in $1000s)",
       y = "Average SAT Score") +
  ggtitle("U.S. Educational Spending by State (2009-2015)") +
  theme(legend.position = "none")
#It's all over the place

#Finding correlations of spending vs test scores by state (also with variance)
funding_data %>%
  group_by(State.Name) %>%
  dplyr::summarize(var_spend = var(ppcstot_state_inf),
                   var_test = var(Total.Score),
                   cor = cor(ppcstot_state_inf, Total.Score)) -> state_cor


#Finding slopes of spending per year by state
spending_test <- dlply(funding_data, "State.Name", function(df) 
  lm(Total.Score ~ ppcstot_state_inf, data = df))

ldply(spending_test, coef) -> spending_test_lm

state_cor %>%
  mutate(slope = spending_test_lm$ppcstot_state_inf*100) -> state_cor

cor(state_cor$var_spend, state_cor$var_test) #0.0632282

## Models

funding_data$State.Name <- factor(funding_data$State.Name)

m <- lm(Total.Score ~ Year + State.Name + necm_ppcstot_state, data = funding_data)
summary(m)

observed_fitted1 <- data.frame(Observed = funding_data$Total.Score,
                               Fitted = predict(m))

cor(observed_fitted1$Observed, observed_fitted1$Fitted)

ggplot(observed_fitted1, aes(Observed, Fitted)) + geom_point()

autoplot(m1)

###### WHICH STATES ARE MOST AFFECTED BY SPENDING AND RUN A CLUSTER ANALYSIS

#The states with the 5 most positive and 5 most negative correlations
high_cor <- c("AL", "SC", "MD", "NV", "NH")
low_cor <- c("DE", "CT", "AZ", "MI", "OH")

funding_data %>%
  filter(State.Code %in% high_cor) -> pos_cor_data

funding_data %>%
  filter(State.Code %in% low_cor) -> neg_cor_data

#Aggregate funding data for all states
funding_data %>%
  group_by(State.Code) %>%
  dplyr::summarize(State.Name = first(State.Name),
                   Total.Math = weighted.mean(Total.Math, Total.Test.takers),
                   Total.Verbal = weighted.mean(Total.Verbal, Total.Test.takers),
                   Total.Score = Total.Math + Total.Verbal,
                   Total.Test.takers = sum(Total.Test.takers),
                   effort = mean(effort),
                   inc_effort = mean(inc_effort),
                   avg_predcost_inf = weighted.mean(predcost_state_inf,
                                                      necm_enroll_state),
                   avg_ppcstot_inf = weighted.mean(ppcstot_state_inf,
                                                     necm_enroll_state),
                   total_enroll_state = sum(necm_enroll_state),
                   avg_fundinggap_inf = avg_ppcstot_inf - avg_predcost_inf,
                   avg_predcost_q1 = weighted.mean(predcost_q1_inf,
                                                    necm_enroll_state),
                   avg_ppcstot_q1 = weighted.mean(ppcstot_q1_inf,
                                                   necm_enroll_q1),
                   total_enroll_q1 = sum(necm_enroll_q1),
                   avg_fundinggap_q1 = avg_ppcstot_q1 - avg_predcost_q1,
                   avg_sal_parity25 = mean(tchsalary25_30) / mean(nontchsal25_30),
                   avg_inc_approx = mean(Avg.Inc)) -> funding_state

#5 states with the highest positive correlation
funding_state %>%
  filter(State.Code %in% high_cor) -> state_pos_cor

#5 states with the highest negative correlation
funding_state %>%
  filter(State.Code %in% low_cor) -> state_neg_cor

#Data frame with most extremes
rbind(state_pos_cor, state_neg_cor) -> state_extreme_cor

# Feb 25 (nothing special day)
#### CLUSTERING

funding_state[ , -c(1, 2)] -> funding_cluster
rownames(funding_cluster) <- funding_state$State.Code

funding_cluster %>%
  agnes(metric = "manhattan", method = "complete") %>%
  plot(which = 2)

funding_cluster %>% diana() %>% plot(which = 2)

#Copied from textbook
funding_cluster %>%
  hcut(k = 5, hc_func = "agnes",
       hc_method = "average",
       hc_metric = "euclidean") %>%
  fviz_dend()

funding_cluster %>% scale() %>% as.matrix() %>% heatmap(scale = "row")


#Feb 27
#Only looking at the high-low correlation states
state_extreme_cor[ , -c(1, 2)] -> funding_cluster10
rownames(funding_cluster10) <- state_extreme_cor$State.Code

funding_cluster10 %>% scale() %>% as.matrix() %>%
  heatmap(scale = "row", margins = c(8, 5), add.expr = abline(h = 5.5, lwd = 5),
          main = "Heatmap of the 10 Highest Correlated States", Colv = NA)
