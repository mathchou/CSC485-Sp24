library(tidyverse)
library(plyr)
library(cluster) #clustering only
library(factoextra) #more clustering only

#Assuming datasets are in working directory
school_scores <- read.csv("school_scores.csv")
SID <- read.csv("StateIndicatorsDatabase_2024.csv")

SID %>% filter(year >= 2007 & year <= 2015) -> SID

school_scores %>%
  filter(State.Code != "PR" & State.Code != "VI") %>%
  filter(Year >= 2007) -> school_scores #Due to incomplete data for Puerto Rico and the Virgin Islands

#Merge the two datasets
merge(school_scores, SID,
      by.x = c("Year", "State.Code"),
      by.y = c("year", "stabbr")) -> test_data
test_data[ , -c(100, 101)] -> test_data #Removing duplicate columns

#Inflation correction and other variables
test_data %>%
  mutate(Total.Score = Total.Math + Total.Verbal,
         Avg.Inc = (inc_pubsch * coverage) + (inc_nonpubsch * (1 - coverage)),
         q1_pct = necm_enroll_q1 / necm_enroll_state,
         predcost_state_inf = necm_predcost_state / ((1.0167)^(Year - 2009)),
         ppcstot_state_inf = necm_ppcstot_state / ((1.0167)^(Year - 2009)),
         fundinggap_state_inf = necm_fundinggap_state / ((1.0167)^(Year - 2009)),
         predcost_q1_inf = necm_predcost_q1 / ((1.0167)^(Year - 2009)),
         ppcstot_q1_inf = necm_ppcstot_q1 / ((1.0167)^(Year - 2009)),
         fundinggap_q1_inf = necm_fundinggap_q1 / ((1.0167)^(Year - 2009)),
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
         Pct.over.100k = Family.Income.More.than.100k.Test.takers / Total.Test.takers) -> test_data

#Aggregating data by year
test_data %>%
  group_by(Year) %>%
  dplyr::summarize(Total.Math = weighted.mean(Total.Math, Total.Test.takers),
            Total.Verbal = weighted.mean(Total.Verbal, Total.Test.takers),
            Total.Score = Total.Math + Total.Verbal,
            Total.Test.takers = sum(Total.Test.takers),
            effort = mean(effort),
            inc_effort = mean(inc_effort)) -> year

#Figure 1
ggplot(year, aes(Year)) +
  geom_line(aes(y = Total.Math, color = "Math")) +
  geom_line(aes(y = Total.Verbal, color = "Verbal")) +
  ggtitle("Average U.S. SAT Scores by Year (2005-2015)") +
  labs(y = "Avg Section Score", color = "Section") + ylim(495, 515)

#Creating dataset for funding variables
test_data %>% filter(Year >= 2009 & State.Code != "HI") -> funding_data
#Funding data is only available after 2009 and not in Hawaii

#Aggregating funding data by year
funding_data %>%
  group_by(Year) %>%
  dplyr::summarize(necm_predcost_state = weighted.mean(necm_predcost_state,
                                                       necm_enroll_state),
                   necm_ppcstot_state = weighted.mean(necm_ppcstot_state,
                                                      necm_enroll_state),
                   predcost_state_inf = weighted.mean(predcost_state_inf,
                                                      necm_enroll_state),
                   ppcstot_state_inf = weighted.mean(ppcstot_state_inf,
                                                     necm_enroll_state)) -> funding_year

#Figure 2
ggplot(funding_year, aes(Year, necm_ppcstot_state)) + geom_line() +
  labs(y = "Money Spent Per Pupil (USD)") +
  ggtitle("U.S. Educational Spending (2009-2015)")

#Aggregating data by year and region
test_data %>%
  group_by(Year, region4) %>%
  dplyr::summarize(Total.Math = weighted.mean(Total.Math, Total.Test.takers),
            Total.Verbal = weighted.mean(Total.Verbal, Total.Test.takers),
            Total.Score = Total.Math + Total.Verbal,
            Total.Test.takers = sum(Total.Test.takers),
            effort = mean(effort),
            inc_effort = mean(inc_effort)) -> region_year

#Test scores by region (Figure 4)
ggplot(region_year, aes(x = Year, y = Total.Score, color = region4)) +
  geom_line() + ggtitle("Average SAT Score by US Region from 2007-2015") +
  labs(y = "Avg Score", color = "Region")
#2010 Midwest is a serious outlier...

#Test scores vs sample size (Figure 5)
ggplot(region_year, aes(x = Total.Test.takers/1000, y = Total.Score, color = region4)) +
  geom_point() + ggtitle("Average SAT Score by US Region from 2007-2015") +
  labs(x = "# of Test Takers (in thousands)", y = "Avg Score", color = "Region")
#...probably because of smaller sample size

cor(region_year$Total.Score, region_year$Total.Test.takers) #-0.9583586, negatively strong!

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

#Figure 3
ggplot(Scores.by.Income, aes(x = Income, y = Total.Score)) + geom_boxplot() +
  ggtitle("Distribution of Average SAT Scores by Income Bracket") +
  labs(x = "Income Range", y = "Avg Score")
#There is a slight difference between the median scores based on income bracket, even with removing the observations with small samples
#The lowest income bracket has the highest variance

#Outliers: 2014 DC, 2007 IL, 2008 IA, 2008 MN


## TEST SCORE ANALYSIS

#Test scores by year (Figure 6)
ggplot(funding_data, aes(x = Year, y = Total.Score, color = region4)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap( ~ State.Name, nrow = 5) +
  ggtitle("U.S. Average SAT Scores by State (2007-2015)") +
  labs(y = "Avg Test Score", color = "Region")

#Finding slopes of test scores per year by state
test_models <- dlply(funding_data, "State.Name", function(df) 
  lm(Total.Score ~ Year, data = df))

ldply(test_models, coef) -> state_test_lm


##ACTUAL SPENDING ANALYSIS

#Actual spending by year (unused)
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

cor(funding_data$Total.Score, funding_data$necm_ppcstot_state) #-0.3162577????


## SPENDING ANALYSIS WITH INFLATION

#Actual spending by year (Figure 7)
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
#Correlation between the average year-to-year differences of actual spending w/ inflation and test scores: 0.01309443


## ACTUAL SPENDING VS TEST SCORES ANALYSIS

#Actual spending vs test scores for each state (Figure 8)
ggplot(funding_data, aes(x = ppcstot_state_inf/1000,
                         y = Total.Score,
                         color = region4)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap( ~ State.Name, nrow = 5) +
  ggtitle("Actual Spending vs Test Scores by U.S. State") +
  labs(x = "Actual Spending Adjusted for Inflation (in $1000s)",
       y = "Average SAT Score") +
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


## Cluster analysis

#Removing extra variables
funding_state[ , -c(1:4)] -> funding_cluster
rownames(funding_cluster) <- funding_state$State.Code

#Heatmap of all 50 states
funding_cluster %>% scale() %>% as.matrix() %>% heatmap(scale = "row")


#Only looking at the high-low correlation states
state_extreme_cor[ , -c(1:4)] -> funding_cluster10
rownames(funding_cluster10) <- state_extreme_cor$State.Code

#Heatmap of strongly correlated states (Figure 9)
funding_cluster10 %>% scale() %>% as.matrix() %>%
  heatmap(scale = "row", margins = c(8, 5),
          Colv = NA)
#Student enrollment and actual spending seem to inversely correlated for these states