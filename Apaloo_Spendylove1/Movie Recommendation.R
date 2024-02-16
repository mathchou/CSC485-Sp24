library(dplyr)
setwd("C:/Users/spend/Downloads/Data Science Project")
credits <- read.csv("tmdb_5000_credits.csv")
movieinfo <- read.csv("tmdb_5000_movies.csv")

credits <- credits |>
  rename(id = movie_id)

movies <- inner_join(credits, movieinfo, by = "id")

sum(is.na(movies))
missing_values <- colSums(is.na(movies))
missing_values

movies <- na.omit(movies)

summary(movies)
max(movies$budget)

