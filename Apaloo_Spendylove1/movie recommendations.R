setwd("C:/Users/spend/Downloads/Data Science Project")
library(dplyr)
library(ggplot2)
library(tidyr)
library(reshape2)


a <- read.csv("Titles.csv")
b <- read.csv ("Dataset.csv")
movies <- inner_join(a, b, by = "item_id")

head(movies)

#Movie Ratings
ratings_count <- movies|>
  count(rating) |>
  rename(Rating = rating, Count = n)

ggplot(ratings_count, aes(x = Rating, y = Count, fill = Rating )) +
  geom_bar(stat = "identity") +
  ggtitle("Rating Count") +
  xlab("Rating") +
  ylab("Count") +
  theme(legend.position="none")


#Number of movies users watched 

user_movie_counts <- movies |>
  group_by(user_id) |>
  summarize(total_movies_watched = n())

#Most watched movies
most_watched_movies <- movies |>
  group_by(title) |>
  summarise(watch_count = n()) |>
  arrange(desc(watch_count)) |>
  top_n(10)

ggplot(most_watched_movies, aes(x = reorder(title, watch_count), 
                                y = watch_count,  fill = title)) +
  geom_bar(stat = "identity") +
  ggtitle("Most Watched Movies") +
  xlab("Title") +
  ylab("Views") +
  coord_flip()+ theme(legend.position="none")

#avg rating 
movies$rating <- as.numeric(movies$rating)
average_ratings <- movies |>
  filter(title %in% most_watched_movies$title) |>
  group_by(title) |>
  summarise(avg_rating = mean(rating))


#Building a recommender system using collaborative filtering


user_movie_matrix <- dcast(movies, user_id ~ item_id, value.var = "rating", fun.aggregate = mean)

user_movie_matrix[is.na(user_movie_matrix)] <- 0


