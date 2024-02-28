library(dplyr)
library(jsonlite)

setwd("C:/Users/spend/Downloads/Data Science Project")
credits <- read.csv("tmdb_5000_credits.csv",  stringsAsFactors = FALSE)
movieinfo <- read.csv("tmdb_5000_movies.csv",  stringsAsFactors = FALSE)

credits <- credits |>
  rename(id = movie_id)


movies <- inner_join(credits, movieinfo, by = "id")
movies <- movies[, !(names(movies) %in% c("homepage", "title.y", "crew",
                                          "production_companies","production_countries", 
                                          "original_title", "keywords", "overview", "keywords", "tagline"))]

json_columns <- c("cast", "genres", "spoken_languages")
for (col in json_columns) {
  movies[[col]] <- lapply(movies[[col]], jsonlite::fromJSON)
}

movies <- movies |>
  rename(title=title.x)

movies[movies == ""] <- NA
movies <- na.omit(movies)

missing_values <- colSums(is.na(movies))
missing_values

movies <- subset(movies, cast != "[]")

max(movies$budget)
min(movies$budget)


