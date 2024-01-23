# Load necessary libraries
library(tidyverse)
install.packages("tidytext")
library(tidytext)
library(dplyr)

#First Dataset 
# Path to the CSV file
file_path <- "/Users/emilygelchie/Downloads/McDonald_s_Reviews.csv"

# Reading the CSV file with a different encoding if needed
data <- read.csv(file_path, fileEncoding = "ISO-8859-1")

# PART 1
# Displaying the column variables
print(colnames(data))
# "reviewer_id"   "store_name"    "category"      "store_address" "latitude"     
# "longitude"     "rating_count"  "review_time"   "review"        "rating"       

# PART 2
# Checking for missing values
nan_values <- sapply(data, function(x) sum(is.nan(x)))
print(nan_values)

# reviewer_id    store_name      category store_address      latitude     longitude 
# 0             0             0             0           660           660 
# rating_count   review_time        review        rating 
# 0             0             0             0 

# PART 3
# Running summary analysis of the columns
print(summary(data))

# reviewer_id     store_name          category         store_address     
# Min.   :    1   Length:33396       Length:33396       Length:33396      
# 1st Qu.: 8350   Class :character   Class :character   Class :character  
# Median :16698   Mode  :character   Mode  :character   Mode  :character  
# Mean   :16698                                                           
# 3rd Qu.:25047                                                           
# Max.   :33396                                                           
# 
# latitude       longitude       rating_count       review_time       
# Min.   :25.79   Min.   :-122.00   Length:33396       Length:33396      
# 1st Qu.:28.66   1st Qu.: -97.79   Class :character   Class :character  
# Median :33.93   Median : -81.47   Mode  :character   Mode  :character  
# Mean   :34.44   Mean   : -90.65                                        
# 3rd Qu.:40.73   3rd Qu.: -75.40                                        
# Max.   :44.98   Max.   : -73.46                                        
# NA's   :660     NA's   :660                                            
# review             rating         
# Length:33396       Length:33396      
# Class :character   Class :character  
# Mode  :character   Mode  :character  

# PART 4
# Creating histograms for the numerical columns
num_cols <- sapply(data, is.numeric)
hist_data <- data[, num_cols]

# Plot histograms using ggplot
hist_plots <- lapply(names(hist_data), function(x) {
  ggplot(data, aes_string(x)) + 
    geom_histogram(binwidth = 1, fill = "pink", color = "red") +
    theme_minimal() +
    ggtitle(paste("Histogram of", x))
})

# Display all histograms
do.call(gridExtra::grid.arrange, c(hist_plots, ncol = 2))

# PART 5 
# How does the sentiment derived from text reviews correlate with the overall star ratings for individual McDonald's branches, and what does this indicate about customer satisfaction and perception at different locations?
# Is there a correlation between geographical region of a McDonald’s and the average rating?
# What is the correlation between number of ratings and average overall rating?

# Second Dataset 
# Path to the second CSV file
file_path2 <- "/Users/emilygelchie/Downloads/GlobalWeatherRepository.csv"

# Reading the second CSV file
data2 <- read.csv(file_path2, fileEncoding = "ISO-8859-1")

# PART 1
# Displaying the column variables for the second dataset
print(colnames(data2))
# [1] "country"                      "location_name"               
# [3] "latitude"                     "longitude"                   
# [5] "timezone"                     "last_updated_epoch"          
# [7] "last_updated"                 "temperature_celsius"         
# [9] "temperature_fahrenheit"       "condition_text"              
# [11] "wind_mph"                     "wind_kph"                    
# [13] "wind_degree"                  "wind_direction"              
# [15] "pressure_mb"                  "pressure_in"                 
# [17] "precip_mm"                    "precip_in"                   
# [19] "humidity"                     "cloud"                       
# [21] "feels_like_celsius"           "feels_like_fahrenheit"       
# [23] "visibility_km"                "visibility_miles"            
# [25] "uv_index"                     "gust_mph"                    
# [27] "gust_kph"                     "air_quality_Carbon_Monoxide" 
# [29] "air_quality_Ozone"            "air_quality_Nitrogen_dioxide"
# [31] "air_quality_Sulphur_dioxide"  "air_quality_PM2.5"           
# [33] "air_quality_PM10"             "air_quality_us.epa.index"    
# [35] "air_quality_gb.defra.index"   "sunrise"                     
# [37] "sunset"                       "moonrise"                    
# [39] "moonset"                      "moon_phase"                  
# [41] "moon_illumination" 

# PART 2
# Checking for missing values
nan_values2 <- sapply(data2, function(x) sum(is.na(x)))
print(nan_values2)
# country                location_name 
# 0                            0 
# latitude                    longitude 
# 0                            0 
# timezone           last_updated_epoch 
# 0                            0 
# last_updated          temperature_celsius 
# 0                            0 
# temperature_fahrenheit               condition_text 
# 0                            0 
# wind_mph                     wind_kph 
# 0                            0 
# wind_degree               wind_direction 
# 0                            0 
# pressure_mb                  pressure_in 
# 0                            0 
# precip_mm                    precip_in 
# 0                            0 
# humidity                        cloud 
# 0                            0 
# feels_like_celsius        feels_like_fahrenheit 
# 0                            0 
# visibility_km             visibility_miles 
# 0                            0 
# uv_index                     gust_mph 
# 0                            0 
# gust_kph  air_quality_Carbon_Monoxide 
# 0                            0 
# air_quality_Ozone air_quality_Nitrogen_dioxide 
# 0                            0 
# air_quality_Sulphur_dioxide            air_quality_PM2.5 
# 0                            0 
# air_quality_PM10     air_quality_us.epa.index 
# 0                            0 
# air_quality_gb.defra.index                      sunrise 
# 0                            0 
# sunset                     moonrise 
# 0                            0 
# moonset                   moon_phase 
# 0                            0 
# moon_illumination 
# 0 

# PART 3
# Running summary analysis of the columns 
print(summary(data2))
# country          location_name         latitude        longitude      
# Length:24949       Length:24949       Min.   :-41.30   Min.   :-175.20  
# Class :character   Class :character   1st Qu.:  3.75   1st Qu.:  -6.84  
# Mode  :character   Mode  :character   Median : 17.25   Median :  23.24  
# Mean   : 19.30   Mean   :  21.91  
# 3rd Qu.: 41.33   3rd Qu.:  50.58  
# Max.   : 64.10   Max.   : 179.22  
# timezone         last_updated_epoch  last_updated       temperature_celsius
# Length:24949       Min.   :1.693e+09   Length:24949       Min.   :-41.90     
# Class :character   1st Qu.:1.696e+09   Class :character   1st Qu.: 14.00     
# Mode  :character   Median :1.699e+09   Mode  :character   Median : 22.00     
# Mean   :1.699e+09                      Mean   : 19.58     
# 3rd Qu.:1.702e+09                      3rd Qu.: 27.00     
# Max.   :1.705e+09                      Max.   : 45.40     
# temperature_fahrenheit condition_text        wind_mph         wind_kph     
# Min.   :-43.40         Length:24949       Min.   : 2.200   Min.   :  3.60  
# 1st Qu.: 57.20         Class :character   1st Qu.: 3.600   1st Qu.:  5.80  
# Median : 71.60         Mode  :character   Median : 5.600   Median :  9.00  
# Mean   : 67.25                            Mean   : 7.038   Mean   : 11.33  
# 3rd Qu.: 80.60                            3rd Qu.: 9.400   3rd Qu.: 15.10  
# Max.   :113.70                            Max.   :87.700   Max.   :141.10  
# wind_degree    wind_direction      pressure_mb    pressure_in      precip_mm      
# Min.   :  1.0   Length:24949       Min.   : 964   Min.   :28.47   Min.   : 0.0000  
# 1st Qu.: 70.0   Class :character   1st Qu.:1010   1st Qu.:29.83   1st Qu.: 0.0000  
# Median :150.0   Mode  :character   Median :1013   Median :29.91   Median : 0.0000  
# Mean   :160.9                      Mean   :1014   Mean   :29.93   Mean   : 0.1489  
# 3rd Qu.:250.0                      3rd Qu.:1018   3rd Qu.:30.06   3rd Qu.: 0.0200  
# Max.   :360.0                      Max.   :1074   Max.   :31.71   Max.   :31.0000  
# precip_in           humidity          cloud        feels_like_celsius
# Min.   :0.000000   Min.   :  4.00   Min.   :  0.00   Min.   :-49.10    
# 1st Qu.:0.000000   1st Qu.: 62.00   1st Qu.:  0.00   1st Qu.: 13.20    
# Median :0.000000   Median : 77.00   Median : 25.00   Median : 24.00    
# Mean   :0.005682   Mean   : 72.43   Mean   : 36.82   Mean   : 20.61    
# 3rd Qu.:0.000000   3rd Qu.: 88.00   3rd Qu.: 75.00   3rd Qu.: 29.30    
# Max.   :1.220000   Max.   :100.00   Max.   :100.00   Max.   : 73.60    
# feels_like_fahrenheit visibility_km    visibility_miles    uv_index     
# Min.   :-56.30        Min.   : 0.000   Min.   : 0.00    Min.   : 1.000  
# 1st Qu.: 55.70        1st Qu.:10.000   1st Qu.: 6.00    1st Qu.: 1.000  
# Median : 75.20        Median :10.000   Median : 6.00    Median : 1.000  
# Mean   : 69.09        Mean   : 9.586   Mean   : 5.66    Mean   : 2.229  
# 3rd Qu.: 84.80        3rd Qu.:10.000   3rd Qu.: 6.00    3rd Qu.: 1.000  
# Max.   :164.40        Max.   :32.000   Max.   :19.00    Max.   :13.000  
# gust_mph        gust_kph      air_quality_Carbon_Monoxide air_quality_Ozone
# Min.   : 0.00   Min.   :  0.00   Min.   :   96.8             Min.   :  0.00   
# 1st Qu.: 6.20   1st Qu.:  9.90   1st Qu.:  233.7             1st Qu.: 16.50   
# Median :10.20   Median : 16.30   Median :  290.4             Median : 37.20   
# Mean   :11.57   Mean   : 18.61   Mean   :  588.8             Mean   : 40.32   
# 3rd Qu.:15.40   3rd Qu.: 24.80   3rd Qu.:  454.0             3rd Qu.: 58.70   
# Max.   :68.70   Max.   :110.50   Max.   :36315.9             Max.   :555.00   
# air_quality_Nitrogen_dioxide air_quality_Sulphur_dioxide air_quality_PM2.5
# Min.   :  0.00               Min.   :  0.00              Min.   :   0.50  
# 1st Qu.:  1.10               1st Qu.:  0.50              1st Qu.:   2.50  
# Median :  4.50               Median :  1.80              Median :   7.60  
# Mean   : 13.91               Mean   :  7.82              Mean   :  25.83  
# 3rd Qu.: 15.30               3rd Qu.:  6.30              3rd Qu.:  23.00  
# Max.   :575.80               Max.   :511.20              Max.   :1558.80  
# air_quality_PM10  air_quality_us.epa.index air_quality_gb.defra.index
# Min.   :   0.50   Min.   :1.000            Min.   : 1.000            
# 1st Qu.:   4.50   1st Qu.:1.000            1st Qu.: 1.000            
# Median :  12.80   Median :1.000            Median : 1.000            
# Mean   :  45.11   Mean   :1.615            Mean   : 2.445            
# 3rd Qu.:  38.80   3rd Qu.:2.000            3rd Qu.: 2.000            
# Max.   :3566.40   Max.   :6.000            Max.   :10.000            
# sunrise             sunset            moonrise           moonset         
# Length:24949       Length:24949       Length:24949       Length:24949      
# Class :character   Class :character   Class :character   Class :character  
# Mode  :character   Mode  :character   Mode  :character   Mode  :character  
# 
# 
# 
# moon_phase        moon_illumination
# Length:24949       Min.   :  0.00   
# Class :character   1st Qu.: 18.00   
# Mode  :character   Median : 52.00   
# Mean   : 51.74   
# 3rd Qu.: 88.00   
# Max.   :100.00   

# PART 4
# Creating histograms for the numerical columns of the second dataset
num_cols2 <- sapply(data2, is.numeric)
hist_data2 <- data2[, num_cols2]

# Plot histograms using ggplot for the second dataset
for (col_name in names(hist_data2)) {
  print(
    ggplot(data2, aes_string(x = col_name)) + 
      geom_histogram(fill = "lightblue", color = "blue", binwidth = 1) +  
      theme_minimal() +
      ggtitle(paste("Histogram of", col_name))
  )
}
# There are 20+ graphs to print however since there are so many datapoints I cannot get them all to print onto one page

# PART 5
# Is there a general trend within all the data indicating global warming?
# What is the correlation between humidity and feels like temperature
# How does latitude/longitude affect temperature.

#Third Dataset
# Path to the CSV file
file_path3 <- "/Users/emilygelchie/Downloads/spotify-2023.csv"
# Reading the third CSV file
data3 <- read.csv(file_path3, fileEncoding = "ISO-8859-1")

# PART 1 
# Displaying the column variables
print(colnames(data3))

# [1] "track_name"           "artist.s._name"       "artist_count"        
# [4] "released_year"        "released_month"       "released_day"        
# [7] "in_spotify_playlists" "in_spotify_charts"    "streams"             
# [10] "in_apple_playlists"   "in_apple_charts"      "in_deezer_playlists" 
# [13] "in_deezer_charts"     "in_shazam_charts"     "bpm"                 
# [16] "key"                  "mode"                 "danceability_."      
# [19] "valence_."            "energy_."             "acousticness_."      
# [22] "instrumentalness_."   "liveness_."           "speechiness_."  

# PART 2 
# Checking for missing values (including NaN)
nan_values3 <- sapply(data3, function(x) sum(is.na(x)))
print(nan_values3)

# track_name       artist.s._name         artist_count        released_year 
# 0                    0                    0                    0 
# released_month         released_day in_spotify_playlists    in_spotify_charts 
# 0                    0                    0                    0 
# streams   in_apple_playlists      in_apple_charts  in_deezer_playlists 
# 0                    0                    0                    0 
# in_deezer_charts     in_shazam_charts                  bpm                  key 
# 0                    0                    0                    0 
# mode       danceability_.            valence_.             energy_. 
# 0                    0                    0                    0 
# acousticness_.   instrumentalness_.           liveness_.        speechiness_. 
# 0                    0                    0                    0 

# PART 3 
# Running summary analysis of the columns
print(summary(data3))
# 
# track_name        artist.s._name      artist_count   released_year 
# Length:953         Length:953         Min.   :1.000   Min.   :1930  
# Class :character   Class :character   1st Qu.:1.000   1st Qu.:2020  
# Mode  :character   Mode  :character   Median :1.000   Median :2022  
# Mean   :1.556   Mean   :2018  
# 3rd Qu.:2.000   3rd Qu.:2022  
# Max.   :8.000   Max.   :2023  
# released_month    released_day   in_spotify_playlists in_spotify_charts
# Min.   : 1.000   Min.   : 1.00   Min.   :   31        Min.   :  0.00   
# 1st Qu.: 3.000   1st Qu.: 6.00   1st Qu.:  875        1st Qu.:  0.00   
# Median : 6.000   Median :13.00   Median : 2224        Median :  3.00   
# Mean   : 6.034   Mean   :13.93   Mean   : 5200        Mean   : 12.01   
# 3rd Qu.: 9.000   3rd Qu.:22.00   3rd Qu.: 5542        3rd Qu.: 16.00   
# Max.   :12.000   Max.   :31.00   Max.   :52898        Max.   :147.00   
# streams          in_apple_playlists in_apple_charts  in_deezer_playlists
# Length:953         Min.   :  0.00     Min.   :  0.00   Length:953         
# Class :character   1st Qu.: 13.00     1st Qu.:  7.00   Class :character   
# Mode  :character   Median : 34.00     Median : 38.00   Mode  :character   
# Mean   : 67.81     Mean   : 51.91                      
# 3rd Qu.: 88.00     3rd Qu.: 87.00                      
# Max.   :672.00     Max.   :275.00                      
# in_deezer_charts in_shazam_charts        bpm            key           
# Min.   : 0.000   Length:953         Min.   : 65.0   Length:953        
# 1st Qu.: 0.000   Class :character   1st Qu.:100.0   Class :character  
# Median : 0.000   Mode  :character   Median :121.0   Mode  :character  
# Mean   : 2.666                      Mean   :122.5                     
# 3rd Qu.: 2.000                      3rd Qu.:140.0                     
# Max.   :58.000                      Max.   :206.0                     
# mode           danceability_.    valence_.        energy_.     acousticness_. 
# Length:953         Min.   :23.00   Min.   : 4.00   Min.   : 9.00   Min.   : 0.00  
# Class :character   1st Qu.:57.00   1st Qu.:32.00   1st Qu.:53.00   1st Qu.: 6.00  
# Mode  :character   Median :69.00   Median :51.00   Median :66.00   Median :18.00  
# Mean   :66.97   Mean   :51.43   Mean   :64.28   Mean   :27.06  
# 3rd Qu.:78.00   3rd Qu.:70.00   3rd Qu.:77.00   3rd Qu.:43.00  
# Max.   :96.00   Max.   :97.00   Max.   :97.00   Max.   :97.00  
# instrumentalness_.   liveness_.    speechiness_.  
# Min.   : 0.000     Min.   : 3.00   Min.   : 2.00  
# 1st Qu.: 0.000     1st Qu.:10.00   1st Qu.: 4.00  
# Median : 0.000     Median :12.00   Median : 6.00  
# Mean   : 1.581     Mean   :18.21   Mean   :10.13  
# 3rd Qu.: 0.000     3rd Qu.:24.00   3rd Qu.:11.00  
# Max.   :91.000     Max.   :97.00   Max.   :64.00  

# PART 4 
# Creating histograms for the numerical columns of the third dataset
num_cols3 <- sapply(data3, is.numeric)
hist_data3 <- data3[, num_cols3]

# Print histograms using ggplot for the third dataset
for (col_name in names(hist_data3)) {
  print(
    ggplot(data3, aes_string(x = col_name)) + 
      geom_histogram(fill = "lightgreen", color = "darkgreen", binwidth = 1) +  # Adjust binwidth if needed
      theme_minimal() +
      ggtitle(paste("Histogram of", col_name))
  )
}

# PART 5
# How does the artist's name correlate to the number of streams
# How do the apple charts compare to the apple playlists?
# Does the streaming platform affect the number of streams a song gets?

# My Datasets 1/3
# Dataset 1: NBA Player Stats 
# Path to the CSV file
file_path4 <- "/Users/emilygelchie/Downloads/archive-2/NBA_Player_Stats_2.csv"
data4 <- read.csv(file_path4, fileEncoding = "ISO-8859-1")

# PART 1 
# Displaying the column variables
print(colnames(data4))
# [1] "Rk"     "Player" "Pos"    "Age"    "Tm"     "G"      "GS"     "MP"     "FG"    
# [10] "FGA"    "FG."    "X3P"    "X3PA"   "X3P."   "X2P"    "X2PA"   "X2P."   "eFG."  
# [19] "FT"     "FTA"    "FT."    "ORB"    "DRB"    "TRB"    "AST"    "STL"    "BLK"   
# [28] "TOV"    "PF"     "PTS"    "Season" "MVP"   

# PART 2
# Checking for missing values (including NaN)
nan_values4 <- sapply(data4, function(x) sum(is.na(x)))
print(nan_values4)
# Rk Player    Pos    Age     Tm      G     GS     MP     FG    FGA    FG.    X3P 
# 0      0      0      0      0      0      0      0      0      0     88      0 
# X3PA   X3P.    X2P   X2PA   X2P.   eFG.     FT    FTA    FT.    ORB    DRB    TRB 
# 0   2198      0      0    154     88      0      0    749      0      0      0 
# AST    STL    BLK    TOV     PF    PTS Season    MVP 
# 0      0      0      0      0      0      0      0 

# PART 3
# Running summary analysis of the columns
print(summary(data4))

# Rk           Player              Pos                 Age       
# Min.   :  1.0   Length:14573       Length:14573       Min.   :18.00  
# 1st Qu.:124.0   Class :character   Class :character   1st Qu.:23.00  
# Median :243.0   Mode  :character   Mode  :character   Median :26.00  
# Mean   :244.3                                         Mean   :26.71  
# 3rd Qu.:361.0                                         3rd Qu.:30.00  
# Max.   :605.0                                         Max.   :44.00  
# 
# Tm                  G               GS              MP              FG        
# Length:14573       Min.   : 1.00   Min.   : 0.00   Min.   : 0.00   Min.   : 0.000  
# Class :character   1st Qu.:22.00   1st Qu.: 0.00   1st Qu.:11.40   1st Qu.: 1.300  
# Mode  :character   Median :48.00   Median : 7.00   Median :18.90   Median : 2.400  
# Mean   :45.54   Mean   :21.57   Mean   :19.62   Mean   : 2.932  
# 3rd Qu.:70.00   3rd Qu.:39.00   3rd Qu.:27.70   3rd Qu.: 4.100  
# Max.   :85.00   Max.   :83.00   Max.   :43.70   Max.   :12.200  
# 
# FGA              FG.              X3P              X3PA       
# Min.   : 0.000   Min.   :0.0000   Min.   :0.0000   Min.   : 0.000  
# 1st Qu.: 3.100   1st Qu.:0.3930   1st Qu.:0.0000   1st Qu.: 0.100  
# Median : 5.500   Median :0.4350   Median :0.3000   Median : 1.100  
# Mean   : 6.599   Mean   :0.4324   Mean   :0.5909   Mean   : 1.704  
# 3rd Qu.: 9.200   3rd Qu.:0.4790   3rd Qu.:1.0000   3rd Qu.: 2.800  
# Max.   :27.800   Max.   :1.0000   Max.   :5.3000   Max.   :13.200  
# NA's   :88                                        
#       X3P.             X2P              X2PA             X2P.       
#  Min.   :0.0000   Min.   : 0.000   Min.   : 0.000   Min.   :0.0000  
#  1st Qu.:0.2220   1st Qu.: 1.000   1st Qu.: 2.100   1st Qu.:0.4230  
#  Median :0.3260   Median : 1.800   Median : 3.900   Median :0.4700  
#  Mean   :0.2843   Mean   : 2.342   Mean   : 4.895   Mean   :0.4648  
#  3rd Qu.:0.3750   3rd Qu.: 3.300   3rd Qu.: 6.800   3rd Qu.:0.5140  
#  Max.   :1.0000   Max.   :12.100   Max.   :23.400   Max.   :1.0000  
#  NA's   :2198                                       NA's   :154     
#       eFG.              FT              FTA              FT.              ORB      
#  Min.   :0.0000   Min.   : 0.000   Min.   : 0.000   Min.   :0.0000   Min.   :0.00  
#  1st Qu.:0.4380   1st Qu.: 0.500   1st Qu.: 0.700   1st Qu.:0.6600   1st Qu.:0.30  
#  Median :0.4830   Median : 1.000   Median : 1.400   Median :0.7500   Median :0.70  
#  Mean   :0.4735   Mean   : 1.401   Mean   : 1.872   Mean   :0.7262   Mean   :0.91  
#  3rd Qu.:0.5240   3rd Qu.: 1.900   3rd Qu.: 2.500   3rd Qu.:0.8220   3rd Qu.:1.30  
#  Max.   :1.5000   Max.   :10.300   Max.   :13.100   Max.   :1.0000   Max.   :6.80  
#  NA's   :88                                         NA's   :749                    
#       DRB              TRB             AST              STL              BLK        
#  Min.   : 0.000   Min.   : 0.00   Min.   : 0.000   Min.   :0.0000   Min.   :0.0000  
#  1st Qu.: 1.300   1st Qu.: 1.70   1st Qu.: 0.500   1st Qu.:0.3000   1st Qu.:0.1000  
#  Median : 2.200   Median : 2.90   Median : 1.200   Median :0.5000   Median :0.2000  
#  Mean   : 2.522   Mean   : 3.43   Mean   : 1.758   Mean   :0.6215   Mean   :0.3902  
#  3rd Qu.: 3.300   3rd Qu.: 4.60   3rd Qu.: 2.300   3rd Qu.:0.9000   3rd Qu.:0.5000  
#  Max.   :12.000   Max.   :18.00   Max.   :12.800   Max.   :3.0000   Max.   :6.0000  
#                                                                                     
#       TOV              PF             PTS            Season         
#  Min.   :0.000   Min.   :0.000   Min.   : 0.000   Length:14573      
#  1st Qu.:0.600   1st Qu.:1.200   1st Qu.: 3.400   Class :character  
#  Median :1.000   Median :1.800   Median : 6.400   Mode  :character  
#  Mean   :1.132   Mean   :1.782   Mean   : 7.853                     
#  3rd Qu.:1.500   3rd Qu.:2.400   3rd Qu.:11.100                     
#  Max.   :5.700   Max.   :6.000   Max.   :36.100                     
#                                                                     
#      MVP           
#  Length:14573      
#  Class :character  
#  Mode  :character  
                  
# PART 4
# Creating histograms for the numerical columns of the fourth dataset
num_cols4 <- sapply(data4, is.numeric)
hist_data4 <- data4[, num_cols4]

# Print histograms using ggplot for the fourth dataset
for (col_name in names(hist_data4)) {
  print(
    ggplot(data4, aes_string(x = col_name)) + 
      geom_histogram(fill = "yellow", color = "orange", binwidth = 1) +  # Adjust binwidth if needed
      theme_minimal() +
      ggtitle(paste("Histogram of", col_name))
  )
}

# PART 5 
# Who is the best overall player in the dataset?
# Who is the best offensive player in the dataset?
# Who is the best defensive player in the dataset?

# My Datasets 2/3
# Dataset 2: THE World University Rankings 2016-2024
file_path5 <- "/Users/emilygelchie/Downloads/THE World University Rankings 2016-2024.csv"
data5 <- read.csv(file_path5, fileEncoding = "ISO-8859-1")

# PART 1
# Displaying the column variables
print(colnames(data5))
# 
# [1] "Rank"                    "Name"                    "Country"                
# [4] "Student.Population"      "Students.to.Staff.Ratio" "International.Students" 
# [7] "Female.to.Male.Ratio"    "Overall.Score"           "Teaching"               
# [10] "Research.Environment"    "Research.Quality"        "Industry.Impact"        
# [13] "International.Outlook"   "Year"   

# PART 2
# Checking for missing values
nan_values5 <- sapply(data5, function(x) sum(is.na(x)))
print(nan_values5)
# Rank                    Name                 Country 
# 0                       0                       0 
# Student.Population Students.to.Staff.Ratio  International.Students 
# 0                       0                       0 
# Female.to.Male.Ratio           Overall.Score                Teaching 
# 0                       0                       0 
# Research.Environment        Research.Quality         Industry.Impact 
# 0                       0                       0 
# International.Outlook                    Year 
# 0  

# PART 3 for
# Running summary analysis of the columns
print(summary(data5))
# 
# Rank            Name             Country          Student.Population
# Min.   :   1.0   Length:12430       Length:12430       Min.   :     25   
# 1st Qu.: 346.0   Class :character   Class :character   1st Qu.:  10150   
# Median : 691.0   Mode  :character   Mode  :character   Median :  17824   
# Mean   : 736.8                                         Mean   :  23367   
# 3rd Qu.:1078.0                                         3rd Qu.:  29218   
# Max.   :1904.0                                         Max.   :1824383   
# Students.to.Staff.Ratio International.Students Female.to.Male.Ratio
# Min.   :  0.3           Length:12430           Length:12430        
# 1st Qu.: 12.3           Class :character       Class :character    
# Median : 16.3           Mode  :character       Mode  :character    
# Mean   : 18.9                                                      
# 3rd Qu.: 22.0                                                      
# Max.   :865.8                                                      
# Overall.Score       Teaching     Research.Environment Research.Quality
# Min.   : 8.223   Min.   : 8.20   Min.   :  0.80       Min.   :  0.70  
# 1st Qu.:21.732   1st Qu.:18.80   1st Qu.: 11.70       1st Qu.: 24.50  
# Median :32.403   Median :24.30   Median : 18.10       Median : 47.45  
# Mean   :35.333   Mean   :28.54   Mean   : 24.11       Mean   : 49.19  
# 3rd Qu.:45.189   3rd Qu.:33.80   3rd Qu.: 30.50       3rd Qu.: 72.97  
# Max.   :98.457   Max.   :99.00   Max.   :100.00       Max.   :100.00  
# Industry.Impact  International.Outlook      Year     
# Min.   :  0.00   Min.   :  7.10        Min.   :2016  
# 1st Qu.: 35.30   1st Qu.: 28.23        1st Qu.:2019  
# Median : 39.50   Median : 43.30        Median :2021  
# Mean   : 46.51   Mean   : 47.60        Mean   :2021  
# 3rd Qu.: 52.20   3rd Qu.: 63.60        3rd Qu.:2023  
# Max.   :100.00   Max.   :100.00        Max.   :2024  
# PART 4 
# Creating histograms for the numerical columns
num_cols5 <- sapply(data5, is.numeric)
hist_data5 <- data5[, num_cols5]

# Print histograms using ggplot for the fifth dataset
for (col_name in names(hist_data5)) {
  print(
    ggplot(data5, aes_string(x = col_name)) + 
      geom_histogram(fill = "lightcoral", color = "darkred", binwidth = 1) +  # Adjust binwidth if needed
      theme_minimal() +
      ggtitle(paste("Histogram of", col_name))
  )
}

# Part 5
# What variables have the strongest affect on the world ranking?
# What is the correlation between female students and high ranking?
# What country has the most top 200 universities?

# My Datasets 3/3
# Dataset 3: Social media popularity (2009 - 2023)
# Path to the CSV file
file_path6 <- "/Users/emilygelchie/Downloads/social_media.csv"
data6 <- read.csv(file_path6, fileEncoding = "ISO-8859-1")

# PART 1 
# Displaying the column variables
print(colnames(data6))

# [1] "Date"                 "Facebook"             "Pinterest"           
# [4] "Twitter"              "StumbleUpon"          "YouTube"             
# [7] "Instagram"            "Tumblr"               "reddit"              
# [10] "VKontakte"            "LinkedIn"             "Google."             
# [13] "Digg"                 "MySpace"              "Fark"                
# [16] "NowPublic"            "iWiW"                 "news.ycombinator.com"
# [19] "Delicious"            "orkut"                "Odnoklassniki"       
# [22] "Vimeo"                "Other"      

# PART 2 
# Checking for missing values (including NaN)
nan_values6 <- sapply(data6, function(x) sum(is.na(x)))
print(nan_values6)

# Date             Facebook            Pinterest              Twitter 
# 0                    0                    0                    0 
# StumbleUpon              YouTube            Instagram               Tumblr 
# 0                    0                    0                    0 
# reddit            VKontakte             LinkedIn              Google. 
# 0                    0                    0                    0 
# Digg              MySpace                 Fark            NowPublic 
# 0                    0                    0                    0 
# iWiW news.ycombinator.com            Delicious                orkut 
# 0                    0                    0                    0 
# Odnoklassniki                Vimeo                Other 
# 0                    0                    0 

# PART 3 
# Running summary analysis of the columns
print(summary(data6))

# Date              Facebook       Pinterest         Twitter      
# Length:178         Min.   : 0.00   Min.   : 0.000   Min.   : 0.000  
# Class :character   1st Qu.:64.68   1st Qu.: 5.105   1st Qu.: 5.518  
# Mode  :character   Median :68.94   Median : 7.320   Median : 6.840  
# Mean   :69.21   Mean   : 7.308   Mean   : 7.267  
# 3rd Qu.:75.53   3rd Qu.:10.685   3rd Qu.: 8.750  
# Max.   :87.83   Max.   :16.960   Max.   :15.480  
# StumbleUpon        YouTube         Instagram          Tumblr      
# Min.   : 0.000   Min.   : 0.000   Min.   : 0.000   Min.   : 0.000  
# 1st Qu.: 0.010   1st Qu.: 1.405   1st Qu.: 0.000   1st Qu.: 0.000  
# Median : 0.385   Median : 3.845   Median : 0.000   Median : 0.415  
# Mean   : 5.177   Mean   : 3.768   Mean   : 2.178   Mean   : 0.979  
# 3rd Qu.: 5.737   3rd Qu.: 5.055   3rd Qu.: 2.400   3rd Qu.: 1.005  
# Max.   :36.790   Max.   :11.040   Max.   :14.320   Max.   :10.250  
# reddit         VKontakte         LinkedIn        Google.       
# Min.   : 0.000   Min.   :0.0000   Min.   :0.000   Min.   :0.00000  
# 1st Qu.: 0.520   1st Qu.:0.1000   1st Qu.:0.130   1st Qu.:0.00000  
# Median : 0.915   Median :0.1450   Median :0.220   Median :0.00000  
# Mean   : 1.667   Mean   :0.1822   Mean   :0.224   Mean   :0.09803  
# 3rd Qu.: 2.618   3rd Qu.:0.2400   3rd Qu.:0.300   3rd Qu.:0.21000  
# Max.   :12.130   Max.   :0.8800   Max.   :0.550   Max.   :0.41000  
# Digg           MySpace             Fark           NowPublic      
# Min.   :0.0000   Min.   : 0.0000   Min.   :0.00000   Min.   :0.00000  
# 1st Qu.:0.0000   1st Qu.: 0.0000   1st Qu.:0.02000   1st Qu.:0.00000  
# Median :0.0000   Median : 0.0000   Median :0.03000   Median :0.00000  
# Mean   :0.3769   Mean   : 0.3847   Mean   :0.06899   Mean   :0.07882  
# 3rd Qu.:0.0400   3rd Qu.: 0.0100   3rd Qu.:0.08750   3rd Qu.:0.07750  
# Max.   :7.3400   Max.   :14.8100   Max.   :0.66000   Max.   :1.43000  
# iWiW        news.ycombinator.com   Delicious           orkut        
# Min.   :0.0000   Min.   :0.00000      Min.   :0.00000   Min.   :0.00000  
# 1st Qu.:0.0000   1st Qu.:0.01000      1st Qu.:0.00000   1st Qu.:0.00000  
# Median :0.0000   Median :0.02000      Median :0.00000   Median :0.00000  
# Mean   :0.1258   Mean   :0.02994      Mean   :0.05006   Mean   :0.08427  
# 3rd Qu.:0.0000   3rd Qu.:0.05000      3rd Qu.:0.02000   3rd Qu.:0.01000  
# Max.   :2.3500   Max.   :0.14000      Max.   :0.58000   Max.   :1.77000  
# Odnoklassniki         Vimeo             Other       
# Min.   :0.00000   Min.   :0.00000   Min.   :0.0000  
# 1st Qu.:0.00000   1st Qu.:0.00000   1st Qu.:0.0000  
# Median :0.00000   Median :0.00500   Median :0.0100  
# Mean   :0.01354   Mean   :0.01579   Mean   :0.1497  
# 3rd Qu.:0.00000   3rd Qu.:0.02000   3rd Qu.:0.0700  
# Max.   :0.17000   Max.   :0.52000   Max.   :2.8300  

# PART 4 
# Creating histograms for the numerical columns
num_cols6 <- sapply(data6, is.numeric)
hist_data6 <- data6[, num_cols6]

# Print histograms using ggplot 
for (col_name in names(hist_data6)) {
  print(
    ggplot(data6, aes_string(x = col_name)) + 
      geom_histogram(fill = "lightskyblue", color = "darkblue", binwidth = 1) +  # Adjust binwidth if needed
      theme_minimal() +
      ggtitle(paste("Histogram of", col_name))
  )
}

# Part 5
# What is the most popular social media platform by year?
# How long do social media platforms stay popular?
# What was the most popular social media platform in 2009? Is it still popular in 2023?