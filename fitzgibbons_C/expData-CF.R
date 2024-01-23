install.packages("tidyverse")
install.packages("ggplot2")
library(tidyverse)
library(ggplot2)
install.packages("Rtools")
library(Rtools)
install.packages("readxl")
library(readxl)
install.packages("caret", dependencies = TRUE)
library(caret)
install.packages("openxlsx")
library(openxlsx)
install.packages("xlsx")
library(xlsx)


###Boston 22 Dataset
Bos22 <- read.csv("/Users/colinfitzgibbons/Downloads/Boston_22_311s.csv")
View(Bos22)
print(colnames(Bos22))
# [1] "X_id"                           "case_enquiry_id"               
# [3] "open_dt"                        "sla_target_dt"                 
# [5] "closed_dt"                      "on_time"                       
# [7] "case_status"                    "closure_reason"                
# [9] "case_title"                     "subject"                       
# [11] "reason"                         "type"                          
# [13] "queue"                          "department"                    
# [15] "submitted_photo"                "closed_photo"                  
# [17] "location"                       "fire_district"                 
# [19] "pwd_district"                   "city_council_district"         
# [21] "police_district"                "neighborhood"                  
# [23] "neighborhood_services_district" "ward"                          
# [25] "precinct"                       "location_street_name"          
# [27] "location_zipcode"               "latitude"                      
# [29] "longitude"                      "geom_4326"                     
# [31] "source"                        
# > 

Bos22 <- na.omit(Bos22)
Bos22 <- distinct(Bos22)

summary(Bos22)
# X_id        case_enquiry_id      open_dt          sla_target_dt       closed_dt        
# Min.   :     2   Min.   :1.01e+11   Length:211074      Length:211074      Length:211074     
# 1st Qu.: 70785   1st Qu.:1.01e+11   Class :character   Class :character   Class :character  
# Median :140216   Median :1.01e+11   Mode  :character   Mode  :character   Mode  :character  
# Mean   :139239   Mean   :1.01e+11                                                           
# 3rd Qu.:207751   3rd Qu.:1.01e+11                                                           
# Max.   :276599   Max.   :1.01e+11                                                           
# on_time          case_status        closure_reason      case_title       
# Length:211074      Length:211074      Length:211074      Length:211074     
# Class :character   Class :character   Class :character   Class :character  
# Mode  :character   Mode  :character   Mode  :character   Mode  :character  
# 
# 
# 
# subject             reason              type              queue          
# Length:211074      Length:211074      Length:211074      Length:211074     
# Class :character   Class :character   Class :character   Class :character  
# Mode  :character   Mode  :character   Mode  :character   Mode  :character  
# 
# 
# 
# department        submitted_photo    closed_photo         location         fire_district   
# Length:211074      Length:211074      Length:211074      Length:211074      Min.   : 1.000  
# Class :character   Class :character   Class :character   Class :character   1st Qu.: 4.000  
# Mode  :character   Mode  :character   Mode  :character   Mode  :character   Median : 7.000  
# Mean   : 6.693  
# 3rd Qu.: 9.000  
# Max.   :12.000  
# pwd_district       city_council_district police_district    neighborhood      
# Length:211074      Min.   :0.000         Length:211074      Length:211074     
# Class :character   1st Qu.:2.000         Class :character   Class :character  
# Mode  :character   Median :4.000         Mode  :character   Mode  :character  
# Mean   :4.443                                              
# 3rd Qu.:7.000                                              
# Max.   :9.000                                              
# neighborhood_services_district     ward             precinct         location_street_name
# Min.   : 0.00                  Length:211074      Length:211074      Length:211074       
# 1st Qu.: 5.00                  Class :character   Class :character   Class :character    
# Median : 9.00                  Mode  :character   Mode  :character   Mode  :character    
# Mean   : 8.49                                                                            
# 3rd Qu.:13.00                                                                            
# Max.   :15.00                                                                            
# location_zipcode    latitude       longitude       geom_4326            source         
# Min.   :2108     Min.   :42.23   Min.   :-71.19   Length:211074      Length:211074     
# 1st Qu.:2119     1st Qu.:42.30   1st Qu.:-71.11   Class :character   Class :character  
# Median :2125     Median :42.33   Median :-71.07   Mode  :character   Mode  :character  
# Mean   :2127     Mean   :42.33   Mean   :-71.08                                        
# 3rd Qu.:2130     3rd Qu.:42.35   3rd Qu.:-71.06                                        
# Max.   :2467     Max.   :42.40   Max.   :-70.99 

#What Neighborhoods see the most tickets?
##Which neighborhood sees the most parking related tickets?
###Which neighborhood sees the the most non-parking related incidients?



###McDonald's Reviews
mcd <- read.csv("/Users/colinfitzgibbons/Downloads/McDonald_s_Reviews.csv")
View(mcd)
print(colnames(mcd))
# [1] "reviewer_id"   "store_name"    "category"      "store_address" "latitude"     
# [6] "longitude"     "rating_count"  "review_time"   "review"        "rating"  
Mcd <- na.omit(mcd)
Mcd <- distinct(Mcd)

summary(Mcd)
# reviewer_id     store_name          category         store_address         latitude    
# Min.   :    1   Length:32736       Length:32736       Length:32736       Min.   :25.79  
# 1st Qu.: 8185   Class :character   Class :character   Class :character   1st Qu.:28.66  
# Median :16368   Mode  :character   Mode  :character   Mode  :character   Median :33.93  
# Mean   :16581                                                            Mean   :34.44  
# 3rd Qu.:25202                                                            3rd Qu.:40.73  
# Max.   :33396                                                            Max.   :44.98  
# longitude       rating_count       review_time           review         
# Min.   :-122.00   Length:32736       Length:32736       Length:32736      
# 1st Qu.: -97.79   Class :character   Class :character   Class :character  
# Median : -81.47   Mode  :character   Mode  :character   Mode  :character  
# Mean   : -90.65                                                           
# 3rd Qu.: -75.40                                                           
# Max.   : -73.46                                                           
# rating         
# Length:32736      
# Class :character  
# Mode  :character 

Mcd$rating <- as.numeric(Mcd$rating)
hist(Mcd$rating, 
     main = "Star count",
     xlab = "Number of Stars", 
     ylab = "Frequency", 
     col = "lightblue", 
     border = "black")
)

#Which McDonalds in the dataset has the most 1 or 5 star reviews?
# Is there a correlation between good or bad reviews and location?

###Shopping Trends
shop <- read.csv("/Users/colinfitzgibbons/Downloads/shopping_trends_updated.csv")
View(shop)
print(colnames(shop))
# [1] "Customer.ID"            "Age"                    "Gender"                
# [4] "Item.Purchased"         "Category"               "Purchase.Amount..USD." 
# [7] "Location"               "Size"                   "Color"                 
# [10] "Season"                 "Review.Rating"          "Subscription.Status"   
# [13] "Shipping.Type"          "Discount.Applied"       "Promo.Code.Used"       
# [16] "Previous.Purchases"     "Payment.Method"         "Frequency.of.Purchases"
sp <- na.omit(shop)
sp <- distinct(sp)

summary(sp)
# Customer.ID          Age           Gender          Item.Purchased       Category        
# Min.   :   1.0   Min.   :18.00   Length:3900        Length:3900        Length:3900       
# 1st Qu.: 975.8   1st Qu.:31.00   Class :character   Class :character   Class :character  
# Median :1950.5   Median :44.00   Mode  :character   Mode  :character   Mode  :character  
# Mean   :1950.5   Mean   :44.07                                                           
# 3rd Qu.:2925.2   3rd Qu.:57.00                                                           
# Max.   :3900.0   Max.   :70.00                                                           
# Purchase.Amount..USD.   Location             Size              Color          
# Min.   : 20.00        Length:3900        Length:3900        Length:3900       
# 1st Qu.: 39.00        Class :character   Class :character   Class :character  
# Median : 60.00        Mode  :character   Mode  :character   Mode  :character  
# Mean   : 59.76                                                                
# 3rd Qu.: 81.00                                                                
# Max.   :100.00                                                                
# Season          Review.Rating  Subscription.Status Shipping.Type      Discount.Applied  
# Length:3900        Min.   :2.50   Length:3900         Length:3900        Length:3900       
# Class :character   1st Qu.:3.10   Class :character    Class :character   Class :character  
# Mode  :character   Median :3.70   Mode  :character    Mode  :character   Mode  :character  
# Mean   :3.75                                                            
# 3rd Qu.:4.40                                                            
# Max.   :5.00                                                            
# Promo.Code.Used    Previous.Purchases Payment.Method     Frequency.of.Purchases
# Length:3900        Min.   : 1.00      Length:3900        Length:3900           
# Class :character   1st Qu.:13.00      Class :character   Class :character      
# Mode  :character   Median :25.00      Mode  :character   Mode  :character      
# Mean   :25.35                                               
# 3rd Qu.:38.00                                               
# Max.   :50.00  

View(sp)

sp$category <- as.numeric(sp$category)
hist(sp$category, 
     main = "Star count",
     xlab = "Number of Stars", 
     ylab = "Frequency", 
     col = "lightblue", 
     border = "black")

