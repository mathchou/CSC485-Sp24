Bos22 <- read.csv("Boston_22_311s.csv")
Bos23 <- read.csv("Boston_23_311s.csv")
GWR <- read.csv("GlobalWeatherRepository.csv")
HR <- read.csv("HR_Analytics.csv")
Netflix <- read.csv("Netflix-most-popular.csv")
NYHouse <- read.csv("NY-House-Dataset.csv")
salary <- read.csv("salary_data_cleaned.csv")
shopping <- read.csv("shopping_trends_updated.csv")
spotify <- read.csv("spotify-2023.csv")


#Bos
Bos <- rbind(Bos22[2:31], Bos23)
summary(Bos)

ggplot(Bos, aes(fire_district)) + geom_histogram(bins = 12)
ggplot(Bos, aes(city_council_district)) + geom_histogram(bins = 9)

#GWR
summary(GWR)

ggplot(GWR, aes(temperature_fahrenheit)) + geom_histogram()
ggplot(GWR, aes(wind_mph)) + geom_histogram()

#HR
summary(HR)

ggplot(HR, aes(DailyRate)) + geom_histogram()
ggplot(HR, aes(YearsAtCompany)) + geom_histogram(bins = 41)
ggplot(HR, aes(MonthlyIncome)) + geom_histogram(bins = 41)

#Netflix
summary(Netflix)

cor(Netflix$runtime, Netflix$hours_viewed_first_91_days)

ggplot(Netflix, aes(hours_viewed_first_91_days)) + geom_histogram()
ggplot(Netflix, aes(runtime)) + geom_histogram(bins = 14)

#Shopping
summary(shopping)

ggplot(shopping, aes(Age)) + geom_histogram(bins = 53)
ggplot(shopping, aes(Purchase.Amount..USD.)) + geom_histogram(bins = 18)
