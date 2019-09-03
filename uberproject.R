library(dplyr)
data14 <- bind_rows(uber.raw.data.may14, uber.raw.data.jun14, uber.raw.data.jul14, uber.raw.data.aug14, uber.raw.data.sep14)
summary(data14)
install.packages("VIM")
library(VIM)
aggr(data14)
install.packages("lubridate")
library(lubridate)
data14$Date.Time <- mdy_hms(data14$Date.Time)
data14$Year <- factor(year(data14$Date.Time))
data14$Month <- factor(month(data14$Date.Time))
data14$Day <- factor(day(data14$Date.Time))
data14$Weekday <- factor(wday(data14$Date.Time))
data14$Hour <- factor(hour(data14$Date.Time))
data14$Minute <- factor(minute(data14$Date.Time))
data14$Second <- factor(second(data14$Date.Time))
data14$Month
head(data14, n=10)
set.seed(20)
clusters <- kmeans(data14[,2:3], 5)
data14$Borough <- as.factor(clusters$cluster)
str(clusters)
install.packages("ggmap")
library(ggmap)
NYCMap <- get_map("New York", zoom = 10)
ggmap(NYCMap) + geom_point(aes(x = Lon[], y = Lat[], colour = as.factor(Borough)),data = data14) +
  ggtitle("NYC Boroughs using KMean")
install.packages("DT")
library(DT)
data14$Month <- as.double(data14$Month)
month_borough_14 <- count_(data14, vars = c('Month', 'Borough'), sort = TRUE) %>% 
  arrange(Month, Borough)
datatable(month_borough_14)
library(dplyr)
monthly_growth <- month_borough_14 %>%
  mutate(Date = paste("04", Month)) %>%
  ggplot(aes(Month, n, colour = Borough)) + geom_line() +
  ggtitle("Uber Monthly Growth - 2014")
monthly_growth