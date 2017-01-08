library(ggplot2)
library(reshape2)
library(dplyr)


# Handle non-standard date format
setClass('myDate')
setAs("character","myDate", function(from) as.Date(from, format="%m/%d/%Y"))

revClasses <- c("POSTING.DATE"="myDate")
rev2013 <- read.csv("CedarCity_Revenue_2013.csv", header = TRUE, colClasses = revClasses)
rev2014 <- read.csv("CedarCity_Revenue_2014.csv", header = TRUE, colClasses = revClasses)
rev2015 <- read.csv("CedarCity_Revenue_2015.csv", header = TRUE, colClasses = revClasses)
rev2016 <- read.csv("CedarCity_Revenue_2016.csv", header = TRUE, colClasses = revClasses)

weather1 <- read.csv("CedarCity_Weather1.csv", header = TRUE, colClasses = c("MST"="Date"))
weather2 <- read.csv("CedarCity_Weather2.csv", header = TRUE, colClasses = c("MST"="Date"))
weather3 <- read.csv("CedarCity_Weather3.csv", header = TRUE, colClasses = c("MST"="Date"))
weather4 <- read.csv("CedarCity_Weather4.csv", header = TRUE, colClasses = c("MST"="Date"))
weather5 <- read.csv("CedarCity_Weather5.csv", header = TRUE, colClasses = c("MST"="Date"))

weather <- rbind(weather1, weather2, weather3, weather4, weather5)

rev <- rbind(rev2013, rev2014, rev2015, rev2016)

aquaticCenterFees <- rev %>% 
  filter(FUND1 == "AQUATIC CENTER", CAT2 == "FEES-ADMISSIONS") %>% 
  select(date = POSTING.DATE, AMOUNT) %>%
  group_by(date) %>%
  summarise(fees = sum(AMOUNT) / 100)

meanTemp <- weather %>% 
  select(date = MST, temp = Mean.TemperatureF, precip = PrecipitationIn)

data <- merge(meanTemp, aquaticCenterFees, by="date")
data$week <- as.Date(cut(data$date, breaks = "week", start.on.monday = FALSE))

ggplot(data = data, aes(week , fees)) + 
  geom_bar(stat = "identity") +
  geom_line(data=data, aes(x=week, y=temp), colour="red") +
  #geom_line(data=data, aes(x=date, y=precip), colour="blue") +
  scale_y_continuous(labels = scales::dollar) + 
  scale_x_date(date_breaks = "6 months", date_labels = "%b-%Y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
