library(ggplot2)
library(reshape2)
library(dplyr)

# Handle non-standard date format
setClass('myDate')
setAs("character","myDate", function(from) as.Date(from, format="%m/%d/%Y"))

revClasses <- c("POSTING.DATE"="myDate")
rev2016 <- read.csv("CedarCity_Revenue_2016.csv", header = TRUE, colClasses = revClasses)
mean(rev2016$AMOUNT)
filter(rev2016, is.na(AMOUNT))

revByCat1 <- rev2016 %>% group_by(CAT1) %>% summarise(total = sum(AMOUNT))
revByCat2 <- rev2016 %>% group_by(CAT2) %>% summarise(total = sum(AMOUNT))
revByFund <- rev2016 %>% group_by(FUND1) %>% summarise(total = sum(AMOUNT))

ggplot(data = revByCat1, aes(reorder(CAT1, total), total)) + 
  geom_bar(stat="identity") + 
  scale_y_continuous(labels = scales::dollar) + 
  xlab("category") + ylab("Revenues") +
  coord_flip()

ggplot(data = revByCat2, aes(reorder(CAT2, total), total)) + 
  geom_bar(stat="identity") + 
  scale_y_continuous(labels = scales::dollar) + 
  xlab("category") + ylab("Revenues") +
  coord_flip()

ggplot(data = revByFund, aes(reorder(FUND1, total), total)) + 
  geom_bar(stat="identity") + 
  scale_y_continuous(labels = scales::dollar) + 
  xlab("category") + ylab("Revenues") +
  coord_flip()

revByMonth <- rev2016 %>%
  mutate(month = format(POSTING.DATE, "%m"), year = format(POSTING.DATE, "%Y")) %>%
  group_by(month, year) %>%
  summarise(total = sum(AMOUNT))

ggplot(data = rev2016, aes(POSTING.DATE,AMOUNT)) + 
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::dollar) + 
  scale_x_date(date_breaks = "1 months", date_labels = "%b-%Y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

busLicenes <- rev2016 %>%
  filter(CAT2 == "BUSINESS LICENSES") %>%
  select(POSTING.DATE, AMOUNT)

ggplot(data = busLicenes, aes(POSTING.DATE,AMOUNT)) + 
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::dollar) + 
  scale_x_date(date_breaks = "1 months", date_labels = "%b-%Y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


admissionFees <- rev2016 %>%
  filter(CAT2 == "FEES-ADMISSIONS") %>%
  select(POSTING.DATE, AMOUNT)