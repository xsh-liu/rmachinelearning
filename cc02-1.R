library(dslabs)
library(dplyr)
library(lubridate)

data("reported_heights")

dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
  filter(date_time >= make_date(2016, 01, 25) & date_time < make_date(2016, 02, 1)) %>%
  mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & between(minute(date_time), 15, 30), "inclass","online")) %>%
  select(sex, type)

y <- factor(dat$sex, c("Female", "Male"))
x <- dat$type

#Q1
fein <- dat %>% filter(sex == "Female" & type == "inclass")
feon <- dat %>% filter(sex == "Female" & type == "online")

inclass <- dat %>% filter(type == "inclass")
online <- dat %>% filter(type == "online")

nrow(fein)/nrow(inclass)
nrow(feon)/nrow(online)

#Q2
y_hat <- ifelse(x == "inclass", "Female", "Male") %>% 
  factor(levels = levels(y))

yaccu <- mean(y_hat == y)

#Q3
table(predicted = y_hat, actual = y)

#Q4
confusionMatrix(data = y_hat, reference = y)
