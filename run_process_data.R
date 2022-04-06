# processing data ---------------------------------------------------------
library("here")
library("dplyr")
library("lubridate")


# 1. convert daliy vix to monthly vix 

# load data 
y_object <- read.csv(here::here("data", "VIX_original.csv"))

# select date and close price
y_object <- y_object[c("DATE", "CLOSE")]

# change the column names
names(y_object) <- c("date", "value")

# set to date class
y_object$date <- as.Date(y_object$date, "%m/%d/%Y")

y_object <- y_object %>% 
  group_by(date=floor_date(date, "month")) %>% 
  summarize(value=mean(value))

# save 
write.csv(y_object, here::here("data", "VIX.csv"), row.names = FALSE)


