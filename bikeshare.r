# --------- R Script for bikeshare data analysis ------
# --------- based on Udacity Nanodegree Project -------

# --------- Start Data Preparation section ------------
# load further R packages
library(dplyr)
library (ggplot2)
library(lubridate)

# Define and read the Working Directory
setwd("C:\\Users\\U2150\\Downloads")
getwd()

# Read Data files
ny <- read.csv("new-york-city.csv")
ch <- read.csv("chicago.csv")
wa <- read.csv("washington.csv")

# Add new column City to all files and put respectrive city name in
ny$City <- "New York City"
ch$City <- "Chicago"
wa$City <- "Washington"

# Combine the 3 City files into 1 bikeshare file
bikeshare = ny %>% full_join(ch)
bikeshare = bikeshare %>% full_join(wa)

# Get Date information extracted
bikeshare$Start.Hour <- hour(ymd_hms(bikeshare$Start.Time))
bikeshare$Start.Day <- weekdays(ymd_hms(bikeshare$Start.Time))
bikeshare$Start.Month <- month(ymd_hms(bikeshare$Start.Time))
bikeshare$Start.Week <- week(ymd_hms(bikeshare$Start.Time))
# bikeshare$Start.Month <- month(ymd_hms(bikeshare$Start.Time), label = TRUE)
bikeshare$DayOfYear <- yday(ymd_hms(bikeshare$Start.Time))
# --------- End of Data Preparation section ----

# --------- Start of Visualization section ------
# Visualization 1: Number of Rides per Hour and City
ggplot(aes(x=Start.Hour, fill=City), data=bikeshare) +
  geom_bar(position = "dodge", color = "black") +
  ggtitle("Number of Bike Rides per City and Rental Start Time Hour") +
  labs(x = "Hour of Rental Start Time", y = "Number of Bike Rides for each City") +
  scale_x_continuous(breaks = 0:23)

# Remove unnecessary visualization
# ggplot(aes(x=Start.Hour, fill=City), data=bikeshare) +
#  geom_density(color = "black") +
#  ggtitle("Number of Bike Rides per City and Rental Start Time Hour") +
#  labs(x = "Hour of Rental Start Time", y = "Number of Bike Rides for each City") +
#  facet_wrap("City")


# Visualization 2: Average Rides per City over time

avg_week_city <- bikeshare %>%
  group_by(Start.Week, City) %>%
  summarise(avg_week_city = mean(Trip.Duration))

avg_week <- bikeshare %>%
  group_by(Start.Week) %>%
  summarise(avg_week_total = mean(Trip.Duration))

ggplot() +
  geom_line(data = avg_week_city, aes(x=Start.Week, y=avg_week_city, color = City)) +
  geom_point(data = avg_week, aes(x=Start.Week, y= avg_week_total)) +
  geom_line(data = avg_week, aes(x=Start.Week, y= avg_week_total, linetype = "Weekly Average")) +
  ggtitle("Average Trip Duration per City and Average of all Cities per Week") +
  xlab("Calendar Weeks in 2017") +
  ylab("Average Trip Duration per Week")


# Visualization 3: Number of Rides per City and Week Day

ggplot(bikeshare, aes(x=City, fill=Start.Day)) +
  geom_bar(position = "dodge", color = "black") +
  ggtitle("Overview of Bike Rides per City and Week Day") +
  labs(x = "City", y = "Number of Bike Rides per Week day")
# ----------- End of Visualization section -------
