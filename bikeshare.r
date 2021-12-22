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

  # ------------ Statistics for Viz 1 ---------
  # Create pre-tables
  chicago <- bikeshare %>%
            group_by(Start.Hour, City) %>%
            summarize(Chicago = n()) %>%
            filter(City == "Chicago")

  newyork <- bikeshare %>%
    group_by(Start.Hour, City) %>%
    summarize("New York City" = n()) %>%
    filter(City == "New York City")

  washington <- bikeshare %>%
    group_by(Start.Hour, City) %>%
    summarize("Washington" = n()) %>%
    filter(City == "Washington")

  overall <- bikeshare %>%
    group_by(Start.Hour) %>%
    summarize(all_Cities = n())

  # Join pre-tables
  stats_viz_1 = chicago %>% left_join(newyork, by = "Start.Hour")
  stats_viz_1 = stats_viz_1 %>% left_join(washington, by = "Start.Hour")
  stats_viz_1 = stats_viz_1 %>% left_join(overall, by = "Start.Hour")

  # removing unnecessary column
  stats_viz_1 <- select(stats_viz_1, -c(City.x, City.y, City))

  # Add additional column and calculate mean value for each hour
  stats_viz_1 <- stats_viz_1 %>%
    rowwise() %>%
    mutate(AVG_per_hour = mean(c(Chicago, `New York City`, Washington)))
  stats_viz_1
  # ---------- End of Statistics for Visualization 1 -------

  # -----------Start of Statistics for Visualization 2 ------
# Overall average (6 months) for each city and for all cities
# Create pre-calculation
chi_6months_avg <- bikeshare %>%
  filter(City == "Chicago") %>%
  summarize(chi_6months_avg = mean(Trip.Duration))

nyc_6months_avg <- bikeshare %>%
  filter(City == "New York City") %>%
  summarize(nyc_6months_avg = mean(Trip.Duration))

was_6months_avg <- bikeshare %>%
  filter(City == "Washington") %>%
  summarize(was_6months_avg = mean(Trip.Duration))

all_6months_avg <- bikeshare %>%
  summarize(all_6months_avg = mean(Trip.Duration))

# Combine pre-calculations
AVG_Overview_6months <- chi_6months_avg
AVG_Overview_6months <- AVG_Overview_6months %>%
  rowwise() %>%
  mutate(nyc_6months_avg)
AVG_Overview_6months <- AVG_Overview_6months %>%
  rowwise() %>%
  mutate(was_6months_avg)
AVG_Overview_6months <- AVG_Overview_6months %>%
  rowwise() %>%
  mutate(all_6months_avg)
AVG_Overview_6months
# ---------- End of Statistics for Visualization 2 -----

# -----------Start of Statistics for Visualization 3 -----
# Average number of Bike rides per week day for all Cities together
avg_per_weekday <- bikeshare %>%
  group_by(Start.Day) %>%
  summarize(Average_Rides_per_Weekday = mean(n()))
avg_per_weekday <- avg_per_weekday %>%
  arrange(desc(Average_per_Weekday))
avg_per_weekday
# ---------- End of Statistics for Visualization 3 -----
