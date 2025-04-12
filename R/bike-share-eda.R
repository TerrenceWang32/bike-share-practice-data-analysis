# loading the packages
library(tidyverse)
library(tidymodels)
# color palettes
install.packages('viridis') 
library('viridis')
install.packages('psych')
library('psych')
install.packages('rmarkdown')
library('rmarkdown')

# the following code prevents scientific figures on the chart, for instance
options(scipen = 999)

# loading data
trip_202403_202502 <- read_csv('../data/processed/trip-202403-202502.csv')

# briefly inspect the data
glimpse(trip_202403_202502)

describe(trip_202403_202502)
# discover the missing data on the following variables
# start_station_name, start_station_id, 18.68%
# end_station_name, end_station_id, 19.20%
# end_lat, end_lng, 0.12%
# so a significant missing data on start_station and end_station

# where are the missing data in terms of user type and type of ride
# start_station_name
trip_202403_202502 |> 
  filter(is.na(start_station_name)) |> 
  select(rideable_type, member_casual) |> 
  ggplot(aes(rideable_type, fill = member_casual)) +
  geom_bar() +
  scale_fill_viridis(discrete = TRUE, option = 'viridis') +
  facet_wrap(~member_casual) +
  labs(
    title = 'Lots of missing data on Electric Rides',
    subtitle = 'Missing data - start_station_name',
    x = 'Type of rides',
    y = 'Frequency',
    caption = 'Data between Mar 2024 to Feb 2025'
  )
# Essentially all the missing data are from Electric

# end_station_name
trip_202403_202502 |> 
  filter(is.na(end_station_name)) |> 
  select(rideable_type, member_casual) |> 
  ggplot(aes(rideable_type, fill = member_casual)) +
  geom_bar() +
  scale_fill_viridis(discrete = TRUE, option = 'viridis') +
  facet_wrap(~member_casual) +
  labs(
    title = 'Majority of missing data on Electric Rides',
    subtitle = 'Missing data - end_station_name',
    x = 'Type of rides',
    y = 'Frequency',
    caption = 'Data between Mar 2024 to Feb 2025'
  )
# The majority of the missing data are from electric rides

# end_lat
trip_202403_202502 |> 
  filter(is.na(end_lat)) |> 
  select(rideable_type, member_casual) |> 
  ggplot(aes(rideable_type, fill = member_casual)) +
  geom_bar() +
  scale_fill_viridis(discrete = TRUE, option = 'viridis') +
  facet_wrap(~member_casual) +
  labs(
    title = 'Almost all the missing data are from traditional bikes',
    subtitle = 'Missing data - end_lat',
    x = 'Type of rides',
    y = 'Frequency',
    caption = 'Data between Mar 2024 to Feb 2025'
  )

# which type of ride different type of user prefer the most
trip_202403_202502 |> 
  select(rideable_type, member_casual) |> 
  group_by(member_casual) |> 
  count(rideable_type) |> 
  ggplot(aes(rideable_type, n, fill = member_casual)) +
  geom_col() +
  scale_fill_viridis(discrete = TRUE, option = 'viridis') +
  facet_wrap(~member_casual) +
  labs(
    title = 'Preference on Electric rides is slightly higher',
    subtitle = 'In general the preference is balanced between classic and electric',
    x = 'Type of rides',
    y = 'Frequency',
    caption = 'Data between Mar 2024 to Feb 2025'
  )

# Popular Start Station
# Casual
trip_202403_202502 |> 
  filter(!is.na(start_station_name), member_casual == 'casual') |> 
  select(start_station_name) |> 
  count(start_station_name) |> 
  slice_max(n, n=20) |> 
  ggplot(aes(fct_reorder(start_station_name,n),n)) +
  geom_col(fill = "#440154") +
  coord_flip() +
  geom_hline(yintercept = 20000, linetype = 'dashed', colour = '#21918c') +
  annotate('rect', 
           xmin = 14.5, 
           xmax = 20.5, 
           ymin = -500, 
           ymax = 51000, 
           colour = '#21918c', 
           alpha = 0) +
  labs(
    title = 'Top 20 Start Stations for Casual Users',
    x = 'Station Name',
    y = 'Frequency',
    caption = 'Data between Mar 2024 to Feb 2025'
  )

# Members
trip_202403_202502 |> 
  filter(!is.na(start_station_name), member_casual == 'member') |> 
  select(start_station_name) |> 
  count(start_station_name) |> 
  slice_max(n, n=20) |> 
  ggplot(aes(fct_reorder(start_station_name,n),n)) +
  geom_col(fill = "#fde725") +
  coord_flip() +
  geom_hline(yintercept = 20000, linetype = 'dashed', colour = '#21918c') +
  labs(
    title = 'Top 20 Start Stations for Members',
    x = 'Station Name',
    y = 'Frequency',
    caption = 'Data between Mar 2024 to Feb 2025'
  )

# Popular End Station
# Casual
trip_202403_202502 |> 
  filter(!is.na(end_station_name), member_casual == 'casual') |> 
  select(end_station_name) |> 
  count(end_station_name) |> 
  slice_max(n, n=20) |> 
  ggplot(aes(fct_reorder(end_station_name, n), n)) +
  geom_col(fill = '#440154') +
  coord_flip() +
  geom_hline(yintercept = 20000, linetype = 'dashed', colour = '#21918c') +
  annotate('rect', 
           xmin = 15.5, 
           xmax = 20.5, 
           ymin = -500, 
           ymax = 55000, 
           colour = '#21918c', 
           alpha = 0) +
  labs(
    title = 'Top 20 End Stations for Casual Users',
    x = 'Station Name',
    y = 'Frequency',
    caption = 'Data between Mar 2024 to Feb 2025'
  )

# Members
trip_202403_202502 |> 
  filter(!is.na(end_station_name), member_casual == 'member') |> 
  select(end_station_name) |> 
  count(end_station_name) |> 
  slice_max(n, n=20) |> 
  ggplot(aes(fct_reorder(end_station_name, n), n)) +
  geom_col(fill = '#fde725') +
  coord_flip() +
  geom_hline(yintercept = 20000, linetype = 'dashed', colour = '#21918c') +
  labs(
    title = 'Top 20 End Stations for Members',
    x = 'Station Name',
    y = 'Frequency',
    caption = 'Data between Mar 2024 to Feb 2025'
  )

# Monthly use different members vs casual
trip_202403_202502 |> 
  select(month, member_casual) |> 
  ggplot(aes(month, fill = member_casual)) +
  geom_bar() +
  facet_wrap(~member_casual) +
  scale_x_continuous(breaks = seq(1,12,1)) +
  scale_fill_viridis(discrete = TRUE, option = 'viridis') +
  labs(
    title = 'Casual users tend to use the service more from Jun to Sep',
    x = 'Month',
    y = 'Frequency',
    caption = 'Data between Mar 2024 to Feb 2025'
  )

# Week day analysis
trip_202403_202502 |> 
  ggplot(aes(wk, fill = member_casual)) +
  geom_bar() +
  facet_wrap(~member_casual) +
  scale_fill_viridis(discrete = TRUE, option = 'viridis') +
  labs(
    title = 'Weekend for the Casuals, while members use them ever so often',
    x = 'Day of the week',
    y = 'Frequency',
    caption = 'Data between Mar 2024 to Feb 2025'
  )

# time of the day analysis
# weekdays
trip_202403_202502 |> 
  filter(wk %in% c('Mon', 'Tue', 'Wed', 'Thu', 'Fri')) |> 
  ggplot(aes(hour, fill = member_casual)) +
  geom_bar() +
  facet_wrap(~ member_casual) +
  scale_x_continuous(breaks = seq(0,23,1)) +
  theme(axis.text.x = element_text(size = 7)) +
  scale_fill_viridis(discrete = TRUE, option = 'viridis') +
  labs(
    title = 'Causal users use more at 4-6 pm during the week',
    subtitle = 'Weekdays',
    x = 'Hours of a day (24 hours basis)',
    y = 'Frequncy',
    caption = 'Data between Mar 2024 to Feb 2025'
  )

# weekends
trip_202403_202502 |> 
  filter(wk %in% c('Sat', 'Sun')) |> 
  ggplot(aes(hour, fill = member_casual)) +
  geom_bar() +
  facet_wrap(~ member_casual) +
  scale_x_continuous(breaks = seq(0,23,1)) +
  theme(axis.text.x = element_text(size = 7)) +
  scale_fill_viridis(discrete = TRUE, option = 'viridis') +
  labs(
    title = 'Causal users are more active in the afternoon during the weekends',
    subtitle = 'Weekends',
    x = 'Hours of a day (24 hours basis)',
    y = 'Frequncy',
    caption = 'Data between Mar 2024 to Feb 2025'
  )

# Casual users months and weeks
trip_202403_202502 |> 
  filter(member_casual == 'casual') |> 
  ggplot(aes(wk)) +
  geom_bar(fill = '#440154') +
  facet_wrap(~ month, ncol = 4) +
  labs(
    title = 'Causal users are most active from May to Sep',
    x = 'Week days in different months',
    y = 'Frequency',
    caption = 'Data between Mar 2024 to Feb 2025'
  )

# members months and weeks
trip_202403_202502 |> 
  filter(member_casual == 'member') |> 
  ggplot(aes(wk)) +
  geom_bar(fill = '#fde725') +
  facet_wrap(~ month, ncol = 4) +
  labs(
    title = 'Members only use it less in Winter',
    x = 'Week days in different months',
    y = 'Frequency',
    caption = 'Data between Mar 2024 to Feb 2025'
  )

trip_202403_202502 |> 
  filter(member_casual == 'casual') |> 
  ggplot(aes(wk)) +
  geom_bar(fill = '#440154') +
  facet_grid(vars(rideable_type), vars(month)) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  labs(
    title = 'Casual users stick to bikes',
    subtitle = 'Some use of scooter in Sep',
    x = 'Days of Week Mon - Sun',
    y = 'Frequency',
    caption = 'Data between Mar 2024 to Feb 2025'
  )

# Summary statistics:
trip_202403_202502 |> 
  group_by(member_casual)|> 
  summarise(mean = mean(duration_min), 
            median = median(duration_min),
            max_d = max(duration_min),
            min_d = min(duration_min),
            mode_d = mode(duration_min))

trip_202403_202502 |> 
  group_by(member_casual) |> 
  count(wk) |> 
  arrange(desc(n))

# Duration boxplot
trip_202403_202502 |> 
  filter(duration_min >= 0 & duration_min < 30) |> 
  ggplot(aes(member_casual, duration_min, fill = member_casual)) +
  geom_boxplot(colour = '#5ec962') + 
  scale_fill_viridis(discrete = TRUE, option = 'viridis') +
  labs(
    title = 'Duration Boxplot Causal VS Member',
    x = 'Type of riders',
    y = 'Duration in  minutes',
    caption = 'Data from Mar 2024 to Feb 2025'
  )
