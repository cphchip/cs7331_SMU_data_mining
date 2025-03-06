library("tidyverse")
library("ggplot2")
library("GGally")

# Import the Texas cases dataset
texas_time_series_df <- read_csv("COVID-19_cases_TX.csv")

texas_plot <- ggplot(texas_time_series_df, aes(x=date, y=confirmed_cases)) +
  geom_point()

print(texas_plot)

texas_clean_df <- texas_time_series_df %>%
  select(
    county_name,
    state,
    date,
    confirmed_cases,
    deaths
  ) %>%
  filter(texas_time_series_df$county_name != 'Statewide Unallocated')

# Import the mobility dataset
mobility3_df <- read_csv("Global_Mobility_Report.csv")

#' In order to enable joining the census data to the mobility data I needed a 
#' lookup table of State name to it's abbreviation. Boy Scouts to the rescue!
#' https://www.scouting.org/resources/los/states/
state_to_postal <- read_csv("state_to_postal_code(Sheet1).csv")
head(state_to_postal)

# Join the State abbreviation dataset to my mobility dataset
mobility3_df <- left_join(
  mobility3_df,
  state_to_postal, 
  by = c('sub_region_1' = 'State')
)


#' The mobility data contains "District of Columbia" which isn't in the census
#' data. Therefore I'm going to drop those instances. I'm also going to pare 
#' back the data to some specific features and drop_NA from sub_region_1 and 
#' sub_region_2.
mobility_3_subset_df <- mobility3_df %>%
  select(
    country_region, 
    sub_region_1,
    sub_region_2,
    date, 
    grocery_and_pharmacy_percent_change_from_baseline,
    retail_and_recreation_percent_change_from_baseline,
    parks_percent_change_from_baseline,
    transit_stations_percent_change_from_baseline,
    workplaces_percent_change_from_baseline,
    residential_percent_change_from_baseline,
    Postal
  ) %>% 
  filter(
    country_region == 'United States',
    sub_region_1 == 'Texas') %>%
  drop_na(sub_region_1, sub_region_2)


#' The join function here needs to be linked to county but also to date. There
#' are more date datapoints in the texas clean data than there are in the mobility
#' 3 subset data. I could aggregate these by months...or weeks?


library(lubridate)


# Aggregate by week (starting on Monday)
date_aggregator <- function(df) {
  df_weekly <- df %>%
    mutate(week = floor_date(date, unit = "week", week_start = 1))
  
  return(df_weekly)
}

weekly_texas_clean_df <- date_aggregator(texas_clean_df)
weekly_mobility_df <- date_aggregator(mobility_3_subset_df)

time_series_df <- left_join(
  mobility_3_subset_df,
  texas_clean_df,
  by = c(
    'sub_region_2' = 'county_name',
    'date' = 'date'
  )
)

time_series_df <- time_series_df %>% drop_na()

time_series_aggregate_df <- time_series_df %>%
    group_by(sub_region_2, date) %>%  # Group by the 'date' column
    summarize(
      avg_retail = mean(retail_and_recreation_percent_change_from_baseline, na.rm = TRUE),
      avg_grocery_and_pharmacy = mean(grocery_and_pharmacy_percent_change_from_baseline, na.rm = TRUE),
      avg_parks = mean(parks_percent_change_from_baseline, na.rm = TRUE),
      avg_transit = mean(transit_stations_percent_change_from_baseline, na.rm = TRUE),
      avg_workplaces = mean(workplaces_percent_change_from_baseline, na.rm = TRUE),
      avg_residential = mean(residential_percent_change_from_baseline, na.rm = TRUE),
      total_confirmed_cases = sum(confirmed_cases, na.rm = TRUE),
      total_deaths = sum(deaths, na.rm = TRUE)
    ) %>%
    ungroup()


# All Texas counties correlation with deaths and confirmed cases
all_tx_counties_corr <- time_series_aggregate_df %>%
  select(
    total_confirmed_cases,
    avg_grocery_and_pharmacy,
    avg_retail,
    avg_parks,
    avg_transit,
    avg_residential,
    avg_workplaces,
    total_deaths
  ) %>%
  as.matrix() %>%
  cor()

ggcorrplot(all_tx_counties_corr)

mobility_variables <- c(
  "avg_retail",
  "avg_grocery_and_pharmacy",
  "avg_parks",
  "avg_transit",
  "avg_residential",
  "avg_workplaces"
)


# 
# time_series_long_df <- time_series_aggregate_df %>%
#   pivot_longer(cols = all_of(mobility_variables), 
#                names_to = "Feature", 
#                values_to = "Percent Change")
# 
# ggplot(time_series_aggregate_df, aes(x = date, y = total_confirmed_cases)) +
#   geom_smooth(se = FALSE)
# 
# plot <- ggplot(time_series_long_df, aes(x = date, y = "Percent Change", color=sub_region_2)) +
#   geom_smooth(se = FALSE) +
# facet_wrap(~ Feature, scales = "free_y") 
# 
# print(plot)
