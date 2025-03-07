library("tidyverse")
library("ggplot2")
library("GGally")

# Import the Texas cases dataset
texas_time_series_df <- read_csv("COVID-19_cases_TX.csv")

#' We'll select a subset of data for the COVID-19_cases_TX dataset, and remove
#' anything that didn't have a county attributed to it.
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


#' To pare down the mobility dataset I'll select a subset of features. I'll 
#' also filter for United States and the state of Texas because that's all
#' my Covid-19 cases datasaet contains. Finally, I'll drop any NA values.
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


#' To make the two datasets join without significant NA's I'll create a function
#' to aggregate the date data at the week level.
library(lubridate)
date_aggregator <- function(df) {
  df_weekly <- df %>%
    mutate(week = floor_date(date, unit = "week", week_start = 1)) %>%
  
  return(df_weekly)
}

weekly_texas_clean_df <- date_aggregator(texas_clean_df)
weekly_mobility_df <- date_aggregator(mobility_3_subset_df)

# Join the two datasets
time_series_df <- left_join(
  mobility_3_subset_df,
  texas_clean_df,
  by = c(
    'sub_region_2' = 'county_name',
    'date' = 'date'
  )
)

# Ensure any lingering NAs are removed
time_series_df <- time_series_df %>% drop_na()

# Create a function to help with aggregating the features by county and date
aggregate_mobility <- function(df) {
  aggregate_df <- df %>%
    group_by(sub_region_2, date) %>%
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
  
  return(aggregate_df)
}

time_series_aggregate_df <- aggregate_mobility(time_series_df)

# We'll start by looking at some correlation data
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

# Vector to store some of our features, will make it easier to refer to them
mobility_variables <- c(
  "avg_retail",
  "avg_grocery_and_pharmacy",
  "avg_parks",
  "avg_transit",
  "avg_residential",
  "avg_workplaces"
)

#' Function to plot mobility data between two counties. This was helpful when 
#' needing to compare various counties to find those of interest without having
#' to repeat code.
plot_county_mobility <- function(county1, county2, df) {
  data_of_interest <- df %>%
    filter(sub_region_2 == county1 | sub_region_2 == county2) %>%
    pivot_longer(
      cols = all_of(mobility_variables),
      names_to = "Feature",
      values_to = "Value"
      )
  
  plot <- ggplot(data_of_interest, aes(x = date, y = Value, color = sub_region_2)) +
    geom_smooth(se = FALSE) +
    scale_color_manual(values = setNames(c("orange", "darkblue"), c(county1, county2))) +
    labs(
      title = paste(county1, "vs", county2, "- Mobility Trends"),
      x = "Date",
      y = "Percent Change",
      color = "Location"
    ) +
    facet_wrap(~ Feature, scales = "free_y") # Create separate plots for each feature
    # theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotate x-axis labels
  
  print(plot)
}

plot_county_mobility("Dallas County", "Harris County", time_series_aggregate_df)
plot_county_mobility("Tarrant County", "Bexar County", time_series_aggregate_df)


#' Similar function for plotting cases and deaths
plot_county_confirmed_cases <- function(county1, county2, df) {
  data_of_interest <- df %>%
    filter(sub_region_2 == county1 | sub_region_2 == county2) %>%
    pivot_longer(
      cols = c(total_confirmed_cases, total_deaths),
      names_to = "Features",
      values_to = "Value"
    )
  
  plot <- ggplot(data_of_interest, aes(x = date, y = Value, color = sub_region_2)) +
    geom_smooth(se = FALSE) +
    scale_color_manual(values = setNames(c("orange", "darkblue"), c(county1, county2))) +
    labs(
      title = paste(county1, "vs", county2, "- Confirmed Cases"),
      x = "Date",
      y = "Count",
      color = "Location"
    ) +
    facet_wrap(~ Features, scales = "free_y")  # Create separate plots for each feature
    # theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotate x-axis labels
  
  print(plot)
}

plot_county_confirmed_cases("Tarrant County", "Bexar County", time_series_aggregate_df)

