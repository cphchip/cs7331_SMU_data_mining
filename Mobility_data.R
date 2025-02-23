library("tidyverse")
library("ggplot2")

# Import the initial mobility dataset
mobility_df <- read_csv("Global_Mobility_Report.csv")
str(mobility_df)

# Convert the chr features to factors
mobility_df <- mobility_df %>% mutate_if(is.character, factor)
dim(mobility_df)

summary(mobility_df)

# There's 14 columns and 3,991,405 instances

# The Sub-country specific data may be of value, but there's a ton of it and 
# it's complicating processing so I'm going to drop it for now
mobility_df <- mobility_df %>%
  select(
    country_region, 
    date, 
    retail_and_recreation_percent_change_from_baseline,
    parks_percent_change_from_baseline,
    transit_stations_percent_change_from_baseline,
    workplaces_percent_change_from_baseline,
    residential_percent_change_from_baseline
  )

# This is a helper df to look at the data in smaller subsets
country <- mobility_df %>%
  filter(country_region == 'Sweden')

# Now the trick is, across all these countries, how do we combine these to be 
# single lines per country. They're all percentages, so averaging might be 
# appropriate. Each has a different date though...something to consider.

# Let's group by country and then by date. We're going to summarize some of the
# numeric data by averaging the percentage values. The goal here is to roll 
# up the data by country so it's not so much to look at. This makes more sense
# for smaller countries, admittedly. US data might look strange as the pandemic
# hit at different times across the country and population densities.
summarize_df <- function(df) { # code help from ChaptGPT
  new_df <- df %>%
      group_by(country_region, date) %>%  # Group by the 'date' column
        summarize(
          avg_retail = mean(retail_and_recreation_percent_change_from_baseline, na.rm = TRUE),
          avg_parks = mean(parks_percent_change_from_baseline, na.rm = TRUE),
          avg_transit = mean(transit_stations_percent_change_from_baseline, na.rm = TRUE),
          avg_workplaces = mean(workplaces_percent_change_from_baseline, na.rm = TRUE),
          avg_residential = mean(residential_percent_change_from_baseline, na.rm = TRUE)
        )
      return(new_df)
}

# Capture the features to plot for easy reference
features_to_plot <- c(
  "avg_retail", 
  "avg_parks",
  "avg_transit",
  "avg_workplaces",
  "avg_residential"
  )


mobility_df_rollup <- summarize_df(filtered_df)
dim(mobility_df_rollup)

# Another piece of data to pull in would be the population of each country.
# I can use a Kaggle dataset 
# (https://www.kaggle.com/datasets/iamsouravbanerjee/world-population-dataset?resource=download)
# to accomplish this
population_df <- read_csv("world_population.csv")
str(population_df)

# Most of my mobility data is from 2020, so I'm only going to use population
# data from that timeframe.
population_df <- population_df %>%
  select(
    'Country/Territory', 
    `2020 Population`
  )

# Join the population dataset to my mobility dataset using the country name
mobility_df_rollup <- left_join(
  mobility_df_rollup, 
  population_df, 
  by = c('country_region' = 'Country/Territory')
  )
head(mobility_df_rollup)

# Grab a random 5 countries to study
# unique_sample <- sample(unique(mobility_df_rollup$country_region),5)
# unique_sample

# This is good, but still a ton of data if I did this for each country. So I'm
# going to try and downselect by random sampling.
# country_subset_df <- filter(mobility_df_rollup, country_region %in% unique_sample)
# head(country_subset_df)

country_plots <- function(country, df) { # ChatGPT assisted
  # Filter dataframe for the selected country
  country_df <- df %>% filter(country_region == country)
  
  # Get numeric column names
  numeric_features <- df %>%
    select(features_to_plot) %>%
    colnames()
  
  # Iterate over numeric feature names
  for (feature in numeric_features) {
    plot <- ggplot(country_df, aes(x = date, y = .data[[feature]])) +
    geom_line() +
    geom_smooth() +
    labs(
      title = paste(country, " - ", feature),
      x = "Date",
      y = "Percent Change"
    )
    print(plot)  # Ensure plots are displayed when running in a function
    }
}

# for (country in unique_sample) {
#   country_plots(country, country_subset_df)
#   }

country_plots("Sweden", mobility_df_rollup)