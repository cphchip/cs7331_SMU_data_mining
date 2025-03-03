library("tidyverse")
library("ggplot2")

#' Per the data website https://www.google.com/covid19/mobility/index.html, the 
#' mobility data "reports charted movement trends over time by geography, 
#' across different categories of places such as retail and recreation, 
#' groceries and pharmacies, parks, transit stations, workplaces, and 
#' residential."

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

# Function to filter down to data of interest and drop unnecessary features
filter_data <- function(data, interest) {
  data_of_interest <- data %>%
    select(
      country_region, 
      sub_region_1,
      date, 
      retail_and_recreation_percent_change_from_baseline,
      parks_percent_change_from_baseline,
      transit_stations_percent_change_from_baseline,
      workplaces_percent_change_from_baseline,
      residential_percent_change_from_baseline
    ) %>% 
    filter(country_region == interest) %>%
    drop_na(sub_region_1)
    
  return(data_of_interest)
}




sweden_data <- filter_data(mobility_df, 'Sweden') %>%
  select(-sub_region_1)

# Since we're comparing Texas to Sweden, I'll just call the country Texas here
texas_data <- filter_data(mobility_df, 'United States') %>%
  mutate(country_region = 'Texas') %>%
  select(-sub_region_1)





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
        ) %>% ungroup()
      return(new_df)
}



# mobility_df_rollup <- summarize_df(mobility_df)
# dim(mobility_df_rollup)
texas_data <- summarize_df(texas_data)
sweden_data <- summarize_df(sweden_data)

# data_of_interet = summarize_df(data_of_interest)

# Capture the features to plot for easy reference
features_to_plot <- c(
  "avg_retail", 
  "avg_parks",
  "avg_transit",
  "avg_workplaces",
  "avg_residential"
  )

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
sweden_data <- left_join(
  sweden_data, 
  population_df, 
  by = c('country_region' = 'Country/Territory')
  )
head(sweden_data)

#' Since my dataset for population is by country, I'll use another data source
#' to get the 2020 populataion of Texas. A census was taken in 2020, so I'll
#' use https://www.census.gov/data/tables/2020/dec/2020-apportionment-data.html.
#' Texas had a population of 29,183,290.

texas_data <- mutate(texas_data, '2020 Population' = 29183290)

# Now we can join the back together
data_of_interest <- bind_rows(texas_data, sweden_data)

country_plots <- function(df) { # ChatGPT assisted
  # Filter dataframe for the selected country
  # country_df <- df %>% filter(country_region == country)
  
  # Get numeric column names
  numeric_features <- df %>%
    select(features_to_plot) %>%
    colnames()
  
  # Iterate over numeric feature names
  for (feature in numeric_features) {
    plot <- ggplot(df, aes(x = date, y = .data[[feature]], color=country_region)) +
    # geom_line() +
    geom_smooth(se = FALSE) +
      scale_color_manual(values = c("Texas" = "orange", "Sweden" = "darkblue")) +
    labs(
      title = paste("Texas vs Sweden - ", feature),
      x = "Date",
      y = "Percent Change",
      color = "Location"
    )
    print(plot)  # Ensure plots are displayed when running in a function
    }
}

country_plots(data_of_interest)



# Reshape data to long format
data_of_interest_long <- data_of_interest %>%
  pivot_longer(cols = all_of(features_to_plot), 
               names_to = "Feature", 
               values_to = "Value")

# Create the faceted plot
plot <- ggplot(data_of_interest_long, aes(x = date, y = Value, color = country_region)) +
  geom_smooth(se = FALSE) +
  scale_color_manual(values = c("Texas" = "orange", "Sweden" = "darkblue")) +
  labs(
    title = "Texas vs Sweden - Mobility Trends",
    x = "Date",
    y = "Percent Change",
    color = "Location"
  ) +
  facet_wrap(~ Feature, scales = "free_y") +  # Create separate plots for each feature
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotate x-axis labels

print(plot)  # Ensure the plot is displayed


