library("tidyverse")

library("ggplot2")
library("ggrepel")
library("ggcorrplot")
library("DT")

mobility_df <- read_csv("Global_Mobility_Report.csv")
str(mobility_df)

# mobility_df <- mobility_df %>% mutate_if(is.character, factor)
dim(mobility_df)

summary(mobility_df)

# There's 14 columns and 3991405 instances

# unique(df$country_region)
# sort(table(df$country_region), decreasing = TRUE)

# The Sub-country specific data may be of value, but there's a ton of it and 
# it's complicating processing so I'm going to drop it for now
filtered_df <- mobility_df %>%
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
country <- filtered_df %>%
  filter(country_region == 'United States')

# Shows the unique number of occurrences of the dates found in the dataframe
# There are a ton of entries for each date depending on the country.
sort(table(country$date), decreasing = TRUE)


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
        summarise(
          avg_retail = mean(retail_and_recreation_percent_change_from_baseline, na.rm = TRUE),
          avg_parks = mean(parks_percent_change_from_baseline, na.rm = TRUE),
          avg_transit = mean(transit_stations_percent_change_from_baseline, na.rm = TRUE),
          avg_workplaces = mean(workplaces_percent_change_from_baseline, na.rm = TRUE),
          avg_residential = mean(residential_percent_change_from_baseline, na.rm = TRUE)
        )
      return(new_df)
}

mobility_df_rollup <- summarize_df(filtered_df)
str(mobility_df_rollup)

# country <- "United States"
# USA <- mobility_df_rollup %>%
#   filter(country_region == country)
# 
# ggplot(USA, aes(x = date, y = avg_retail)) +
#   geom_line() +
#   geom_smooth() +
#   labs(
#     title = paste("avg retail over time ", country),
#        x = "Date",
#        y = "Percent Change"
#      )

# This is good, but still a ton of data if I did this for each country. So I'm
# going to try and downselect by random sampling.





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

str(population_df)


mobility_df_rollup <- mobility_df_rollup %>%
  mutate(country_region = as.character(country_region))

str(mobility_df_rollup)

mobility_df_rollup <- left_join(
  mobility_df_rollup, 
  population_df, 
  by = c('country_region' = 'Country/Territory')
  )
head(mobility_df_rollup)
