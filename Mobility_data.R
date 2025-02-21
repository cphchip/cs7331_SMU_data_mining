library("tidyverse")

library("ggplot2")
library("ggrepel")
library("ggcorrplot")
library("DT")

df <- read_csv("Global_Mobility_Report.csv")

head(df)

unique(df$country_region)
sort(table(df$country_region), decreasing = TRUE)

# note the %>% is the pipeline operator, but I'm only doing one function here 
# so probably not needed
# The Sub-country specific data may be of value, but there's a ton of it and 
# it's complicating processing so I'm going to drop it for now

filtered_df <- df %>%
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
head(filtered_df)
country <- filtered_df %>%
  filter(country_region == 'United States')

# view(country)

# Shows the unique number of occurrences of the dates found in the dataframe
# There are a ton of entries for each date depending on the country.
sort(table(country$date), decreasing = TRUE)


# Now the trick is how do we combine these to be single lines per country
# They're all percentages, so averaging might be appropriate.
# Each has a different date though...something to consider

# Let's group by country and then by date. We're going to summarize some of the
# numeric data by averaging the percentage values. The goal here is to roll 
# up the data by country so it's not so much to look at. This makes more sense
# for smaller countries, admittedly. US data might look strange as the pandemic
# hit at different times across the country and population densities.
summarize_df <- function(df) {
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

tmp <- summarize_df(filtered_df)
head(tmp)
country <- "United States"
USA <- tmp %>%
  filter(country_region == country)

ggplot(USA, aes(x = date, y = avg_retail)) + 
  geom_line(color = "blue") +
  labs(
    title = paste("avg retail over time ", country),
       x = "Date",
       y = "Percent Change"
     )
