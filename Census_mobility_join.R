library("tidyverse")
library("ggplot2")
library("GGally")


# Import the initial mobility dataset
census_cases_df <- read_csv("COVID-19_cases_plus_census.csv")
str(census_cases_df)

# Select a subset of variables to examine
census_subset <- census_cases_df %>%
  select(
    state,
    confirmed_cases,
    deaths,
    total_pop,
    male_pop,
    female_pop,
    black_pop,
    white_pop,
    hispanic_pop,
    commuters_by_public_transportation,
    worked_at_home,
    income_per_capita,
    masters_degree,
    bachelors_degree,
    high_school_diploma,
    one_year_more_college
  )

#' Aggregate the data at the US State level. 
census_summary_df <- census_subset %>%
  group_by(state) %>%
  summarize(
    state_total_confirmed_cases = sum(confirmed_cases, na.rm = TRUE),
    state_total_deaths = sum(deaths, na.rm = TRUE),
    state_total_pop = sum(total_pop, na.rm = TRUE),
    state_total_male_pop = sum(male_pop, na.rm = TRUE),
    state_total_female_pop = sum(female_pop, na.rm = TRUE),
    state_total_black_pop = sum(black_pop, na.rm = TRUE),
    state_total_white_pop = sum(white_pop, na.rm = TRUE),
    state_total_hispanic_pop = sum(hispanic_pop, na.rm = TRUE),
    state_total_commuters_by_public_transportation = sum(commuters_by_public_transportation, na.rm =TRUE),
    state_total_worked_at_home = sum(worked_at_home, na.rm = TRUE),
    state_avg_income_per_capita = mean(income_per_capita, na.rm = TRUE),
    state_total_masters_degree = sum(masters_degree, na.rm = TRUE),
    state_total_bachelors_degree = sum(bachelors_degree, na.rm = TRUE),
    state_total_high_school_diploma = sum(high_school_diploma, na.rm = TRUE),
    state_total_one_year_more_college = sum(one_year_more_college, na.rm = TRUE)
) %>% ungroup()

# Import the mobility dataset
mobility2_df <- read_csv("Global_Mobility_Report.csv")

#' In order to enable joining the census data to the mobility data I needed a 
#' lookup table of State name to it's abbreviation. Boy Scouts to the rescue!
#' https://www.scouting.org/resources/los/states/
state_to_postal <- read_csv("state_to_postal_code(Sheet1).csv")
head(state_to_postal)

# Join the State abbreviation dataset to my mobility dataset
mobility2_df <- left_join(
  mobility2_df,
  state_to_postal, 
  by = c('sub_region_1' = 'State')
)

#' The mobility data contains "District of Columbia" which isn't in the census
#' data. Therefore I'm going to drop those instances. I'm also going to pare 
#' back the data to some specific features.
mobility_subset_df <- mobility2_df %>%
  select(
    country_region, 
    sub_region_1,
    date, 
    retail_and_recreation_percent_change_from_baseline,
    parks_percent_change_from_baseline,
    transit_stations_percent_change_from_baseline,
    workplaces_percent_change_from_baseline,
    residential_percent_change_from_baseline,
    Postal
  ) %>% 
  filter(country_region == 'United States', 
         sub_region_1 != 'District of Columbia') %>%
  drop_na(sub_region_1)



#' I may also want to know the end state of all these measurements, so I'll pull
#' that info out here.
mobility_end_state_df <- mobility_subset_df %>%
  group_by(country_region, sub_region_1) %>%
  slice_max(order_by = date, n = 1) %>% # ChatGPT assisted
  ungroup()

# Function to aggregate the mobility data
summarize_mobility <- function(df) { # code help from ChaptGPT
  new_df <- df %>%
    group_by(Postal, date) %>%  # Group by the 'date' column
    summarize(
      state_avg_retail = mean(retail_and_recreation_percent_change_from_baseline, na.rm = TRUE),
      state_avg_parks = mean(parks_percent_change_from_baseline, na.rm = TRUE),
      state_avg_transit = mean(transit_stations_percent_change_from_baseline, na.rm = TRUE),
      state_avg_workplaces = mean(workplaces_percent_change_from_baseline, na.rm = TRUE),
      state_avg_residential = mean(residential_percent_change_from_baseline, na.rm = TRUE)
    ) %>%
    ungroup()
  return(new_df)
}

# Vector to store the values we're interested in plotting
mobility_features <- c(
  "state_avg_retail", 
  "state_avg_parks",
  "state_avg_transit",
  "state_avg_workplaces",
  "state_avg_residential"
)

mobility_end_state_aggregate_df <- summarize_mobility(mobility_end_state_df)

# Reshape to produce the boxplots
mobility_end_state_aggregate_long_df <- mobility_end_state_aggregate_df %>%
  pivot_longer(cols = all_of(mobility_features), names_to = "feature", values_to = "value")

# Plot all features as boxplots
ggplot(mobility_end_state_aggregate_long_df, aes(x = feature, y = value)) +
  geom_boxplot() +
  theme_minimal() +
  labs(x = "Feature", y = "Percent Change", title = "Distribution of End-state Values")

# Plot the features by histograms for each state
  # Iterate over numeric feature names
plot <- ggplot(mobility_end_state_aggregate_long_df, aes(x = Postal, y = value, fill = feature)) +
  geom_col(position = "dodge") +
  facet_wrap(~ feature, scales = "free_y") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

print(plot)


# State to State comparison. To just compare two States this will require adjustment
# Reshape data to long format
mobility_subset_aggregate_df <- summarize_mobility(mobility_subset_df)
mobility_subset_aggregate_df_long <- mobility_subset_aggregate_df %>%
  pivot_longer(cols = all_of(mobility_features), names_to = "Feature", values_to = "Percent Change")

plot <- ggplot(mobility_subset_aggregate_df_long, aes(x = date, y = `Percent Change`, color = Postal)) +
  geom_smooth(se = FALSE) +
  scale_color_manual(values = c("TX" = "orange", "WA" = "darkblue")) +
  labs(
    title = "Texas vs Washington - Mobility Trends",
    x = "Date",
    y = "Percent Change",
    color = "Location"
  ) +
  facet_wrap(~ Feature, scales = "free_y") +  # Create separate plots for each feature
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotate x-axis labels

print(plot)  # Ensure the plot is displayed

# Join up the census data with the mobility data
combo_data <- left_join(
  mobility_end_state_aggregate_df, 
  census_summary_df, 
  by = c('Postal' = 'state')
)

make_histograms <- function(df, feature) {
  # Using Freedman-Diaconis Rule for bin width with input from ChatGPT
  bin_width <- 2 * IQR(df[[feature]], na.rm = TRUE) / (length(df[[feature]])^(1/3))
  p <- ggplot(df, aes(x = .data[[feature]])) +
    geom_histogram(binwidth = bin_width)
  
  print(p)
}

histogram_features <- c(
  "state_total_confirmed_cases",
  "state_total_deaths",
  "state_total_pop",
  "state_total_male_pop",
  "state_total_female_pop",
  "state_total_black_pop",
  "state_total_white_pop",
  "state_total_hispanic_pop",
  "state_total_commuters_by_public_transportation",
  "state_total_worked_at_home",
  "state_avg_income_per_capita",
  "state_total_masters_degree",
  "state_total_bachelors_degree",
  "state_total_high_school_diploma",
  "state_total_one_year_more_college"
)

for (feature in histogram_features) {
  make_histograms(combo_data, feature)
}




# Experiment with correlation
library(ggcorrplot)

corr_data <- combo_data %>%
  select(where(is.numeric)) %>%
  as.matrix() %>%
  cor()

ggcorrplot(corr_data)

# Let's look at education and number of cases
education_vs_cases <- combo_data %>%
  select(
    state_total_one_year_more_college,
    state_total_high_school_diploma,
    state_total_bachelors_degree,
    state_total_masters_degree,
    state_total_confirmed_cases
  ) %>%
  as.matrix() %>%
  cor()

ggcorrplot(education_vs_cases)

# How about money?
income_vs_cases <- combo_data %>%
  select(
    state_avg_income_per_capita,
    state_total_confirmed_cases,
    state_total_deaths
  ) %>%
  as.matrix() %>%
  cor()

ggcorrplot(income_vs_cases)