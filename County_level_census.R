library(tidyverse)
library(ggplot2)
library(GGally)


# Import the initial mobility dataset
census_cases_df <- read_csv("COVID-19_cases_plus_census.csv")

#' Because the majority of our data is based on Texas, we'll start with that
#' portion of the data for examination. I'd like to see the distribution of 
#' values starting with box plots.

# Select a subset of variables to examine
texas_state_census_df <- census_cases_df %>%
  select(
    county_name,
    state,
    confirmed_cases,
    deaths,
    male_65_to_66,
    male_67_to_69,
    male_70_to_74,
    male_75_to_79,
    male_80_to_84,
    male_85_and_over,
    female_65_to_66,
    female_67_to_69,
    female_70_to_74,
    female_75_to_79,
    female_80_to_84,
    female_85_and_over,
    median_income,
    poverty,
    white_pop,
    hispanic_pop,
    black_pop,
    asian_pop
  ) %>%
  filter(state == "TX")

#' We know covid disproportionately affected the elderly. I'd like an idea of 
#' just how prevalent elderly populations were, so I'm going to combine some 
#' features.

male_elderly_features <- c(
  "male_65_to_66",
  "male_67_to_69",
  "male_70_to_74",
  "male_75_to_79",
  "male_80_to_84", 
  "male_85_and_over"
  )
female_elderly_features <- c(
  "female_65_to_66",
  "female_67_to_69",
  "female_70_to_74",
  "female_75_to_79",
  "female_80_to_84",
  "female_85_and_over"
  )


# Create new variables for combined age population, drop old ones
texas_state_census_df <- texas_state_census_df %>%
  mutate(male_65_and_over = rowSums(across(male_elderly_features))) %>%
  mutate(female_65_and_over = rowSums(across(female_elderly_features))) %>%
  mutate(pop_over_65 = male_65_and_over + female_65_and_over) %>%
  select(
    -male_elderly_features, 
    -female_elderly_features,
    -male_65_and_over,
    -female_65_and_over
    )

install.packages("clipr")
library(clipr)
summary(texas_state_census_df)
summary(texas_state_census_df) %>% as.data.frame() %>% write_clip()

features_of_interest <- c(
  "confirmed_cases",
  "deaths",
  "pop_over_65",
  "median_income",
  "poverty",
  "white_pop",
  "hispanic_pop",
  "black_pop",
  "asian_pop"
  )

#' Function to create histograms for all the features of interest
make_histograms <- function(df) {
  df_long <- df %>%
    pivot_longer(
      cols = -c(county_name, state),
      names_to = "feature",
      values_to = "value"
    )
  
  plot <- ggplot(df_long, aes(x = value)) +
    geom_histogram(aes(y = after_stat(count)), bins = 30) +
    facet_wrap(~feature, scales = "free") +  # wrap the charts
    # facet_grid(~feature, scales = "free") +  # single row
    labs(title = "Distribution of Selected Features in Texas Counties",
         x = "Value", y = "Count") +
    theme_minimal()
  
  print(plot)
}

make_histograms(texas_state_census_df)


#' To do answer our analysis question I'd like to pick two counties to compare.
#' The counties should have roughly the same population density because we 
#' know COVID spread faster in dense cities than in wide open areas.

# Referencing code from provided class example
library("ggrepel")
ggplot(texas_state_census_df, mapping = aes(x = confirmed_cases, y = deaths, label = county_name)) + 
  geom_smooth(method = lm) +
  geom_point(mapping = aes(size = pop_over_65), color = "grey") + 
  geom_text_repel(data = subset(texas_state_census_df, deaths >= 1000)) +
  labs(x = "confirmed cases", size = "Total Population Over 65")

# Create a dataframe of just for Tarrant and Bexar counties
county_census_df <- texas_state_census_df %>%
  filter(county_name == "Tarrant County" | county_name == "Bexar County")

head(county_census_df)

# Create a vector for demographic values to ease their use in plotting
demographic_features <- c(
  "confirmed_cases",
  "deaths",
  "poverty",
  "white_pop",
  "hispanic_pop",
  "black_pop",
  "asian_pop",
  "pop_over_65"
)

# I'm interested in looking at some correlation between ethnicity and cases/deaths
library(ggcorrplot)

corr_data <- texas_state_census_df %>%
  select(all_of(demographic_features)) %>%
  as.matrix() %>%
  cor()

ggcorrplot(corr_data)

#'This correlation data is interesting, but all ethnicities were correlated
#'to cases, with some more than others. I need a different way to look at the
#'magnitiude of this correlation.


# Adjust my feature vector to remove cases and deaths for this purpose
demographic_features <- c(
  "poverty",
  "white_pop",
  "hispanic_pop",
  "black_pop",
  "asian_pop",
  "pop_over_65"
)

#' The following is a different look at demographic how the strength of these
#' correlations played a role in the number of confirmed cases and deaths. This
#' was produced with help from ChatGPT
outcome_vars <- c("confirmed_cases", "deaths")

# Compute correlation matrix
corr_matrix <- texas_state_census_df %>%
  select(all_of(c(outcome_vars, demographic_features))) %>%
  cor()

# Extract only correlations between race and outcomes
corr_subset <- corr_matrix[demographic_features, outcome_vars]

# Convert to long format for ggplot
corr_long <- as.data.frame(as.table(corr_subset)) %>%
  rename(Race = Var1, Metric = Var2, Correlation = Freq)

# Create the dot plot
ggplot(corr_long, aes(x = Correlation, y = Race, color = Correlation)) +
  geom_point(size = 5) +
  scale_color_gradient(low = "lightblue", high = "red") +
  facet_wrap(~ Metric, scales = "free_x") +  # Separate for cases and deaths
  theme_minimal() +
  labs(title = "Correlation Between Race and COVID-19 Outcomes",
       x = "Correlation Strength",
       y = "Demographic",
       color = "Correlation")