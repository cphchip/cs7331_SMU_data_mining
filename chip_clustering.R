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
    total_pop,
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

texas_state_census_df <- texas_state_census_df %>% mutate(across(where(is.character), factor))
dim(texas_state_census_df)

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

# Calculate cases, deaths per thousand people
#######################################################################
# This portion of code came from the getting started content from class
#######################################################################
texas_state_census_df <- texas_state_census_df %>% mutate(
  cases_per_1000 = confirmed_cases/total_pop*1000, 
  deaths_per_1000 = deaths/total_pop*1000, 
  deaths_per_case = deaths/confirmed_cases)

summary(texas_state_census_df)

features_of_interest <- c(
  "confirmed_cases",
  "deaths",
  "median_income",
  "poverty",
  "white_pop",
  "hispanic_pop",
  "black_pop",
  "asian_pop",
  "pop_over_65",
  "cases_per_1000",
  "deaths_per_1000",
  "deaths_per_case"
)

counties_polygon <- as_tibble(map_data("county"))
counties_polygon_TX <- counties_polygon %>% dplyr::filter(region == "texas") %>% 
  rename(c(county = subregion)) 
counties_polygon_TX

texas_state_census_df <- texas_state_census_df %>% mutate(county = county_name %>% 
  str_to_lower() %>% str_replace('\\s+county\\s*$', ''))
texas_state_census_df %>% pull(county)

counties_polygon_TX <- right_join(counties_polygon_TX, texas_state_census_df)

ggplot(counties_polygon_TX, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = deaths_per_1000)) +
  coord_quickmap() +
  scale_fill_continuous(type = "viridis") +
  labs(title = "Texas death rates by county")

#' Scale our data. Clustering algorithms use distances and the variables with 
#' the largest number range will dominate distance calculation.
cases_TX_scaled <- texas_state_census_df %>% 
  select(
    features_of_interest,
    -confirmed_cases,
    -deaths,
    -cases_per_1000,
    -deaths_per_1000,
    -deaths_per_case
  ) %>% 
  scale() %>% as_tibble()

summary(cases_TX_scaled)

km <- kmeans(cases_TX_scaled, centers = 4, nstart = 10)
km

ggplot(pivot_longer(as_tibble(km$centers,  rownames = "cluster"), 
  cols = colnames(km$centers)), 
  aes(y = name, x = value, fill = cluster)) +
  geom_bar(stat = "identity") +
  facet_grid(cols = vars(cluster)) +
  labs(y = "feature", x = "z-scores", title = "Cluster Profiles") + 
  guides(fill="none")


cases_TX_clust <- texas_state_census_df %>% 
  add_column(cluster = factor(km$cluster))
counties_polygon_TX_clust <- right_join(counties_polygon_TX, cases_TX_clust, 
                                        join_by(county))

ggplot(counties_polygon_TX_clust, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = cluster)) +
  coord_quickmap() + 
  labs(title = "Texas Map with clustered counties")

cases_TX_clust %>% group_by(cluster) %>% summarize(
  avg_cases_per_1000 = mean(cases_per_1000), 
  avg_deaths_per_1000 = mean(deaths_per_1000))



####################################################
# Let's try hierarchial clustering
library(factoextra)
d <- dist(cases_TX_scaled)
hc <- hclust(d, method = "complete")
plot(hc)

fviz_dend(hc)



####################################################
# Let's try DBSCAN clustering
install.packages("dbscan")
library(dbscan)

kNNdistplot(cases_TX_scaled, minPts = 4)
abline(h=.32, col = "red")
