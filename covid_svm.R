library(tidyverse)
library(gridExtra)


################################# Data Import ################################# 

# Import the initial census dataset
census_cases_df <- read_csv("COVID-19_cases_plus_census.csv")

# Pull in the Texas counties map
counties_polygon <- as_tibble(map_data("county"))
counties_polygon_TX <- counties_polygon %>% dplyr::filter(region == "texas") %>% 
  rename(c(county = subregion)) 

# Change the county names to be compatible with the county map
census_cases_df <- census_cases_df %>% mutate(county = county_name %>% 
                                                str_to_lower() %>% str_replace('\\s+county\\s*$', ''))
census_cases_df %>% pull(county)

# Select a subset of variables to examine
texas_state_census_df <- census_cases_df %>%
  select(
    county, state, confirmed_cases, deaths, total_pop, male_65_to_66,
    male_67_to_69, male_70_to_74, male_75_to_79, male_80_to_84, male_85_and_over,
    female_65_to_66, female_67_to_69, female_70_to_74, female_75_to_79,
    female_80_to_84, female_85_and_over, male_pop, female_pop, median_income,
    poverty, white_pop, hispanic_pop, black_pop, asian_pop, amerindian_pop,
    other_race_pop, two_or_more_races_pop, households
  ) %>%
  filter(state == "TX") %>%
  select(where(~ !is.logical(.))) %>%
  na.omit()

#set randomness seed
set.seed(1015)


################################## Data Prep ##################################

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


# Create new variables for combined age population, drop old ones.
# For this effort I just want pop_over_65.
texas_state_census_df <- texas_state_census_df %>%
  mutate(male_over_65 = rowSums(across(male_elderly_features))) %>%
  mutate(female_over_65 = rowSums(across(female_elderly_features))) %>%
  mutate(pop_over_65 = male_over_65 + female_over_65) %>%
  select(
    -male_elderly_features, 
    -female_elderly_features,
    -male_over_65,
    -female_over_65
  )

# Create opposite variable to hold all population under 65
texas_state_census_df$pop_under_65 <- texas_state_census_df$total_pop - texas_state_census_df$pop_over_65

#' To make sure all counties are on a level playing field from a density of 
#' measurement perspective, I'll ensure my data is in a format of "per 1000 
#' people"
texas_census_per_1000 <- texas_state_census_df

cols_to_convert <- c(
  "deaths", "confirmed_cases", "white_pop", "black_pop", "asian_pop", 
  "hispanic_pop", "amerindian_pop", "other_race_pop", "two_or_more_races_pop", 
  "male_pop", "female_pop","pop_over_65", "pop_under_65", "households"
)

# Apply the 'per 1000' conversion to all cols_to_convert
texas_census_per_1000 <- texas_state_census_df %>%
  mutate(across(
    all_of(cols_to_convert),
    ~ .x / total_pop * 1000,
    .names = "{.col}_per_1000"
  )) %>%
  select(-cols_to_convert)

summary(texas_census_per_1000$deaths_per_1000)
summary(texas_census_per_1000$confirmed_cases_per_1000)


############################# Determining Classes #############################

plot_risk_cut_methods <- function(data, risk_var_name, bins = 30) {
  risk_sym <- sym(risk_var_name)
  risk_values <- pull(data, !!risk_sym)
  
  # Quantile cuts
  p1 <- ggplot(data, aes(x = !!risk_sym)) +
    geom_histogram(bins = bins, fill = "lightblue", color = "black") +
    geom_vline(
      xintercept = quantile(risk_values, probs = c(1/3, 2/3), na.rm = TRUE),
      color = "red", linetype = "dashed", size = 1.2
    ) +
    labs(
      title = paste("Quantile Cutoffs on", risk_var_name),
      x = risk_var_name, y = "Count"
    )
  
  # Equal width cuts
  equal_width_breaks <- seq(
    from = min(risk_values, na.rm = TRUE),
    to   = max(risk_values, na.rm = TRUE),
    length.out = 4 # 3 bins = 4 endpoints
  )
  
  p2 <- ggplot(data, aes(x = !!risk_sym)) +
    geom_histogram(bins = bins, fill = "lightblue", color = "black") +
    geom_vline(
      xintercept = equal_width_breaks[2:3],
      color = "red", linetype = "dashed", size = 1.2
    ) +
    labs(
      title = paste("Equal Width Cutoffs on", risk_var_name),
      x = risk_var_name, y = "Count"
    )
  
  # K-means clustering
  set.seed(42)
  kmeans_result <- kmeans(risk_values, centers = 3)
  kmeans_centers <- sort(kmeans_result$centers)
  
  # Estimate cutoffs as midpoint between sorted cluster centers
  kmeans_cutoffs <- kmeans_centers[1:2] + diff(kmeans_centers) / 2
  
  p3 <- ggplot(data, aes(x = !!risk_sym)) +
    geom_histogram(bins = bins, fill = "lightblue", color = "black") +
    geom_vline(
      xintercept = kmeans_cutoffs,
      color = "red", linetype = "dashed", size = 1.2
    ) +
    labs(
      title = paste("K-means Cutoffs on", risk_var_name),
      x = risk_var_name, y = "Count"
    )
  
  # Combine the plots vertically
  grid.arrange(p1, p2, p3, ncol = 1)
}

plot_risk_cut_methods(texas_census_per_1000, "deaths_per_1000")
plot_risk_cut_methods(texas_census_per_1000, "confirmed_cases_per_1000")



# Perform K-means clustering as the selected method for class identification
set.seed(42)
kmeans_result <- kmeans(texas_census_per_1000$confirmed_cases_per_1000, centers = 3)
kmeans_centers <- sort(kmeans_result$centers)

county_risk_df <- texas_census_per_1000 %>% 
  add_column(cluster = factor(kmeans_result$cluster)) %>%
  select(
    "county", "deaths_per_1000", "confirmed_cases_per_1000", "cluster"
  )

counties_polygon_TX_clust <- right_join(counties_polygon_TX, 
                                        county_risk_df, 
                                        join_by(county))

ggplot(counties_polygon_TX_clust, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = cluster)) +
  coord_quickmap() + 
  labs(title = "Texas Map with Age Clustered Counties")

cluster_to_label <- c("med", "high", "low")

county_risk_df <- county_risk_df %>%
  mutate(risk_level = cluster_to_label[cluster]) %>%
  select("county", "risk_level")

texas_census_risk <- right_join(texas_census_per_1000,county_risk_df,"county") %>%
  mutate(across(where(is.character), factor))


# Count how many counties are in each cluster
risk_counts <- texas_census_risk %>%
  count(risk_level) %>%
  mutate(label = as.character(n))

# Create a pie chart
ggplot(risk_counts, aes(x = "", y = n, fill = risk_level)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y") +
  geom_text(aes(label = label), 
            position = position_stack(vjust = 0.5), 
            size = 5, color = "white") +
  labs(title = "Counties by Risk Level (Clustered)", fill = "Risk Level") +
  theme_void()


######################### Train-Test-Validation Split #########################

library(sampling)

X <- texas_census_risk %>%
  select(-"county", -"state", -"risk_level", -"confirmed_cases_per_1000")

X_scaled <- X |> scale() |> as_tibble()

y <- texas_census_risk$risk_level

# X_y <- X %>% add_column(y)
X_y <- X_scaled %>% add_column(y)

summary(X_y)

set.seed(1000) # for repeatability

# Will use stratified sampling because of our class imbalance
id <- strata(X_y, stratanames = "y",
             size = c(25, 25, 25), method = "srswr")
X_y_train <- X_y |>
  slice(id$ID_unit)

# Pull out a validation set that doesn't include the training data
X_y_validation <- X_y %>%
  filter(!row_number() %in% id$ID_unit)

dim(X_y_train)
dim(X_y_validation)


############################# Support Vector Machine ###########################

library(caret)

# Run svmFit with hyperparameter Linear
svmFit_linear <- X_y_train |>
  train(y ~.,
        method = "svmLinear",
        data = _,
        tuneLength = 5,
        trControl = trainControl(method = "cv"))
svmFit_linear

# Run svmFit with hyperparameter Radial
svmFit_rad <- X_y_train |>
  train(y ~.,
        method = "svmRadial",
        data = _,
        tuneLength = 5,
        trControl = trainControl(method = "cv"))
svmFit_rad


# Run svmFit with hyperparameter Poly
svmFit_poly <- X_y_train |>
  train(y ~.,
        method = "svmPoly",
        data = _,
        tuneLength = 5,
        trControl = trainControl(method = "cv"))
svmFit_poly

# Plot the ranges of Kappa
resamples <- resamples(list(Linear = svmFit_linear, Radial = svmFit_rad, Poly = svmFit_poly))
summary(resamples)
bwplot(resamples, metric = "Kappa")

# Using the trained model, predict the classes of the validation set
y_pred_linear <- predict(svmFit_linear, newdata = X_y_validation)
y_pred_rad    <- predict(svmFit_rad, newdata = X_y_validation)
y_pred_poly   <- predict(svmFit_poly, newdata = X_y_validation)

# Linear Confusion Matrix
confusionMatrix(y_pred_linear, X_y_validation$y)

# Radial Confusion Matrix
confusionMatrix(y_pred_rad, X_y_validation$y)

# Poly Confusion Matrix
confusionMatrix(y_pred_poly, X_y_validation$y)


############################### Naive Bayes ####################################

NBFit <- train(
  x = X_scaled,
  y = y,
  method = "nb",
  tuneGrid = data.frame(fL = c(.2, .5, 1, 5), 
                        usekernel = TRUE, adjust = 1),
  trControl = trainControl(method = "cv")
)
NBFit

y_pred_NB <- predict(NBFit, newdata = X_y_validation)

confusionMatrix(y_pred_NB, X_y_validation$y)
