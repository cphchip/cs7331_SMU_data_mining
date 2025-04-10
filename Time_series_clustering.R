library(tidyverse)
library(ggplot2)
library(GGally)
library(dplyr)
library(conflicted)
library(Rtsne)
library(umap)
conflicts_prefer(dplyr::filter)
conflicted::conflicts_prefer(proxy::dist)
library(gridExtra)

################################# Data Import ################################# 

# Import the Texas cases dataset
texas_time_series_df <- read_csv("COVID-19_cases_TX.csv")

# Pull in the Texas counties map
counties_polygon <- as_tibble(map_data("county"))
counties_polygon_TX <- counties_polygon %>% dplyr::filter(region == "texas") %>% 
  rename(c(county = subregion)) 

# Import the mobility dataset
mobility_df <- read_csv("Global_Mobility_Report.csv")

#' In order to enable joining the census data to the mobility data I needed a 
#' lookup table of State name to it's abbreviation. Boy Scouts to the rescue!
#' https://www.scouting.org/resources/los/states/
state_to_postal <- read_csv("state_to_postal_code(Sheet1).csv")
head(state_to_postal)


############################### Helper Functions ############################### 

# Aggregate the time series data at the week level
library(lubridate)
date_aggregator <- function(df) {
  df_monthly <- df %>%
    mutate(date = floor_date(date, unit = "month")) %>%
    
    return(df_monthly)
}


# Convert dataframe to matrix
ts_df_to_matrix <- function(df, feature) {
  mat <- df %>%
    select(county, date, all_of(feature)) %>%
    arrange(county, date) %>%
    pivot_wider(names_from = date, values_from = all_of(feature)) %>%
    column_to_rownames("county") %>%
    drop_na() %>%
    as.matrix()
  return(mat)
}

matrix_to_ts_df <- function(df, feature_name = "value") {
  df %>%
    pivot_longer(
      cols = -c(county, cluster),   # Keep both county and cluster as-is
      names_to = "date",
      values_to = feature_name
    ) %>%
    mutate(date = as.Date(date))
}


# Perform time-series clustering
run_ts_clustering <- function(mat, mytitle) {
  # Convert to list of time series (one per county)
  mat_as_list <- split(mat, row(mat))
  
  # Run clustering
  clustering <- tsclust(mat_as_list, type = "partitional", k = 3, distance = "dtw")
  
  # Rebuild the dataframe for visualization
  clustered_df <- as.data.frame(mat) %>%
    rownames_to_column("county") %>%
    mutate(cluster = as.factor(clustering@cluster))
  
  numeric_data <- clustered_df %>%
    select(-county)
  
  visualize_multiDim_cluster(numeric_data, ncol(numeric_data), mytitle)
  clustered_df <- clustered_df %>%
    select(county, cluster)
  return(clustered_df)
}


visualize_multiDim_cluster <- function(data, ncol, mytitle) {
  # PCA
  pca <- prcomp(data[,-ncol(data)], center=TRUE, scale.=TRUE)
  df_pca <- data.frame(pca$x[,1:2], cluster = as.factor(data$cluster))
  
  g1 <- ggplot(df_pca, aes(x = PC1, y = PC2, color = cluster)) +
    geom_point(size = 3) +
    labs(title = paste("PCA - ",mytitle)) +
    theme_minimal()
  
  # t-SNE
  tsne_results <- Rtsne(data[, -ncol(data)], perplexity = 5, check_duplicates = FALSE)
  df_tsne <- data.frame(tsne_results$Y, cluster = as.factor(data$cluster))
  
  g2 <- ggplot(df_tsne, aes(x = X1, y = X2, color = cluster)) +
    geom_point(size = 3) +
    labs(title = paste("t-SNE - ", mytitle)) +
    theme_minimal()
  
  # UMAP
  umap_result <- umap(data[, -ncol])
  df_umap <- data.frame(umap_result$layout, cluster = as.factor(data$cluster))
  
  g3 <- ggplot(df_umap, aes(x = X1, y = X2, color = cluster)) +
    geom_point(size = 3) +
    labs(title = paste("UMAP - ", mytitle)) +
    theme_minimal()
  
  # Arrange in a row
  grid.arrange(g1, g2, g3, ncol = 3)
}

cluster_profiles <- function(ts_cluster_result, original_matrix) {
  cluster_assignments <- ts_cluster_result$cluster
  
  # Rebuild data frame: add county, cluster, and reshape
  df <- as.data.frame(original_matrix) %>%
    rownames_to_column("county") %>%
    mutate(cluster = as.factor(cluster_assignments)) %>%
    pivot_longer(
      cols = -c(county, cluster),
      names_to = "date", values_to = "z_score"
    )
  
  # Plot mean z-scores by cluster over time
  df %>%
    group_by(cluster, date) %>%
    summarise(mean_z = mean(z_score, na.rm = TRUE), .groups = "drop") %>%
    ggplot(aes(x = date, y = mean_z, color = cluster, group = cluster)) +
    geom_line(size = 1.2) +
    labs(
      title = "Cluster Profiles Over Time",
      x = "Date",
      y = "Average Z-score",
      color = "Cluster"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(size = 18, face = "bold"),
      axis.title = element_text(size = 16),
      axis.text = element_text(size = 12)
    )
}


################################## Data Prep ################################## 

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

# Join the State abbreviation dataset to my mobility dataset
mobility_df <- left_join(
  mobility_df,
  state_to_postal, 
  by = c('sub_region_1' = 'State')
)

#' To pare down the mobility dataset I'll select a subset of features. I'll 
#' also filter for United States and the state of Texas because that's all
#' my Covid-19 cases datasaet contains. Finally, I'll drop any NA values.
mobility_subset_df <- mobility_df %>%
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


montly_texas_clean_df <- date_aggregator(texas_clean_df)
montly_mobility_df <- date_aggregator(mobility_subset_df)

# Join the two datasets
time_series_df <- left_join(
  montly_mobility_df,
  montly_texas_clean_df,
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

mobility_features <- c(
  "avg_retail", 
  "avg_grocery_and_pharmacy",
  "avg_parks",
  "avg_transit",
  "avg_workplaces",
  "avg_residential"
  )

time_series_aggregate_df <- aggregate_mobility(time_series_df)

# Change the county names to be compatible with the county map
time_series_aggregate_df <- time_series_aggregate_df %>% mutate(county = sub_region_2 %>% 
  str_to_lower() %>% str_replace('\\s+county\\s*$', ''))

# time_series_aggregate_df %>% pull(county)

summary(time_series_aggregate_df)

aggregate_mobility_scaled <- time_series_aggregate_df %>%
  mutate(across(all_of(mobility_features), scale))  # Scale the mobility features

summary(aggregate_mobility_scaled)


################# Grouping 1: Time Phase Clustering w/dtwclust #################

library(dtwclust)
conflicts_prefer(dtwclust::as.matrix)

# Clean and convert to matrix for tsclust
avg_retail_mat      <- ts_df_to_matrix(aggregate_mobility_scaled, "avg_retail")
avg_groc_pharm_mat  <- ts_df_to_matrix(aggregate_mobility_scaled, "avg_grocery_and_pharmacy")
avg_parks_mat       <- ts_df_to_matrix(aggregate_mobility_scaled, "avg_parks")
avg_transit_mat     <- ts_df_to_matrix(aggregate_mobility_scaled, "avg_transit")
avg_workplaces_mat  <- ts_df_to_matrix(aggregate_mobility_scaled, "avg_workplaces")
avg_residential_mat <- ts_df_to_matrix(aggregate_mobility_scaled, "avg_residential")

matrix_features <- list(
  avg_retail_mat,
  avg_groc_pharm_mat,
  avg_parks_mat,
  avg_transit_mat,
  avg_workplaces_mat,
  avg_residential_mat
)

retail_res      <- run_ts_clustering(avg_retail_mat, "Average Retail")
gorc_pharm_res  <- run_ts_clustering(avg_groc_pharm_mat, "Average Groc & Pharm")
park_res        <- run_ts_clustering(avg_parks_mat, "Average Park")
transit_res     <- run_ts_clustering(avg_transit_mat, "Average Transit")
workplaces_res  <- run_ts_clustering(avg_workplaces_mat, "Average Workplaces")
residential_res <- run_ts_clustering(avg_residential_mat, "Average Residential")

cluster_profiles(retail_res,avg_retail_mat)

# Map our results to the county map of Texas
county_plot <- function(df, title) {
  counties_polygon_TX_clust <- left_join(counties_polygon_TX, df, join_by(county))
  
  ggplot(counties_polygon_TX_clust, aes(long, lat)) +
    geom_polygon(aes(group = group, fill = cluster), color = "white") +
    scale_fill_manual(
      values = c("1" = "#1f77b4", "2" = "#ff7f0e", "3" = "#2ca02c"),
      na.value = "black"  # fill counties with no cluster as black
    ) +
    coord_quickmap() +
    labs(title = paste("Texas County Clustering -", title), fill = "Cluster")
}

c1 <- county_plot(retail_res, "Average Retail")
c2 <- county_plot(gorc_pharm_res, "Average Groc & Pharm")
c3 <- county_plot(park_res, "Average Park")
c4 <- county_plot(transit_res, "Average Transit")
c5 <- county_plot(workplaces_res, "Average Workplaces")
c6 <- county_plot(residential_res, "Average Residential")

grid.arrange(c1, c2, c3, c4, c5, c6, ncol = 2)

