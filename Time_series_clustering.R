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


############################### Helper Functions ############################### 

# Aggregate the time series data at the week level
library(lubridate)
date_aggregator <- function(df) {
  df_monthly <- df %>%
    mutate(date = floor_date(date, unit = "month")) %>%
    
    return(df_monthly)
}

ts_df_to_matrix <- function(df, feature) {
  # Clean and convert to matrix as before
  mat <- df %>%
    select(county, date, feature) %>%
    arrange(county, date) %>%
    pivot_wider(names_from = date, values_from = feature) %>%
    column_to_rownames("county") %>%
    drop_na() %>%
    as.matrix()
  return(mat)
}

# getWSS_SIL <- function(k, data, type, ts_clusters=NULL){
#   # dist_matrix <- dist(data, method="DTW")
#   groc_pharm_clusters <- tsclust(groc_pharm_list, type = "partitional", k = 3, distance = "dtw")
#   cluster_labels <- ts_clusters@cluster
#   
#   if (type == "WSS") {
#     wss = 0
#     for (i in 1:k) {
#       cluster_points <- data[cluster_labels == i, , drop = FALSE]
#       dist_matrix <- dist(cluster_points, method = "DTW")
#       wss <- wss + sum(dist_matrix)
#     }
#     return(wss)
#   }
#   
#   if (type == "SIL") {
#     sil_score <- silhouette(cluster_labels, dist(data))
#     return(mean(sil_score[, 3]))
#   }
# }
# 
# # plot wss and sil graphs
# plot_ideal_ts_cluster_graph <- function(data) {
#   # Run k-means for 2 to 10 clusters
#   k_values <- 2:10
#   wss_values <- sapply(k_values, getWSS_SIL, data=data, type="WSS")
#   
#   df <- data.frame(wss = wss_values, k_values=k_values)
#   
#   p1 <- ggplot(df, aes(x=k_values,y=wss)) +
#     geom_line(color="blue", size=1) +
#     geom_point(color="red",size=2) +
#     labs(
#       title="WSS vs Number of Clusters", 
#       x="Number of Clusters (k)",
#       y="Sum of Squares"
#       ) +
#     theme_minimal() +
#     theme(
#       plot.title = element_text(size = 18, face = "bold"),
#       axis.title = element_text(size = 16),
#       axis.text = element_text(size = 14)
#     )
#   print(p1)
#   
#   sil_values <- sapply(k_values, getWSS_SIL, data=data, type="SIL")
#   df <- data.frame(k = k_values, silhouette = sil_values)
#   
#   # Make the plot
#   p2 <- ggplot(df, aes(x = k, y = silhouette)) +
#     geom_point(color = "blue") +
#     geom_line(color = "blue") +
#     labs(
#       title = "Silhouette Score vs Number of Clusters",
#       x = "Number of Clusters",
#       y = "Silhouette Score"
#     ) +
#     theme_minimal() +
#     theme(
#       plot.title = element_text(size = 18, face = "bold"),
#       axis.title = element_text(size = 16),
#       axis.text = element_text(size = 14)
#     )
#   
#   # Arrange in a row
#   grid.arrange(p1, p2,ncol = 2)
# }

visualize_multiDim_cluster <- function(data, ncol) {
  # PCA
  pca <- prcomp(data[,-ncol(data)], center=TRUE, scale.=TRUE)
  df_pca <- data.frame(pca$x[,1:2], cluster = as.factor(data$cluster))
  
  g1 <- ggplot(df_pca, aes(x = PC1, y = PC2, color = cluster)) +
    geom_point(size = 3) +
    labs(title = "PCA") +
    theme_minimal()
  
  # t-SNE
  tsne_results <- Rtsne(data[, -ncol(data)], perplexity = 5, check_duplicates = FALSE)
  df_tsne <- data.frame(tsne_results$Y, cluster = as.factor(data$cluster))
  
  g2 <- ggplot(df_tsne, aes(x = X1, y = X2, color = cluster)) +
    geom_point(size = 3) +
    labs(title = "t-SNE") +
    theme_minimal()
  
  # UMAP
  umap_result <- umap(data[, -ncol])
  df_umap <- data.frame(umap_result$layout, cluster = as.factor(data$cluster))
  
  g3 <- ggplot(df_umap, aes(x = X1, y = X2, color = cluster)) +
    geom_point(size = 3) +
    labs(title = "UMAP") +
    theme_minimal()
  
  # Arrange in a row
  grid.arrange(g1, g2, g3, ncol = 3)
}

cluster_profiles <- function(results) {
  ggplot(pivot_longer(as_tibble(results$centers,  rownames = "cluster"), 
                      cols = colnames(results$centers)), 
         aes(y = name, x = value, fill = cluster)) +
    geom_bar(stat = "identity") +
    facet_grid(cols = vars(cluster)) +
    labs(y = "feature", x = "z-scores", title = "Cluster Profiles") + 
    guides(fill="none") + 
    theme(
      plot.title = element_text(size = 18, face = "bold"),
      axis.title = element_text(size = 16),
      axis.text = element_text(size = 14)
    )
}

library(cluster)
silhouette_score <- function(k, data) {
  km <- kmeans(data, centers=k, nstart = 10)
  mean(silhouette(km$cluster, dist(data))[, 3])
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

# Import the mobility dataset
mobility_df <- read_csv("Global_Mobility_Report.csv")

#' In order to enable joining the census data to the mobility data I needed a 
#' lookup table of State name to it's abbreviation. Boy Scouts to the rescue!
#' https://www.scouting.org/resources/los/states/
state_to_postal <- read_csv("state_to_postal_code(Sheet1).csv")
head(state_to_postal)

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

# aggregate_mobility_scaled <- time_series_aggregate_df %>%
#   select(all_of(mobility_features)) %>%
#   scale() %>% as_tibble()

aggregate_mobility_scaled <- time_series_aggregate_df %>%
  mutate(across(all_of(mobility_features), scale))  # Scale the mobility features

summary(aggregate_mobility_scaled)


################# Grouping 1: Time Phase Clustering w/dtwclust #################

library(dtwclust)
conflicts_prefer(dtwclust::as.matrix)

# ts_cluster_data <- aggregate_mobility_scaled %>%
#   select(all_of(mobility_features))
# 
# ts_clusters <- tsclust(ts_cluster_data, type = "partitional", k=4, distance = "dtw")
# ts_cluster_data$cluster <- as.factor(ts_clusters@cluster)
# 
# mobility_clusters <- aggregate_mobility_scaled %>% 
#   add_column(cluster = factor(ts_clusters@cluster))
# 
# 
# visualize_multiDim_cluster(ts_cluster_data, ncol(ts_cluster_data))



# Clean and convert to matrix for tsclust
avg_retail_mat <- ts_df_to_matrix(aggregate_mobility_scaled, "avg_retail")
groc_pharm_mat <- ts_df_to_matrix(aggregate_mobility_scaled, "avg_grocery_and_pharmacy")
avg_parks_mat <- ts_df_to_matrix(aggregate_mobility_scaled, "avg_grocery_and_pharmacy")
avg_transit_mat <- ts_df_to_matrix(aggregate_mobility_scaled, "avg_transit")
avg_workplaces_mat <- ts_df_to_matrix(aggregate_mobility_scaled, "avg_workplaces")
avg_residential_mat <- ts_df_to_matrix(aggregate_mobility_scaled, "avg_residential")


# Convert to list of time series (one per county)
groc_pharm_list <- split(groc_pharm_mat, row(groc_pharm_mat))

# Run clustering
groc_pharm_clusters <- tsclust(groc_pharm_list, type = "partitional", k = 3, distance = "dtw")

# Rebuild the dataframe for visualization
groc_pharm_df <- as.data.frame(groc_pharm_mat) %>%
  rownames_to_column("county") %>%
  mutate(cluster = as.factor(groc_pharm_clusters@cluster))

groc_pharm_numeric <- groc_pharm_df %>%
  select(-county)

visualize_multiDim_cluster(groc_pharm_numeric, ncol(groc_pharm_numeric))


###################### Grouping 2: Time Phase Clustering ######################

library(TSclust)

ts_cluster_data <- aggregate_mobility_scaled %>%
  select(all_of(mobility_features))

dissim <- diss(ts_cluster_data, "CORT")
hc <- hclust(dissim, method = "ward.D2")
plot(hc)  






library(tsfeatures)
mobility_features <- tsfeatures(time_series_list)
kmeans_result <- kmeans(mobility_features, centers = 3)




