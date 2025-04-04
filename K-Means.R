library(tidyverse)
library(ggplot2)
library(GGally)
library(dplyr)
library(conflicted)
library(Rtsne)
library(umap)
conflicts_prefer(dplyr::filter)

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
    county,
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
    male_pop,
    female_pop,
    median_income,
    poverty,
    white_pop,
    hispanic_pop,
    black_pop,
    asian_pop,
    amerindian_pop,
    other_race_pop,
    two_or_more_races_pop
  ) %>%
  filter(state == "TX") %>%
  select(where(~ !is.logical(.))) %>%
  na.omit()

#set randomness seed
set.seed(1015)


##################### Helper functions ##################### 

getWSS_SIL <- function(k, data, type){
  dist_matrix <- dist(data)
  km <- kmeans(data, centers = k, nstart = 10)
  if (type == "WSS") {
    return(km$tot.withinss)
  }
  else {
    return(mean(silhouette(km$cluster, dist_matrix)[, 3]))
  }
}

# plot wss and sil graphs
plot_ideal_cluster_graph <- function(data) {
  # Run k-means for 2 to 10 clusters
  k_values <- 2:10
  wss_values <- sapply(k_values, getWSS_SIL, data=data, type="WSS")

  df <- data.frame(wss = wss_values,
                   k_values=k_values)
  
  p <- ggplot(df, aes(x=k_values,y=wss)) +
    geom_line(color="blue", size=1) +
    geom_point(color="red",size=2) +
    labs(title="WSS vs Number of Clusters", x="Number of Clusters (k)",y="Sum of Squares") +
    theme_minimal()
  print(p)
  
  sil_values <- sapply(k_values, getWSS_SIL, data=data, type="SIL")
  
  plot(k_values, sil_values, type = "b", pch = 19, col = "blue",
       xlab = "Number of Clusters", ylab = "Silhouette Score",
       main = "Silhouette Score vs Number of Clusters")
}


# requires that data has already been clustered
visualize_multiDim_cluster <- function(data, ncol) {
  # PCA
  pca <- prcomp(data[,-ncol(data)], center=TRUE, scale.=TRUE)
  df_pca <-data.frame(pca$x[,1:2],cluster=as.factor(data$cluster))
  
  g1<-ggplot(df_pca, aes(x = PC1, y = PC2, color = cluster)) +
    geom_point(size = 3) +
    labs(title = "PCA Visualization of Clusters") +
    theme_minimal()
  
  # t-SNE
  tsne_results <- Rtsne(data[, -ncol(data)], perplexity=30, check_duplicates=FALSE)
  df_tsne <- data.frame(tsne_results$Y, cluster=as.factor(data$cluster))
  
  g2<- ggplot(df_tsne, aes(x = X1, y = X2, color = cluster)) +
    geom_point(size = 3) +
    labs(title = "t-SNE Visualization of Clusters") +
    theme_minimal()
  
  # umap
  umap_result <- umap(data[,-ncol])
  df_umap <- data.frame(umap_result$layout, cluster = as.factor(data$cluster))
  
  g3<-ggplot(df_umap, aes(x = X1, y = X2, color = cluster)) +
    geom_point(size = 3) +
    labs(title = "UMAP Visualization of Clusters") +
    theme_minimal()
  print(g1)
  print(g2)
  print(g3)
}

cluster_profiles <- function(results) {
  ggplot(pivot_longer(as_tibble(results$centers,  rownames = "cluster"), 
                      cols = colnames(results$centers)), 
         aes(y = name, x = value, fill = cluster)) +
    geom_bar(stat = "identity") +
    facet_grid(cols = vars(cluster)) +
    labs(y = "feature", x = "z-scores", title = "Cluster Profiles") + 
    guides(fill="none")
}

library(cluster)
silhouette_score <- function(k, data) {
  km <- kmeans(data, centers=k, nstart = 10)
  mean(silhouette(km$cluster, dist(data))[, 3])
}



################## Data Prep ################## 

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
  mutate(male_over_65 = rowSums(across(male_elderly_features))) %>%
  mutate(female_over_65 = rowSums(across(female_elderly_features))) %>%
  mutate(pop_over_65 = male_over_65 + female_over_65) %>%
  select(
    -male_elderly_features, 
    -female_elderly_features,
  )

# I'll also add opposite features to these:
texas_state_census_df$male_under_65   <- texas_state_census_df$male_pop - texas_state_census_df$male_over_65
texas_state_census_df$female_under_65 <- texas_state_census_df$female_pop - texas_state_census_df$female_over_65
texas_state_census_df$pop_under_65    <- texas_state_census_df$total_pop - texas_state_census_df$pop_over_65

#' To make sure all counties are on a level playing field from a density of 
#' measurement perspective, I'll ensure my data is in a format of "per 1000 
#' people"
texas_census_per_1000 <- texas_state_census_df

texas_census_per_1000$deaths_per_1000          <- texas_census_per_1000$deaths / texas_census_per_1000$total_pop * 1000
texas_census_per_1000$cases_per_1000           <- texas_census_per_1000$confirmed_cases / texas_census_per_1000$total_pop * 1000
texas_census_per_1000$white_pop_per_1000       <- texas_census_per_1000$white_pop / texas_census_per_1000$total_pop * 1000
texas_census_per_1000$black_pop_per_1000       <- texas_census_per_1000$black_pop / texas_census_per_1000$total_pop * 1000
texas_census_per_1000$asian_pop_per_1000       <- texas_census_per_1000$asian_pop / texas_census_per_1000$total_pop * 1000
texas_census_per_1000$hispanic_pop_per_1000    <- texas_census_per_1000$hispanic_pop / texas_census_per_1000$total_pop * 1000
texas_census_per_1000$amerindian_pop_per_1000  <- texas_census_per_1000$amerindian_pop / texas_census_per_1000$total_pop * 1000
texas_census_per_1000$other_race_pop_per_1000  <- texas_census_per_1000$other_race_pop / texas_census_per_1000$total_pop * 1000
texas_census_per_1000$two_or_more_races_pop    <- texas_census_per_1000$two_or_more_races_pop / texas_census_per_1000$total_pop * 1000
texas_census_per_1000$female_over_65_per_1000  <- texas_census_per_1000$female_over_65 / texas_census_per_1000$total_pop * 1000
texas_census_per_1000$male_over_65_per_1000    <- texas_census_per_1000$male_over_65 / texas_census_per_1000$total_pop * 1000
texas_census_per_1000$male_pop_per_1000        <- texas_census_per_1000$male_pop / texas_census_per_1000$total_pop * 1000
texas_census_per_1000$female_pop_per_1000      <- texas_census_per_1000$female_pop / texas_census_per_1000$total_pop * 1000
texas_census_per_1000$male_under_65_per_1000   <- texas_census_per_1000$male_under_65 / texas_census_per_1000$total_pop * 1000
texas_census_per_1000$female_under_65_per_1000 <- texas_census_per_1000$female_under_65 / texas_census_per_1000$total_pop * 1000
texas_census_per_1000$pop_under_65_per_1000    <- texas_census_per_1000$pop_under_65 / texas_census_per_1000$total_pop * 1000

summary(texas_census_per_1000)

#' For our clustering functions to work properly, my data needs to be scaled. 
#' This data will be the basis of my scaling functions.
scaled_tx_census_features <- texas_census_per_1000 %>%
  select(where(is.numeric)) %>%
  scale() %>% as_tibble()

summary(scaled_tx_census_features)

counties_polygon_TX <- right_join(counties_polygon_TX, texas_census_per_1000)

ggplot(counties_polygon_TX, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = deaths_per_1000)) +
  coord_quickmap() +
  scale_fill_continuous(type = "viridis") +
  labs(title = "Texas death rates by county")

################### Grouping 1: Ethnic makeup of the county ###################
pop_features <- c(
  "white_pop_per_1000",
  "black_pop_per_1000",
  "asian_pop_per_1000", 
  "hispanic_pop_per_1000", 
  "amerindian_pop_per_1000", 
  "other_race_pop_per_1000"
  )

scaled_census_pop_features <- scaled_tx_census_features %>%
  select(all_of(pop_features))

# perform kmeans
plot_ideal_cluster_graph(scaled_census_pop_features)

# dist_matrix <- dist(scaled_census_pop_features)
k <- 6
kmeans_results <- kmeans(scaled_census_pop_features, centers=k, nstart = 10)

# Assign cluster labels to dataset
scaled_census_pop_features$cluster <- as.factor(kmeans_results$cluster)

# Assess cluster profiles
cluster_profiles(kmeans_results)

# plotting with PCA, UMAP, and tsne
visualize_multiDim_cluster(scaled_census_pop_features, ncol(scaled_census_pop_features))

# Visualize with Texas county map
# cases_TX_clust <- texas_state_census_df %>% 
#   add_column(cluster = factor(km$cluster))

cases_TX_clust_race <- texas_state_census_df %>% 
  add_column(cluster = factor(kmeans_results$cluster))

counties_polygon_TX_clust <- right_join(counties_polygon_TX, cases_TX_clust_race, 
                                        join_by(county))

ggplot(counties_polygon_TX_clust, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = cluster)) +
  coord_quickmap() + 
  labs(title = "Texas Map with Ethnicity Clustered Counties")
#################### Grouping 2: Ages above and below 65 ####################

age_features <- c(
  "female_over_65_per_1000", 
  "male_over_65_per_1000", 
  "male_under_65_per_1000", 
  "female_under_65_per_1000"
  )

scaled_census_age_features <- scaled_tx_census_features %>%
  select(all_of(age_features))

# perform kmeans
plot_ideal_cluster_graph(scaled_census_age_features)

# dist_matrix <- dist(scaled_census_age_features)
k <- 3
kmeans_results <- kmeans(scaled_census_age_features, centers=k, nstart = 10)

# Assign cluster labels to dataset
scaled_census_age_features$cluster <- as.factor(kmeans_results$cluster)

# Assess cluster profiles
cluster_profiles(kmeans_results)

# plotting with PCA, UMAP, and tsne
visualize_multiDim_cluster(scaled_census_age_features, ncol(scaled_census_age_features))

cases_TX_clust_race <- texas_state_census_df %>% 
  add_column(cluster = factor(kmeans_results$cluster))

counties_polygon_TX_clust <- right_join(counties_polygon_TX, cases_TX_clust_race, 
                                        join_by(county))

ggplot(counties_polygon_TX_clust, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = cluster)) +
  coord_quickmap() + 
  labs(title = "Texas Map with Age Clustered Counties")
