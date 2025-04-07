library(tidyverse)
library(ggplot2)
library(GGally)

library(dplyr)
library(conflicted)
library(Rtsne)
library(umap)
conflicts_prefer(dplyr::filter)
library(gridExtra)
library(cluster)
library(seriation)

##################### Data Import############################ ################################# 

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
    two_or_more_races_pop,
    households,
    income_less_10000,                                             
    income_10000_14999,                                            
    income_15000_19999,                                            
    income_20000_24999,                                            
    income_25000_29999,                                            
    income_30000_34999,                                            
    income_35000_39999,                                            
    income_40000_44999,                                            
    income_45000_49999,                                            
    income_50000_59999,                                            
    income_60000_74999,                                            
    income_75000_99999,                                            
    income_100000_124999,                                          
    income_125000_149999,                                          
    income_150000_199999,                                          
    income_200000_or_more,
    employed_agriculture_forestry_fishing_hunting_mining,
    employed_arts_entertainment_recreation_accommodation_food,     
    employed_construction,                                         
    employed_education_health_social,                              
    employed_finance_insurance_real_estate,                        
    employed_information,                                          
    employed_manufacturing,                                        
    employed_other_services_not_public_admin,                      
    employed_public_administration,                                
    employed_retail_trade,                                         
    employed_science_management_admin_waste,                       
    employed_transportation_warehousing_utilities,                 
    employed_wholesale_trade,
    unemployed_pop
  ) %>%
  filter(state == "TX") %>%
  select(where(~ !is.logical(.))) %>%
  na.omit()

#set randomness seed
set.seed(1015)

##################### Helper functions ######################

clustering_method <- "ward.D2"

# manually compute wss
compute_wss <- function(data, clusters){
  wss <- 0
  for (k in unique(clusters)) {
    cluster_points <- data[clusters == k, , drop = FALSE]
    centroid <- colMeans(cluster_points)
    wss <- wss + sum(rowSums((cluster_points - centroid)^2))
  }
  return(wss)
}

better_compute_wss <- function(data, clusters) {
  totwss <- 0
  for (k in unique(clusters)) {
    cluster_points <- data[clusters == k, ]
    centroid <- colMeans(cluster_points)
    for (i in 1:nrow(cluster_points)) {
      wss <- 0
      for (j in 1:ncol(cluster_points)) {
        pt <- cluster_points[i,][j]
        wss <- wss + (pt[[1]][1] - centroid[[j]][1])^2
      }
      totwss <- totwss + wss
    }
  }
  return(totwss)
}

# visualizes clusters bases on more than 2 dimensions
# Visual 1: 
# Visual 2: Non-Linear Dimension Reduction
# Visual 3: Uniform Manifold Approximation and Projection
#
# requires that data has already been clustered
visualize_multiDim_cluster <- function(data, ncol, mytitle) {
  # PCA
  pca <- prcomp(data[,-ncol], center=TRUE, scale.=TRUE)
  df_pca <-data.frame(pca$x[,1:2],cluster=as.factor(data$cluster))
  
  g1<-ggplot(df_pca, aes(x = PC1, y = PC2, color = cluster)) +
    geom_point(size = 3) +
    labs(title = paste("PCA Visualization of Clusters - ",mytitle)) +
    theme_minimal()
  
  # t-SNE
  tsne_results <- Rtsne(data, perplexity=30,check_duplicates=FALSE)
  df_tsne <- data.frame(tsne_results$Y, cluster=as.factor(data$cluster))
  
  g2<- ggplot(df_tsne, aes(x = X1, y = X2, color = cluster)) +
    geom_point(size = 3) +
    labs(title = paste("t-SNE Visualization of Clusters - ",mytitle)) +
    theme_minimal()
  
  # umap
  umap_result <- umap(data[,-ncol])
  df_umap <- data.frame(umap_result$layout, cluster = as.factor(data$cluster))
  
  g3<-ggplot(df_umap, aes(x = X1, y = X2, color = cluster)) +
    geom_point(size = 3) +
    labs(title = paste("UMAP Visualization of Clusters - ",mytitle)) +
    theme_minimal()
  
  grid.arrange(g1, g2, g3, ncol=3)
}

# plots wss using my compute_wss()
plot_wss_graph <- function(mydata, max_k, mytitle) {
  d <- dist(mydata)
  hc <- hclust(d, method=clustering_method)
  all_wss <- c()
  all_sil <- c()
  for (k in 2:max_k)
  {
    
    clusters <- cutree(hc, k=k)
    
    # using wss and elbow
    wss_total <- better_compute_wss(mydata, clusters)
    all_wss <- c(all_wss, wss_total)
    
    # using sil
    sil <- getSIL(clusters,d)
    all_sil <- c(all_sil, sil)
  }
  k_values <- 2:max_k
  
  df1 <- data.frame(wss = all_wss,
                   k_values=k_values)
  
  # elbow method using wss
  g1 <- ggplot(df1, aes(x=k_values,y=wss)) +
    geom_line(color="blue", size=1) +
    geom_point(color="red",size=2) +
    labs(title=paste("WSS vs Number of Clusters: ",mytitle), x="Number of Clusters (k)",y="Sum of Squares") +
    scale_x_discrete(limits = factor(1:10)) +
    theme_minimal() + 
    theme(
      plot.title = element_text(size = 18, face = "bold"),
      axis.title = element_text(size = 16),
      axis.text = element_text(size = 14)
    )
  
  df2 <- data.frame(sil = all_sil,
                    k_values=k_values)
  
  g2 <- ggplot(df2, aes(x=k_values, y=sil)) +
    geom_point(color = "blue") +
    geom_line(color = "blue") +
    labs(
      title = paste("Silhouette Score vs Number of Clusters", mytitle),
      x = "Number of Clusters",
      y = "Silhouette Score"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 18, face = "bold"),
      axis.title = element_text(size = 16),
      axis.text = element_text(size = 14) 
      )
  
  # arrange in a row
  grid.arrange(g1, g2, ncol=2)
  
  
}

# assumes a Texas state census df exist. See Data prep section for 
# details on how it was created
run_hierarchical_clustering_v1 <- function(features, mytitle) {
  
  #texas_census_per_1000
  #texas_state_census_df
  cases_TX_scaled <- texas_state_census_df %>% 
    select(features) %>% 
    scale() %>%
    as_tibble()
  
  plot_wss_graph(cases_TX_scaled, 10, mytitle)
  
  # Now that we have determined the best clusters to create
  # Perform the actual clustering
  hclust <- hclustering(cases_TX_scaled, 4)
  g1 <- ggplot(pivot_longer(as_tibble(hclust$center,  rownames = "cluster"), cols = colnames(hclust$center)), 
         aes(y = name, x = value, fill = cluster)) +
         geom_bar(stat = "identity") +
         facet_grid(cols = vars(cluster)) +
         labs(y = "feature", x = "z-scores", title = paste("Cluster Profiles",mytitle)) + 
         guides(fill="none")
  print(g1)
  
  cases_TX_clust <- texas_state_census_df %>% 
    add_column(cluster = factor(hclust$cluster))
  cases_TX_scaled$cluster <- hclust$cluster
  
  counties_polygon_TX_clust <- right_join(counties_polygon_TX, cases_TX_clust, 
                                          join_by(county))
  
  g2 <- ggplot(counties_polygon_TX_clust, aes(long, lat)) + 
    geom_polygon(aes(group = group, fill = cluster)) +
    coord_quickmap() + 
    labs(title = paste("Texas Map with clustered counties - ", mytitle))
  
  visualize_multiDim_cluster(cases_TX_scaled, ncol(cases_TX_scaled), mytitle)
}

hclustering <- function(mydata,ncenters) {
  d <- dist(mydata)
  hc <- hclust(d, method=clustering_method)
  
  results <- list()
  
  # results$cluster = vectors of integers indicating which cluster each observation belongs to
  clusters <- cutree(hc, ncenters)
  results$cluster <- clusters
  
  # results$center = a matrix of cluster centers, one row for each cluster
  center <- matrix(numeric(0),nrow=0,ncol=ncol(mydata))
  for (k in unique(clusters)) {
    cluster_points <- mydata[clusters == k, , drop = FALSE]
    center <- rbind(center,colMeans(cluster_points))
  }
  results$centers <- center
  return(results)
}

getSIL <- function(cluster, mydist) {
  return(mean(silhouette(cluster, mydist)[,3]))
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

run_hierarchical_clustering <- function(scaled_census_feature, mytitle) {
  cases_TX_scaled <- scaled_census_feature
  
  plot_wss_graph(cases_TX_scaled, 10, mytitle)
  
  # Now that we have determined the best clusters to create
  # Perform the actual clustering
  hclust <- hclustering(cases_TX_scaled, 4)
  g1 <- ggplot(pivot_longer(as_tibble(hclust$center,  rownames = "cluster"), cols = colnames(hclust$center)), 
               aes(y = name, x = value, fill = cluster)) +
    geom_bar(stat = "identity") +
    facet_grid(cols = vars(cluster)) +
    labs(y = "feature", x = "z-scores", title = paste("Cluster Profiles",mytitle)) + 
    guides(fill="none")
  print(g1)
  
  cases_TX_clust <- texas_state_census_df %>% 
    add_column(cluster = factor(hclust$cluster))
  cases_TX_scaled$cluster <- hclust$cluster
  
  counties_polygon_TX_clust <- right_join(counties_polygon_TX, cases_TX_clust, 
                                          join_by(county))
  
  g2 <- ggplot(counties_polygon_TX_clust, aes(long, lat)) + 
    geom_polygon(aes(group = group, fill = cluster)) +
    coord_quickmap() + 
    labs(title = paste("Texas Map with clustered counties - ", mytitle))
  
  visualize_multiDim_cluster(cases_TX_scaled, ncol(cases_TX_scaled), mytitle)
}

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

cols_to_convert <- c(
  "deaths", "confirmed_cases", "white_pop", "black_pop", "asian_pop", 
  "hispanic_pop", "amerindian_pop", "other_race_pop", "two_or_more_races_pop", 
  "female_over_65", "male_over_65", "male_pop", "female_pop", "male_under_65", 
  "female_under_65", "pop_under_65", "households", "income_less_10000",
  "income_10000_14999", "income_15000_19999", "income_20000_24999",
  "income_25000_29999", "income_30000_34999", "income_35000_39999",
  "income_40000_44999", "income_45000_49999", "income_50000_59999",                                            
  "income_60000_74999", "income_75000_99999", "income_100000_124999",
  "income_125000_149999", "income_150000_199999", "income_200000_or_more",
  "employed_agriculture_forestry_fishing_hunting_mining",
  "employed_arts_entertainment_recreation_accommodation_food",     
  "employed_construction",                                         
  "employed_education_health_social",                              
  "employed_finance_insurance_real_estate",                        
  "employed_information",                                          
  "employed_manufacturing",                                        
  "employed_other_services_not_public_admin",                      
  "employed_public_administration",                                
  "employed_retail_trade",                                         
  "employed_science_management_admin_waste",                       
  "employed_transportation_warehousing_utilities",                 
  "employed_wholesale_trade",
  "unemployed_pop"
)

# Apply the 'per 1000' conversion to all cols_to_convert
texas_census_per_1000 <- texas_state_census_df %>%
  mutate(across(
    all_of(cols_to_convert),
    ~ .x / total_pop * 1000,
    .names = "{.col}_per_1000"
  ))

# Add some pop statistics columns
texas_census_per_1000$percent_white      <- texas_census_per_1000$white_pop / texas_census_per_1000$total_pop * 100
texas_census_per_1000$percent_black      <- texas_census_per_1000$black_pop / texas_census_per_1000$total_pop * 100
texas_census_per_1000$percent_asian      <- texas_census_per_1000$asian_pop / texas_census_per_1000$total_pop * 100
texas_census_per_1000$percent_hispanic   <- texas_census_per_1000$hispanic_pop / texas_census_per_1000$total_pop * 100
texas_census_per_1000$percent_amerindian <- texas_census_per_1000$amerindian_pop / texas_census_per_1000$total_pop * 100


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
  select(all_of(pop_features)) %>% drop_na()

dist_matrix <- dist(scaled_census_pop_features)

plot_wss_graph(scaled_census_pop_features, 10, "ethnicity") # looks like 6 is a best number of clustering

hier_results <- hclustering(scaled_census_pop_features, 6)
scaled_census_pop_features$cluster <- as.factor(hier_results$cluster)

sil <- silhouette(hier_results$cluster, dist_matrix)
plot(sil)

cluster_profiles(hier_results)

visualize_multiDim_cluster(scaled_census_pop_features, ncol(scaled_census_pop_features), "Ethnicity")

ggpimage(dist_matrix, order=order(hier_results$cluster))

cases_TX_clust_race <- texas_state_census_df %>% 
  add_column(cluster = factor(hier_results$cluster))

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

dist_matrix <- dist(scaled_census_age_features)

plot_wss_graph(scaled_census_age_features, 10, "Age") # looks like 4 is a best number of clustering

hier_results <- hclustering(scaled_census_age_features, 4)
scaled_census_age_features$cluster <- as.factor(hier_results$cluster)

sil <- silhouette(hier_results$cluster, dist_matrix)
plot(sil)

cluster_profiles(hier_results)

visualize_multiDim_cluster(scaled_census_age_features, ncol(scaled_census_age_features), "Age")

ggpimage(dist_matrix, order=order(hier_results$cluster))

cases_TX_clust_race <- texas_state_census_df %>% 
  add_column(cluster = factor(hier_results$cluster))

counties_polygon_TX_clust <- right_join(counties_polygon_TX, cases_TX_clust_race, 
                                        join_by(county))

ggplot(counties_polygon_TX_clust, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = cluster)) +
  coord_quickmap() + 
  labs(title = "Texas Map with Age Clustered Counties")

############################# Grouping 3: Income ##############################

income_features <- c(
  "households_per_1000",
  "income_less_10000_per_1000",
  "income_10000_14999_per_1000",
  "income_15000_19999_per_1000",
  "income_20000_24999_per_1000",
  "income_25000_29999_per_1000",
  "income_30000_34999_per_1000",
  "income_35000_39999_per_1000",
  "income_40000_44999_per_1000",
  "income_45000_49999_per_1000",
  "income_50000_59999_per_1000",
  "income_60000_74999_per_1000",
  "income_75000_99999_per_1000",
  "income_100000_124999_per_1000",
  "income_125000_149999_per_1000",
  "income_150000_199999_per_1000",
  "income_200000_or_more_per_1000"
)

scaled_census_income_features <- scaled_tx_census_features %>%
  select(all_of(income_features))

dist_matrix <- dist(scaled_census_income_features)

plot_wss_graph(scaled_census_income_features, 10, "Income") # looks like 3 is a best number of clustering

hier_results <- hclustering(scaled_census_income_features, 3)
scaled_census_income_features$cluster <- as.factor(hier_results$cluster)

sil <- silhouette(hier_results$cluster, dist_matrix)
plot(sil)

cluster_profiles(hier_results)

visualize_multiDim_cluster(scaled_census_income_features, ncol(scaled_census_income_features), "Income")

ggpimage(dist_matrix, order=order(hier_results$cluster))

cases_TX_clust_race <- texas_state_census_df %>% 
  add_column(cluster = factor(hier_results$cluster))

counties_polygon_TX_clust <- right_join(counties_polygon_TX, cases_TX_clust_race, 
                                        join_by(county))

ggplot(counties_polygon_TX_clust, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = cluster)) +
  coord_quickmap() + 
  labs(title = "Texas Map with Income Clustered Counties")

########################### Grouping 4: Employment ###########################
job_features <- c(
  "employed_agriculture_forestry_fishing_hunting_mining",
  "employed_arts_entertainment_recreation_accommodation_food",     
  "employed_construction",                                         
  "employed_education_health_social",                              
  "employed_finance_insurance_real_estate",                        
  "employed_information",                                          
  "employed_manufacturing",                                        
  "employed_other_services_not_public_admin",                      
  "employed_public_administration",                                
  "employed_retail_trade",                                         
  "employed_science_management_admin_waste",                       
  "employed_transportation_warehousing_utilities",                 
  "employed_wholesale_trade",
  "unemployed_pop"
)

scaled_census_job_features <- scaled_tx_census_features %>%
  select(all_of(job_features))

dist_matrix <- dist(scaled_census_job_features)

plot_wss_graph(scaled_census_job_features, 10, "Employment") # looks like 4 is a best number of clustering

hier_results <- hclustering(scaled_census_job_features, 4)
scaled_census_job_features$cluster <- as.factor(hier_results$cluster)

sil <- silhouette(hier_results$cluster, dist_matrix)
plot(sil)

cluster_profiles(hier_results)

visualize_multiDim_cluster(scaled_census_job_features, ncol(scaled_census_job_features), "Employment")

ggpimage(dist_matrix, order=order(hier_results$cluster))

cases_TX_clust_race <- texas_state_census_df %>% 
  add_column(cluster = factor(hier_results$cluster))

counties_polygon_TX_clust <- right_join(counties_polygon_TX, cases_TX_clust_race, 
                                        join_by(county))

ggplot(counties_polygon_TX_clust, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = cluster)) +
  coord_quickmap() + 
  labs(title = "Texas Map with Income Clustered Counties")
