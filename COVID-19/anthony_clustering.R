library(tidyverse)
library(ggplot2)
library(GGally)

library(dplyr)
library(conflicted)
library(Rtsne)
library(umap)
conflicts_prefer(dplyr::filter)

set.seed(1015)

##################### Helper functions ######################
# gets wss using kmeans
getWSS <- function(k, data){
  kmeans(data,centers=k,nstart=10)$tot.withinss
}

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

# plot wss using getWSS. hard codes that max cluster limit to be 10
plot_ideal_cluster_graph <- function(data) {
  k_values <- 1:10
  wss_values <- sapply(k_values, getWSS, data=data)
  print(wss_values)
  df <- data.frame(wss = wss_values,
                   k_values=k_values)
  
  
  ggplot(df, aes(x=k_values,y=wss)) +
    geom_line(color="blue", size=1) +
    geom_point(color="red",size=2) +
    labs(title="WSS vs Number of Clusters", x="Number of Clusters (k)",y="Sum of Squares") +
    theme_minimal()
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
  print(g1)
  print(g2)
  print(g3)
}

# plots wss using my compute_wss()
plot_wss_graph <- function(mydata, max_k, mytitle) {
  d <- dist(mydata)
  hc <- hclust(d)
  all_wss <- c()
  for (K in 1:max_k)
  {
    clusters <- cutree(hc, k=K)
    wss_total <- compute_wss(mydata, clusters)
    all_wss <- c(all_wss, wss_total)
  }
  k_values <- 1:max_k
  
  df <- data.frame(wss = all_wss,
                   k_values=k_values)
  
  g <- ggplot(df, aes(x=k_values,y=wss)) +
    geom_line(color="blue", size=1) +
    geom_point(color="red",size=2) +
    labs(title=paste("WSS vs Number of Clusters: ",mytitle), x="Number of Clusters (k)",y="Sum of Squares") +
    scale_x_discrete(limits = factor(1:10)) +
    theme_minimal()
  print(g)
}

# assumes a Texas state census df exist. See Data prep section for 
# details on how it was created
run_hierarchical_clustering <- function(features, mytitle) {
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
  
  cases_TX_clust %>% group_by(cluster) %>% summarize(
    avg_cases_per_1000 = mean(cases_per_1000), 
    avg_deaths_per_1000 = mean(deaths_per_1000))
  
  visualize_multiDim_cluster(cases_TX_scaled, ncol(cases_TX_scaled), mytitle)
}

hclustering <- function(mydata,centers) {
  d <- dist(mydata)
  hc <- hclust(d)
  
  results <- list()
  
  # results$cluster = vectors of integers indicating which cluster each observation belongs to
  clusters <- cutree(hc, centers)
  results$cluster <- clusters
  
  # results$center = a matrix of cluster centers, one row for each cluster
  center <- matrix(numeric(0),nrow=0,ncol=ncol(mydata))
  for (k in unique(clusters)) {
    cluster_points <- mydata[clusters == k, , drop = FALSE]
    center <- rbind(center,colMeans(cluster_points))
  }
  results$center <- center
  return(results)
}

#############################################################


##################### Feature Selection #####################
## Subset 1: Employment
employed_features <- c(
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

## Subset 2: Ethnicity 
ethnic_features <- c(
  "amerindian_pop",
  "other_race_pop",
  "white_pop",
  "hispanic_pop",
  "black_pop",
  "asian_pop"
)

## Subset 3: Household incomes
income_features <- c(
 "households",
 "income_less_10000",                                             
 "income_10000_14999",                                            
 "income_15000_19999",                                            
 "income_20000_24999",                                            
 "income_25000_29999",                                            
 "income_30000_34999",                                            
 "income_35000_39999",                                            
 "income_40000_44999",                                            
 "income_45000_49999",                                            
 "income_50000_59999",                                            
 "income_60000_74999",                                            
 "income_75000_99999",                                            
 "income_100000_124999",                                          
 "income_125000_149999",                                          
 "income_150000_199999",                                          
 "income_200000_or_more"
)

## Subset 4: Over than 65 years old
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

age_features <- c(
  "male_65_and_over",
  "female_65_and_over"
)

##################### Data Preparation ######################
#' Because the majority of our data is based on Texas, we'll start with that
#' portion of the data for examination.
census_cases_df <- read_csv("COVID-19_cases_plus_census.csv")
texas_state_census_df <- census_cases_df %>% 
  filter(state== "TX") %>%
  select(where(~ !is.logical(.))) %>%
  na.omit(census_tx)

texas_state_census_df <- texas_state_census_df %>% mutate(across(where(is.character), factor))
dim(texas_state_census_df)

# Since we know that Covid disproportionately impacts age 65+, lets combine the 
# age range of male and female 65 and over
# Create new variables for combined age population, drop old ones
texas_state_census_df <- texas_state_census_df %>%
  mutate(male_65_and_over = rowSums(across(male_elderly_features))) %>%
  mutate(female_65_and_over = rowSums(across(female_elderly_features))) %>%
  mutate(pop_over_65 = male_65_and_over + female_65_and_over) %>%
  select(
    -male_elderly_features, 
    -female_elderly_features,
  )

counties_polygon <- as_tibble(map_data("county"))
counties_polygon_TX <- counties_polygon %>% dplyr::filter(region == "texas") %>% 
  rename(c(county = subregion)) 
counties_polygon_TX

texas_state_census_df <- texas_state_census_df %>% mutate(county = county_name %>% 
                                                            str_to_lower() %>% str_replace('\\s+county\\s*$', ''))
texas_state_census_df %>% pull(county)

counties_polygon_TX <- right_join(counties_polygon_TX, texas_state_census_df)

# Calculate cases, deaths per thousand people
#######################################################################
# This portion of code came from the getting started content from class
#######################################################################
texas_state_census_df <- texas_state_census_df %>% mutate(
  cases_per_1000 = confirmed_cases/total_pop*1000, 
  deaths_per_1000 = deaths/total_pop*1000, 
  deaths_per_case = deaths/confirmed_cases)

g <- ggplot(counties_polygon_TX, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = deaths_per_1000)) +
  coord_quickmap() +
  scale_fill_continuous(type = "viridis") +
  labs(title = "Texas death rates by county")
print(g)

##################### Analysis ############################## 
run_hierarchical_clustering(ethnic_features, "Ethnicity")
run_hierarchical_clustering(employed_features, "Employment")
run_hierarchical_clustering(income_features, "Household Income")
run_hierarchical_clustering(age_features, "Over 65 years old")

