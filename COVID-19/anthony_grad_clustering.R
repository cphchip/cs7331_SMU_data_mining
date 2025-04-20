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
library(mclust)
library(patchwork)
library(factoextra)
########################## Helper Functions ####################################
visualize_multiDim_cluster <- function(data, ncol) {
  # PCA
  pca <- prcomp(data[,-ncol(data)], center=TRUE, scale.=TRUE)
  df_pca <- data.frame(pca$x[,1:2], cluster = as.factor(data$cluster))
  
  g1 <- ggplot(df_pca, aes(x = PC1, y = PC2, color = cluster)) +
    geom_point(size = 3) +
    labs(title = "PCA") +
    theme_minimal()
  
  # t-SNE
  tsne_results <- Rtsne(data[, -ncol(data)], perplexity = 30, check_duplicates = FALSE)
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

# Since PAM doesnt have center; rather it uses its medoids
cluster_profiles_PAM <- function(results) {
  ggplot(pivot_longer(as_tibble(results$medoids,  rownames = "cluster"), 
                      cols = colnames(results$medoid)), 
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

purity <- function(cluster, truth, show_table = FALSE) {
  if (length(cluster) != length(truth))
    stop("Cluster vector and ground truth vectors are not of the same length!")
  
  # tabulate
  tbl <- table(cluster, truth)
  if(show_table)
    print(tbl)
  
  # find majority class
  majority <- apply(tbl, 1, max)
  sum(majority) / length(cluster)
}

entropy <- function(cluster, truth, show_table = FALSE) {
  if (length(cluster) != length(truth))
    stop("Cluster vector and ground truth vectors are not of the same length!")
  
  # calculate membership probability of cluster to class
  tbl <- table(cluster, truth)
  p <- sweep(tbl, 2, colSums(tbl), "/")
  
  if(show_table)
    print(p)
  
  # calculate cluster entropy
  e <- -p * log(p, 2)
  e <- rowSums(e, na.rm = TRUE)
  
  # weighted sum over clusters
  w <- table(cluster) / length(cluster)
  sum(w * e)
}

purity <- function(cluster, truth, show_table = FALSE) {
  if (length(cluster) != length(truth))
    stop("Cluster vector and ground truth vectors are not of the same length!")
  
  # tabulate
  tbl <- table(cluster, truth)
  if(show_table)
    print(tbl)
  
  # find majority class
  majority <- apply(tbl, 1, max)
  sum(majority) / length(cluster)
}

entropy <- function(cluster, truth, show_table = FALSE) {
  if (length(cluster) != length(truth))
    stop("Cluster vector and ground truth vectors are not of the same length!")
  
  # calculate membership probability of cluster to class
  tbl <- table(cluster, truth)
  p <- sweep(tbl, 2, colSums(tbl), "/")
  
  if(show_table)
    print(p)
  
  # calculate cluster entropy
  e <- -p * log(p, 2)
  e <- rowSums(e, na.rm = TRUE)
  
  # weighted sum over clusters
  w <- table(cluster) / length(cluster)
  sum(w * e)
}

clustering_method <- "ward.D2"

# manually compute wss
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

# plots wss using my compute_wss()
plot_wss_graph <- function(mydata, max_k, mytitle, type) {
  d <- dist(mydata)
  all_wss <- c()
  all_sil <- c()
  for (k in 2:max_k)
  {
    if (type == "PAM") {
      clusters <- pam(mydata, k)$cluster
    }
    else if (type == "GMM") {
      clusters <- Mclust(mydata, G=k)$classification
      
    } else {
      print("Unknown type.... Exiting")
      return()
    }
    
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
    #scale_x_discrete(limits = factor(1:max_k)) +
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

# emulate the call and return vlaue of kmeans() but for hierarchical clustering
hclustering <- function(mydata,ncenters) {
  # calculate dist matrix and builds dendrogram
  d <- dist(mydata)
  hc <- hclust(d, method=clustering_method)
  
  # just like kmeans, the result is a list of information
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

# function getting the silhouette value with just the clustering
# and distance matrix
getSIL <- function(cluster, mydist) {
  return(mean(silhouette(cluster, mydist)[,3]))
}

# performs the same functionality as better_compute_wss but also outputs the wss
# for each cluster
better_compute_wss_verbose <- function(data, clusters) {
  totwss <- 0
  for (k in unique(clusters)) {
    kwss <- 0
    cluster_points <- data[clusters == k, ]
    centroid <- colMeans(cluster_points)
    for (i in 1:nrow(cluster_points)) {
      wss <- 0
      for (j in 1:ncol(cluster_points)) {
        pt <- cluster_points[i,][j]
        wss <- wss + (pt[[1]][1] - centroid[[j]][1])^2
      }
      kwss <- kwss + wss
    }
    print(paste("Cluster ", k, ": ", kwss))
    totwss <- totwss + kwss
  }
  return(totwss)
}

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
    other_race_pop, two_or_more_races_pop, households, income_less_10000,                                             
    income_10000_14999, income_15000_19999, income_20000_24999,                                            
    income_25000_29999, income_30000_34999, income_35000_39999,                                            
    income_40000_44999, income_45000_49999, income_50000_59999,                                            
    income_60000_74999, income_75000_99999, income_100000_124999,                                          
    income_125000_149999, income_150000_199999, income_200000_or_more,
    employed_agriculture_forestry_fishing_hunting_mining,
    employed_arts_entertainment_recreation_accommodation_food,
    employed_construction, employed_education_health_social,                              
    employed_finance_insurance_real_estate, employed_information,                                          
    employed_manufacturing, employed_other_services_not_public_admin,                      
    employed_public_administration, employed_retail_trade,                                         
    employed_science_management_admin_waste,                       
    employed_transportation_warehousing_utilities,                 
    employed_wholesale_trade, unemployed_pop
  ) %>%
  filter(state == "TX") %>%
  select(where(~ !is.logical(.))) %>%
  na.omit()

#set randomness seed
set.seed(1015)

################################# Data Prep ##################################
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
  "employed_construction", "employed_education_health_social",                              
  "employed_finance_insurance_real_estate", "employed_information",                                          
  "employed_manufacturing", "employed_other_services_not_public_admin",                      
  "employed_public_administration", "employed_retail_trade",                                         
  "employed_science_management_admin_waste",
  "employed_transportation_warehousing_utilities", "employed_wholesale_trade",
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
texas_census_per_1000$percent_other    <- texas_census_per_1000$other_race_pop / texas_census_per_1000$total_pop * 100

# Add some age statistics columns
texas_census_per_1000$percent_men_over_65      <- texas_census_per_1000$male_over_65 / texas_census_per_1000$total_pop * 100
texas_census_per_1000$percent_men_under_65      <- texas_census_per_1000$male_under_65 / texas_census_per_1000$total_pop * 100
texas_census_per_1000$percent_women_over_65      <- texas_census_per_1000$female_over_65 / texas_census_per_1000$total_pop * 100
texas_census_per_1000$percent_women_under_65   <- texas_census_per_1000$female_under_65 / texas_census_per_1000$total_pop * 100


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

################################# Grouping 1: Ethnicity ########################
pop_features <- c(
  "white_pop_per_1000",
  "black_pop_per_1000",
  "asian_pop_per_1000", 
  "hispanic_pop_per_1000", 
  "amerindian_pop_per_1000", 
  "other_race_pop_per_1000"
)

# get the scaled features we care about
scaled_census_pop_features <- scaled_tx_census_features %>%
  select(all_of(pop_features))

# plot the wss and sil graph
# max k had to be 30 to see the elbow clearly
plot_wss_graph(scaled_census_pop_features, 30, "Ethnicity - PAM", "PAM")

# calcute distance matrix
dist_matrix <- dist(scaled_census_pop_features)

k <- 11 # determined by our plot of wss
pam_results <- pam(scaled_census_pop_features, k=k)

# make a copy of the scaled dataset before add the column for clusters
scaled_census_pop_features_clustered <- scaled_census_pop_features
scaled_census_pop_features_clustered$cluster <- as.factor(pam_results$cluster)

# plotting with PCA, UMAP, and tsne
visualize_multiDim_cluster(scaled_census_pop_features_clustered, ncol(scaled_census_pop_features_clustered))

# Unsupervise Analysis
# let see what the breakdown of each cluster is
cluster_profiles_PAM(pam_results)

# lets calculate the invidual wss for each cluster
better_compute_wss_verbose(scaled_census_pop_features, pam_results$cluster)

# count the number of elements in each cluster
for (i in 1:k) {
  print(sum(pam_results$cluster == i))
} 

# print out silhouette plot
p1 <- fviz_silhouette(silhouette(pam_results$cluster, dist_matrix)) +
  ggtitle("PAM Silhouette Plot") +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14)
  )
print(p1)

# Gaussian Mixture Model
# just to be on the safe side, regrab the selected fields
scaled_census_pop_features <- scaled_tx_census_features %>%
  select(all_of(pop_features))

# Mclust() can actual determine the necessay number of clusters to use
m <- Mclust(scaled_census_pop_features)
summary(m)

# lets visualize this new clustering in 2D space
scaled_census_pop_features_clustered <- scaled_census_pop_features
scaled_census_pop_features_clustered$cluster <- as.factor(m$classification)
visualize_multiDim_cluster(scaled_census_pop_features_clustered, ncol(scaled_census_pop_features_clustered))

# lets calculate the invidual wss for each cluster
better_compute_wss_verbose(scaled_census_pop_features, m$classification)

# count the number of elements in each cluster
for (i in 1:8) {
  print(sum(m$classification == i))
}

# print out silhouette plot
p1 <- fviz_silhouette(silhouette(m$classification, dist_matrix)) +
  ggtitle("GMM Silhouette Plot") +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14)
  )
print(p1)

## Pre Supervised Clustering
# lets start by force cluster number to better align with the number of selected features
m_results <- Mclust(scaled_census_pop_features, G=6)
pam_results <- pam(scaled_census_pop_features, k=6)


better_compute_wss_verbose(scaled_census_pop_features, pam_results$cluster)
p1 <- fviz_silhouette(silhouette(pam_results$cluster, dist_matrix)) +
  ggtitle("PAM Silhouette Plot") +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14)
  )
print(p1)


better_compute_wss_verbose(scaled_census_pop_features, m_results$classification)

p2 <- fviz_silhouette(silhouette(m_results$classification, dist_matrix)) +
  ggtitle("PAM Silhouette Plot") +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14)
  )
print(p1)


### Supervised Cluster Evaluation ###
# Create truth values
percent_race_features <- c(
  "percent_white",
  "percent_black",
  "percent_asian",
  "percent_hispanic",
  "percent_amerindian"
)

supervised_eval_df <- texas_census_per_1000 %>%
  select(
    "county",
    all_of(percent_race_features),
  )

cluster_profiles_PAM(pam_results)
supervised_eval_df$cluster <- as.factor(pam_results$cluster)

# Based on our cluster profiles, these appear to be the truth assignments
race_to_cluster <- c(
  percent_other      = 3,
  percent_white      = 2,
  percent_amerindian = 1,
  percent_black      = 5,
  percent_hispanic   = 4,
  percent_asian      = 6
)

# pull the percent race features from supervised_eval_df into race_percent_df
race_percent_df <- supervised_eval_df[percent_race_features]

# Create a column to hold the majority race for the county
max_race_name <- apply(race_percent_df, 1, function(row) {
  names(row)[which.max(row)]
})

# Create a dataframe for performing supervised evaluation
supervised_eval_df$max_race <- max_race_name
supervised_eval_df$truth_cluster <- race_to_cluster[supervised_eval_df$max_race]

# Using 4 and 7 here, just want to be different from 6 which was my Kmeans number
random_4 <- sample(1:4, nrow(scaled_census_pop_features), replace = TRUE)
random_7 <- sample(1:6, nrow(scaled_census_pop_features), replace = TRUE)

# Truth values as taken from my assignment from cluster profiles
truth <- as.integer(supervised_eval_df$truth_cluster)

# Code from Introduction to Data Mining 7.5. Modified for my content.
r <- rbind(
  truth = c(
    unlist(fpc::cluster.stats(dist_matrix, truth,
                              truth, compareonly = TRUE)),
    purity = purity(truth, truth),
    entropy = entropy(truth, truth)
  ),
  
  pam_6 = c(
    unlist(fpc::cluster.stats(dist_matrix, pam_results$cluster,
                              truth, compareonly = TRUE)),
    purity = purity(pam_results$cluster, truth),
    entropy = entropy(pam_results$cluster, truth)
  ),
  gmm_6 = c(
    unlist(fpc::cluster.stats(dist_matrix, m_results$classification,
                              truth, compareonly = TRUE)),
    purity = purity(m_results$classification, truth),
    entropy = entropy(m_results$classification, truth)
  ),
  random_4 = c(
    unlist(fpc::cluster.stats(dist_matrix, random_4,
                              truth, compareonly = TRUE)),
    purity = purity(random_4, truth),
    entropy = entropy(random_4, truth)
  ),
  random_7 = c(
    unlist(fpc::cluster.stats(dist_matrix, random_7,
                              truth, compareonly = TRUE)),
    purity = purity(random_7, truth),
    entropy = entropy(random_7, truth)
  )
)
print(r)

######################### Grouping 2: Age and Gender ###########################
age_features <- c(
  "female_over_65_per_1000", 
  "male_over_65_per_1000", 
  "male_under_65_per_1000", 
  "female_under_65_per_1000"
)

scaled_census_age_features <- scaled_tx_census_features %>%
  select(all_of(age_features))

plot_wss_graph(scaled_census_age_features, 10, "Ethnicity - PAM", "PAM")
m <- Mclust(scaled_census_age_features)
summary(m)

m <- Mclust(scaled_census_age_features, G=4)
summary(m)
plot(m, what="classification")

dist_matrix <- dist(scaled_census_age_features)

pam_results <- pam(scaled_census_age_features, k=4)

scaled_census_age_features_clustered_PAM <- scaled_census_age_features
scaled_census_age_features_clustered_GMM <- scaled_census_age_features
scaled_census_age_features_clustered_PAM$cluster <- as.factor(pam_results$cluster)
scaled_census_age_features_clustered_GMM$cluster <- as.factor(m$classification)

# plotting with PCA, UMAP, and tsne
visualize_multiDim_cluster(scaled_census_age_features_clustered_PAM, ncol(scaled_census_age_features_clustered_PAM))
visualize_multiDim_cluster(scaled_census_age_features_clustered_GMM, ncol(scaled_census_age_features_clustered_GMM))

better_compute_wss_verbose(scaled_census_age_features, pam_results$cluster)
better_compute_wss_verbose(scaled_census_age_features, m$classification)

for (i in 1:4) {
  print(sum(pam_results$cluster == i))
}
for (i in 1:4) {
  print(sum(m$classification == i))
}

sil <- silhouette(pam_results$cluster, dist_matrix)
p1 <- fviz_silhouette(silhouette(pam_results$cluster, dist_matrix)) +
  ggtitle("PAM Silhouette Plot") +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14)
  )
print(p1)

sil <- silhouette(m$classification, dist_matrix)
p2 <- fviz_silhouette(silhouette(m$classification, dist_matrix)) +
  ggtitle("GMM Silhouette Plot") +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14)
  )
print(p2)

percent_age_features <- c(
  "percent_men_over_65",
  "percent_men_under_65",
  "percent_women_over_65",
  "percent_women_under_65"
)

supervised_eval_df <- texas_census_per_1000 %>%
  select(
    "county",
    all_of(percent_age_features),
  )
summary(supervised_eval_df)

supervised_eval_df$cluster <- as.factor(pam_results$cluster)

# Based on our cluster profiles, these appear to be the truth assignments
age_to_cluster <- c(
  percent_men_over_65    = 2,
  percent_men_under_65   = 4,
  percent_women_over_65  = 3,
  percent_women_under_65 = 1
)

# pull the percent race features from supervised_eval_df into race_percent_df
age_percent_df <- supervised_eval_df[percent_age_features]

# Create a column to hold the majority race for the county
max_age_name <- apply(age_percent_df, 1, function(row) {
  names(row)[which.max(row)]
})

# Create a dataframe for performing supervised evaluation
supervised_eval_df$max_age <- max_age_name
supervised_eval_df$truth_cluster <- age_to_cluster[supervised_eval_df$max_age]

head(supervised_eval_df)

# Using 4 and 7 here, just want to be different from 6 which was my Kmeans number
random_4 <- sample(1:4, nrow(scaled_census_age_features), replace = TRUE)
random_7 <- sample(1:6, nrow(scaled_census_age_features), replace = TRUE)

# Truth values as taken from my assignment from cluster profiles
truth <- as.integer(supervised_eval_df$truth_cluster)

# Code from Introduction to Data Mining 7.5. Modified for my content.
r <- rbind(
  truth = c(
    unlist(fpc::cluster.stats(dist_matrix, truth,
                              truth, compareonly = TRUE)),
    purity = purity(truth, truth),
    entropy = entropy(truth, truth)
  ),
  
  pam_6 = c(
    unlist(fpc::cluster.stats(dist_matrix, pam_results$cluster,
                              truth, compareonly = TRUE)),
    purity = purity(pam_results$cluster, truth),
    entropy = entropy(pam_results$cluster, truth)
  ),
  gmm_6 = c(
    unlist(fpc::cluster.stats(dist_matrix, m$classification,
                              truth, compareonly = TRUE)),
    purity = purity(m$classification, truth),
    entropy = entropy(m$classification, truth)
  ),
  random_4 = c(
    unlist(fpc::cluster.stats(dist_matrix, random_4,
                              truth, compareonly = TRUE)),
    purity = purity(random_4, truth),
    entropy = entropy(random_4, truth)
  ),
  random_7 = c(
    unlist(fpc::cluster.stats(dist_matrix, random_7,
                              truth, compareonly = TRUE)),
    purity = purity(random_7, truth),
    entropy = entropy(random_7, truth)
  )
)
print(r)

