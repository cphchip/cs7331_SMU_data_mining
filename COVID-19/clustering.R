library("tidyverse")
library("ggplot2")
library(dplyr)
library(conflicted)
library(Rtsne)
library(umap)
conflicts_prefer(dplyr::filter)

#set randomness seed
set.seed(1015)

##################### Helper functions
getWSS <- function(k, data){
 kmeans(data,centers=k,nstart=10)$tot.withinss
}

# plot wss
plot_ideal_cluster_graph <- function(data) {
  k_values <- 1:10
  print("1")
  wss_values <- sapply(k_values, getWSS, data=data)
  print("2")
  
  df <- data.frame(wss = wss_values,
                   k_values=k_values)
  
  print("3")
  
  ggplot(df, aes(x=k_values,y=wss)) +
    geom_line(color="blue", size=1) +
    geom_point(color="red",size=2) +
    labs(title="WSS vs Number of Clusters", x="Number of Clusters (k)",y="Sum of Squares") +
    theme_minimal()
}

# visualizes clusters bases on more than 2 dimensions
# Visual 1: PCA
# Visual 2: Non-Linear Dimension Reduction
# Visual 3: Uniform Manifold Approximation and Projection
#
# requires that data has already been clustered
visualize_multiDim_cluster <- function(data, ncol) {
  # PCA
  pca <- prcomp(data[,-ncol], center=TRUE, scale.=TRUE)
  df_pca <-data.frame(pca$x[,1:2],cluster=as.factor(data$cluster))
  
  g1<-ggplot(df_pca, aes(x = PC1, y = PC2, color = cluster)) +
    geom_point(size = 3) +
    labs(title = "PCA Visualization of Clusters") +
    theme_minimal()
  
  # t-SNE
  tsne_results <- Rtsne(data, perplexity=30,check_duplicates=FALSE)
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

##################### Analysis

census_cases_df <- read_csv("COVID-19_cases_plus_census.csv")
census_tx <- census_cases_df %>% 
  filter(state== "TX") %>%
  select(where(~ !is.logical(.))) %>%
  na.omit(census_tx)

county_names <- census_tx$county_name
county_cases <- census_tx$confirmed_cases
county_deaths <- census_tx$deaths
county_total_pop <- census_tx$total_pop

# remove non-numeric and specific columns
census_numeric <- census_tx %>%
  select(where(is.numeric)) %>%
  select(-confirmed_cases) %>%
  select(-deaths) %>%
  select(-do_date)

# save a copy of census_numeric
og_census_numeric <- census_numeric

############### reset point ##################
census_numeric <- og_census_numeric

##### Grouping 1: Ethnic makeup of the county
cols_to_select <- c("total_pop","white_pop", "black_pop", "asian_pop", "hispanic_pop", "amerindian_pop","other_race_pop")
sel_census_numeric <- census_numeric %>%
    select(all_of(cols_to_select))

sel_census_numeric$white_pop_percent      <- sel_census_numeric$white_pop / sel_census_numeric$total_pop
sel_census_numeric$black_pop_percent      <- sel_census_numeric$black_pop / sel_census_numeric$total_pop
sel_census_numeric$asian_pop_percent      <- sel_census_numeric$asian_pop / sel_census_numeric$total_pop
sel_census_numeric$hispanic_pop_percent   <- sel_census_numeric$hispanic_pop / sel_census_numeric$total_pop
sel_census_numeric$amerindian_pop_percent <- sel_census_numeric$amerindian_pop / sel_census_numeric$total_pop
sel_census_numeric$other_race_pop_percent <- sel_census_numeric$other_race_pop / sel_census_numeric$total_pop

sel_census_numeric <- sel_census_numeric %>%
  select(-all_of(cols_to_select))

# kmeans
plot_ideal_cluster_graph(sel_census_numeric) # selecting 3 or 4
kmeans_results <- kmeans(sel_census_numeric, centers=3)
sel_census_numeric$cluster <- as.factor(kmeans_results$cluster)

#plotting with PCA
visualize_multiDim_cluster(sel_census_numeric, 7)

# hiearchical clustering
d <- dist(sel_census_numeric)
hc <- hclust(d)
plot(hc, main="Hierarchical Clustering Dendrogram")
clusters <- cutree(hc, k = 3)
sel_census_numeric$cluster <- clusters
visualize_multiDim_cluster(sel_census_numeric, 7)

##### Grouping 2: Age and Gender makeup of the county

##### Grouping 3: Employment makeup of the county

##### Grouping 4: Income Makeup of the county
