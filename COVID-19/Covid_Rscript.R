library("tidyverse")
library("ggplot2")
library("ggrepel")
library("ggcorrplot")
library("DT")
library(dplyr)
library(conflicted)
conflicts_prefer(dplyr::filter)

census_data <- read_csv("COVID-19_cases_plus_census.csv")
#' Contains snapshot of cases and deaths on Jan 19 2021

# Lets just filter based on Texas since we also have covid set for texas only
# also remove the logical field, they are blank in the actual census report
# drop all rows with missing elements
census_tx <- census_data %>% 
  filter(state== "TX") %>%
  select(where(~ !is.logical(.))) %>%
  na.omit(census_tx)
  
# save the county names, confirmed cases, and deaths
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

summary(census_numeric)

# select features of interest
cols_to_select <- c("male_pop","female_pop")
census_numeric <- census_numeric %>%
  select(all_of(cols_to_select))

summary(census_numeric)
census_numeric

set.seed(1015)

# kmean clustering on data
# Using the Elbow Method to determine the best number of clusters
getWSS <- function(k){
  kmeans(census_numeric,centers=k,nstart=10)$tot.withinss
}

k_values <- 1:10
wss_values <- sapply(k_values,getWSS)
df <- data.frame(
  wss = wss_values,
  k_values=k_values
)
ggplot(df, aes(x=k_values,y=wss)) +
  geom_line(color="blue", size=1) +
  geom_point(color="red",size=2) +
  labs(title="WSS vs Number of Clusters", x="Number of Clusters (k)",y="Sum of Squares") +
  theme_minimal()

# based on the graph, the best number for k is 3. Now create perform k means cluster
kmeans_result <- kmeans(census_numeric, centers = 3)
census_numeric$cluster <- as.factor(kmeans_result$cluster)
census_numeric$confirmed_cases <- county_cases
census_numeric$total_pop <- county_total_pop

summary(census_numeric)
 
ggplot(census_numeric, aes(x=census_numeric$male_pop, 
                           y=census_numeric$female_pop, 
                           color=census_numeric$cluster)) +
  geom_point(size = 3)

census_numeric$county_name <- county_names

cluster3 <- census_numeric %>% filter(cluster == 3)
cluster1 <- census_numeric %>% filter(cluster == 1)
cluster2 <- census_numeric %>% filter(cluster == 2)

cluster1$confirmed_cases_percent <- cluster1$confirmed_cases / cluster1$total_pop
cluster2$confirmed_cases_percent <- cluster2$confirmed_cases / cluster2$total_pop
cluster3$confirmed_cases_percent <- cluster3$confirmed_cases / cluster3$total_pop


size <- c(2340,3250,2350,2674)
cluster2$size <- size
cluster2$pop_den <- cluster2$total_pop / cluster2$size

cluster2

print(cluster1)
print(cluster2)
print(cluster3)
print(max(cluster3$confirmed_cases_percent))
print(min(cluster3$confirmed_cases_percent))




# Only use this section if there are more than two dimensions
# Since there are multiple dimensions we cannot display a visualization of the
# clusters without performing a dimension reduction technique

# PCA (Principal Componenet Analysis)
pca <- prcomp(census_numeric[,-241], center=TRUE, scale.=TRUE)
df_pca <-data.frame(pca$x[,1:2],cluster=as.factor(census_numeric$cluster))

ggplot(df_pca, aes(x = PC1, y = PC2, color = cluster)) +
  geom_point(size = 3) +
  labs(title = "PCA Visualization of Clusters") +
  theme_minimal()

# t-SNE (Non-Linear Dimensionality Reduction)
library(Rtsne)
tsne_results <- Rtsne(census_numeric, perplexity=30,check_duplicates=FALSE)
df_tsne <- data.frame(tsne_results$Y, cluster=as.factor(census_numeric$cluster))

ggplot(df_tsne, aes(x = X1, y = X2, color = cluster)) +
  geom_point(size = 3) +
  labs(title = "t-SNE Visualization of Clusters") +
  theme_minimal()

# umap (Uniform Manifold Approximation and Projection)
library(umap)
umap_result <- umap(census_numeric[,-241])
df_umap <- data.frame(umap_result$layout, cluster = as.factor(census_numeric$cluster))

ggplot(df_umap, aes(x = X1, y = X2, color = cluster)) +
  geom_point(size = 3) +
  labs(title = "UMAP Visualization of Clusters") +
  theme_minimal()
##########

# Count the number of counties in each cluster
number_in_1 <- nrow(census_numeric %>% filter(cluster==1))
print(number_in_1)
number_in_2 <- nrow(census_numeric %>% filter(cluster==2))
print(number_in_2)
number_in_3 <- nrow(census_numeric %>% filter(cluster==3))
print(number_in_3)
number_in_4 <- nrow(census_numeric %>% filter(cluster==4))
print(number_in_4)


# add back the names, cases, and death 
census_numeric$county_name <- county_names
census_numeric$confirmed_cases <- county_cases
census_numeric$deaths <- county_deaths

#lets take a look at one of the clusters (which ever is the largest)
census_cluster<- census_numeric %>% filter(cluster == 3)

summary(census_cluster)
boxplot(census_cluster$confirmed_cases, 
        main="Boxplot of Confirmed cases in Cluster",
        ylab = "Values",
        col = "lightblue",
        border = "blue")

#' important variables
#' 1) County Name
#' 2) State
#' 3) confirmed cases
#' 4) deaths
#' 5) total population
#' 
#' economical status
#' 1) poverty | pop_determined_poverty_status
#' 2) income ranges (may need to combine columns)
#' 3) employed/unemployed population
#' 4) gini index (how unequal income or wealth distribution)
#' 
#' Race/Ethnicity/Gender
#' 1) male/female pop
#' 2) white/black/hispanic/asian/amerindian/other/race pop
#' 3) age break down by genders
#' 4) age break down by race
#' 
#' social distancing
#' 1) worked_at_home
#' 2) walked_to_work
#' 3) commute times
#' 4) commute by ____
#' 

#' -----------------------------------------------------------------------
covid_tx <- read_csv("COVID-19_cases_TX.csv")

getCountyDataByName <- function(countyname, og_data)
{
  county_data <- og_data %>% filter(county_name == countyname)
  return(county_data)
}

getCountyDataByFIPS <- function(fips, og_data)
{
  county_data <- og_data %>% filter(county_fips_code == fips)
  return(county_data)
}

# stat breakdown for COVID-19_cases_TX
num_unique_counties <- length(unique(covid_tx$county_name)) # 255 unique counties including "Statewide Unallocated"
num_unique_dates <- length(unique(covid_tx$date)) # 370 unique dates

williamson <- getCountyDataByName("Williamson County",covid_tx)

# assumes the rows in sorted in ascending time order
# create a side by side bar graph of confirmed cases and death for a particular county data
create_barChart_cases_death <- function(county_data) 
{
  county_name  <- county_data$county_name[1]
  num_dates <- length(unique(county_data$date))
  df <- data.frame(
    category = rep(c(county_data$date),each = 1),
    value = c(county_data$confirmed_cases,county_data$deaths),
    graph = rep(c("Confirmed Case","Death"),each=num_dates)
  )
  
  return(ggplot(df, aes(x=category, y=value, fill=category)) +
          geom_bar(stat="identity") +
          facet_wrap(~graph,ncol=2) +
          labs(title = paste("Side-by-Side Bar graph of Confirmed Cases and Death",county_name), x="Days", y="Counts")+
          theme_minimal())
  
}

create_barChart_cases_death(williamson)

# What county did the best and why low pop density, better mobility stats?
# 