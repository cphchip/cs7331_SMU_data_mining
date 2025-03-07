library("tidyverse")
library("ggplot2")
library("ggrepel")
library("ggcorrplot")
library("DT")
library(dplyr)
library(conflicted)
conflicts_prefer(dplyr::filter)

# Census Data
census_data <- read_csv("COVID-19_cases_plus_census.csv")
census_tarrant_bexar <- census_data %>% 
                  filter(county_name %in% c("Tarrant County", "Bexar County")) %>%
                  select(where(~ !is.logical(.))) %>%
                  na.omit(census_tx)

# add population density - size_km derived from wikipedia
census_tarrant_bexar$size_km <- c(2340,3250)
census_tarrant_bexar$pop_den <- census_tarrant_bexar$total_pop / census_tarrant_bexar$size_km

barchart_this <- function(property)
{
  df <- data.frame(Category = c("Tarrant County", "Bexar County"),
                      Value = census_tarrant_bexar %>% pull(property))
  
  ggplot(df, aes(x=Category, y=Value, fill=Category))+
    geom_bar(stat="identity") +
    geom_text(aes(label = Value), vjust=-0.5, size=5) +
    labs(title=paste("Bar Chart:",property), x="County",y="Value")+
    theme_minimal()
}
barchart_this("total_pop")

