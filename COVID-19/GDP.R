library("tidyverse")
library("ggplot2")
library("ggrepel")
library("ggcorrplot")
library("DT")
library(dplyr)
library(conflicted)
library(patchwork)
conflicts_prefer(dplyr::filter)

#' Import quarterly GDP Percent Change
#' We will combine the the identifiers as well as a quarters for Q4 2019 to Q1 2021
full_tx_quarterly_gdp <- read_csv("State_quarterly_GDP/SQGDP11_TX_2005_2024.csv")
df_1 <- full_tx_quarterly_gdp %>% select(1:8)
df_2 <- full_tx_quarterly_gdp %>% select(67:73) %>%
  mutate(across(where(is.character), as.numeric))
tx_quarterly_gdp <- bind_cols(df_1,df_2)

# Dropping empty rows and unnecessary identifiers
tx_quarterly_gdp <- tx_quarterly_gdp %>%
  select(7:ncol(tx_quarterly_gdp)) %>%
  select(-Unit) %>%
  na.omit(tx_quarterly_gdp)

# Checkout first 10 rows and get some statistics on reach column
head(tx_quarterly_gdp, 10)
summary(tx_quarterly_gdp)

# function to plot quarterly change for a given industry
# name must match one of the Description values exactly
plot_quarterly_gdp_industry <- function(name)
{
  selectedDF <- tx_quarterly_gdp %>% 
    filter(Description == name) %>%
    select(2:ncol(tx_quarterly_gdp))
  
  row_long <- tibble(Variable = names(selectedDF), Value = as.numeric(selectedDF))
  
  return(ggplot(row_long, aes(x=Variable, y = Value, group=1)) +
    geom_line(color="blue") + 
    geom_point(size=3, color="red") +
    geom_text(aes(label = Value), vjust = -1) +
    labs(title = paste("Percent Change of",name,"Industry from 2019Q4 to 2021Q2 "), x="Quaters", y="Percent") +
    theme_minimal())
}

# Ploting the change across all industries
plot_quarterly_gdp_industry("All industry total (percent change)")

# Grabbing a subset of industries that were greatly affected and least affected
# by Covid-19
# Source: https://www.spglobal.com/market-intelligence/en/news-insights/research/industries-most-and-least-impacted-by-covid-19-from-a-probability-of-default-perspective-january-2022-update
interested <- c("Manufacturing",
                "Retail trade",
                "Finance and insurance",
                "Real estate and rental and leasing",
                "Arts, entertainment, and recreation",
                "Accommodation and food services",
                "Educational services",
                "Health care and social assistance")

# Plotting each of the interested industries
industry_plots <- lapply(1:length(interested), function(i) {
  industry <- interested[i]
  plot_quarterly_gdp_industry(industry)
})

wrap_plots(industry_plots, ncol = 2)

## County Yearly GDP 
full_tx_counties_gdp <- read_csv("GDP_current_dollar_county/CAGDP2_TX_2001_2023.csv")

# Grab identifiers and GDP levels for 2019-2022
subset_tx_counties_gdp_pt1 <- full_tx_counties_gdp %>%
  select(1:7)
subset_tx_counties_gdp_pt2 <- full_tx_counties_gdp %>%
  select(27:30) %>%
  mutate(across(where(is.character), as.numeric))
tx_counties_gdp <- bind_cols(subset_tx_counties_gdp_pt1, subset_tx_counties_gdp_pt2)
head(tx_counties_gdp)

# Dropping empty rows and unnecessary identifiers
tx_counties_gdp <- tx_counties_gdp %>%
  na.omit(tx_counties_gdp) %>%
  select(-GeoFIPS, -Region, -TableName, -LineCode, -IndustryClassification)

# Checkout first 10 rows and get some statistics on reach column
head(tx_counties_gdp)
summary(tx_counties_gdp)

# function that plots industry GDP levels for specific county
# Note: countyname must match entry in GeoName and industryname must match something in Description
plot_yearly_GPD_industry <- function(countyname, industryname)
{
  selectedDF <- tx_counties_gdp %>% 
    filter(GeoName == countyname) %>%
    filter(Description == industryname) %>%
    select(3:ncol(tx_counties_gdp))
  
  row_long <- tibble(Variable = names(selectedDF), Value = as.numeric(selectedDF))
  
  ggplot(row_long, aes(x=Variable, y = Value, group=1)) +
    geom_line(color="blue") + 
    geom_point(size=3, color="red") +
    labs(title = paste("GDP for",industryname,"in",countyname), x="Year", y="Thousands of Dollars") +
    theme_minimal()
}
plot_yearly_GPD_industry("Texas","All industry total")

# Plotting GDP for All industry total and for the interested industries
selectedDF <- tx_counties_gdp %>% 
  filter(GeoName == "Texas") %>%
  filter(Description == "All industry total") %>%
  select(3:ncol(tx_counties_gdp))
s_row <- data.frame(Variable = names(selectedDF), Value=as.numeric(selectedDF))
s_row$Group <- "All industry total"

combined <- s_row

for (industry in interested)
{
  selectedDF <- tx_counties_gdp %>% 
    filter(GeoName == "Texas") %>%
    filter(Description == industry) %>%
    select(3:ncol(tx_counties_gdp))
  s_row <- data.frame(Variable = names(selectedDF), Value=as.numeric(selectedDF))
  s_row$Group <- industry
  combined <- bind_rows(combined, s_row)
}
# If we want to remove "All industry total", execute the line below 
combined <- combined[-(1:4),]

ggplot(combined, aes(x=Variable, y = Value, color=Group, group=Group)) +
  geom_line(size=1) + 
  geom_point(size=3) +
  labs(title = paste("GDP for Selected Industries"), x="Year", y="Thousands of Dollars") +
  theme(legend.position = "right")

# Function for comparing two counties yearly GDP for specific industry
compare_yearly_GPD_industry <- function(county1, county2, industryname)
{
  df1 <- tx_counties_gdp %>% 
    filter(GeoName == county1) %>%
    filter(Description == industryname) %>%
    select(3:ncol(tx_counties_gdp))
  
  row1 <- data.frame(Variable = names(df1), Value = as.numeric(df1))
  row1$Group <- county1
  
  df2 <- tx_counties_gdp %>% 
    filter(GeoName == county2) %>%
    filter(Description == industryname) %>%
    select(3:ncol(tx_counties_gdp))
  
  row2 <- data.frame(Variable = names(df2), Value = as.numeric(df2))
  row2$Group <-county2
  
  df_combined <- bind_rows(row1,row2)
  
  return(ggplot(df_combined, aes(x = Variable, y = Value, color = Group, group = Group)) +
    geom_line(size = 1) + 
    geom_point(size = 2) +
    labs(title = paste(county1,"vs",county2,"-",industryname), x = "Year", y = "Thousands of Dollars") +
    theme_minimal())
  
}
# Compare Bexar and Tarrant County GDP for all industry
compare_yearly_GPD_industry("Tarrant, TX", "Bexar, TX", "All industry total")

# Compare Bexar and Tarrant County for interested industries
industry_plots <- lapply(1:length(interested), function(i) {
    industry <- interested[i]
    compare_yearly_GPD_industry("Tarrant, TX", "Bexar, TX", industry)
  })

wrap_plots(industry_plots, ncol = 2)

########

