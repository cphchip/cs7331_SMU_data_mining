library("tidyverse")

library("ggplot2")
library("ggrepel")
library("ggcorrplot")
library("DT")

cases <- read_csv("COVID-19_cases_plus_census.csv")

cases
str(cases)
cases <- cases %>% mutate_if(is.character, factor)
dim(cases)
str(cases)
cases_TX <- cases %>% filter(state == "TX")
dim(cases_TX)
summary(cases_TX[,1:10])

ggplot(cases_TX, mapping = aes(confirmed_cases)) + 
  geom_histogram(bins = 20) + 
  labs(x = "confirmed cases")

ggplot(cases_TX, mapping = aes(x = confirmed_cases, y = deaths, size = total_pop)) + 
  geom_point() +
  labs(x = "confirmed cases", size = "total population")

ggplot(cases_TX, mapping = aes(x = confirmed_cases, y = deaths, label = county_name)) + 
  geom_smooth(method = lm) +
  geom_point(mapping = aes(size = total_pop), color = "grey") + 
  geom_text_repel(data = subset(cases_TX, deaths >= 1000)) +
  labs(x = "confirmed cases", size = "total population")

cases_TX_select <- cases_TX %>% filter(confirmed_cases > 100) %>% 
  arrange(desc(confirmed_cases)) %>%    
  select(county_name, confirmed_cases, deaths, total_pop, median_income)
cases_TX_select <- cases_TX_select %>% mutate(
  cases_per_1000 = confirmed_cases/total_pop*1000, 
  deaths_per_1000 = deaths/total_pop*1000, 
  death_per_case = deaths/confirmed_cases)

head(cases_TX_select)

datatable(cases_TX_select) %>% formatRound(6:7, 4) %>% formatPercentage(8, 2)

ggplot(cases_TX_select, mapping = aes(x = cases_per_1000, y = deaths_per_1000, label = county_name)) + 
  geom_smooth(method = lm) +
  geom_point(mapping = aes(size = total_pop), color = "grey") + 
  geom_text_repel(data = subset(cases_TX_select, deaths_per_1000 > quantile(deaths_per_1000, .95))) +
  labs(x = "confirmed cases", y = "deaths per 1000", size = "total population")