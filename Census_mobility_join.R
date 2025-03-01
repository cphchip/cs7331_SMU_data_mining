
# Import the initial mobility dataset
census_cases_df <- read_csv("COVID-19_cases_plus_census.csv")
str(census_cases_df)

census_subset <- census_cases_df %>%
  select(
    state,
    confirmed_cases,
    deaths,
    total_pop,
    male_pop,
    female_pop,
    black_pop,
    white_pop,
    hispanic_pop,
    commuters_by_public_transportation,
    worked_at_home,
    income_per_capita
  )

# head(census_subset)

aggregate_census <- function(df) { # code help from ChaptGPT
  new_df <- df %>%
    group_by(state) %>%  # Group by the 'date' column
    summarize(
      total_confirmed_cases = sum(confirmed_cases, na.rm = TRUE),
      total_deaths = sum(deaths, na.rm = TRUE),
      total_pop = sum(total_pop, na.rm = TRUE),
      total_male_pop = sum(male_pop, na.rm = TRUE),
      total_female_pop = sum(female_pop, na.rm = TRUE),
      total_black_pop = sum(black_pop, na.rm = TRUE),
      total_white_pop = sum(white_pop, na.rm = TRUE),
      total_hispanic_pop = sum(hispanic_pop, na.rm = TRUE),
      total_commuters_by_public_transportation = sum(commuters_by_public_transportation, na.rm =TRUE),
      total_worked_at_home = sum(worked_at_home, na.rm = TRUE),
      avg_income_per_capita = mean(income_per_capita, na.rm = TRUE),
    )
  return(new_df)
}

census_summary_df = aggregate_census(census_subset)
head(census_summary_df)

mobility2 <- read_csv("Global_Mobility_Report.csv")

# Function to filter down to data of interest and drop unnecessary features
mobility_filter <- function(data, interest, sub_interest) {
  data_of_interest <- data %>%
    select(
      country_region, 
      sub_region_1,
      # sub_region_2,
      date, 
      retail_and_recreation_percent_change_from_baseline,
      parks_percent_change_from_baseline,
      transit_stations_percent_change_from_baseline,
      workplaces_percent_change_from_baseline,
      residential_percent_change_from_baseline
    ) %>% 
    filter(country_region == interest) %>%
    drop_na(sub_region_1)
    
  return(data_of_interest)
}


mobility2 <- mobility_filter(mobility2, 'United States')
  
head(mobility2)

# https://www.scouting.org/resources/los/states/
state_to_postal <- read_csv("state_to_postal_code(Sheet1).csv")
head(state_to_postal)

# Join the population dataset to my mobility dataset using the country name
mobility2 <- left_join(
  mobility2,
  state_to_postal, 
  by = c('sub_region_1' = 'State')
)
head(mobility2)
