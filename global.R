
library(shinydashboard)
library(DT)
library(googleVis)
library(maps)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(plotly)
library(rlang)
library(timeDate)
library(zoo)
library(lubridate)


# Read and clean raw data -------------------------------------------------

# Raw Data from Kaggle https://www.kaggle.com/ehallmar/beers-breweries-and-beer-reviews#breweries.csv
beers <- read_csv("Assets/beers.csv") %>% tbl_df()
beer_list <- read_csv("Assets/beer_list.csv") %>% tbl_df()
breweries <- read_csv("Assets/breweries.csv") %>% tbl_df() %>% select(-notes)

# derived from reviews raw data. See Data Cleaning for more information
reviews_rating_ts <- read_csv("Assets/reviews_rating_ts.csv") %>% tbl_df() 
reviews_rating <- read_csv("Assets/reviews_rating.csv") %>% tbl_df() 

# Source: mannual mapped using https://www.webstaurantstore.com/article/27/different-types-of-beers.html
beer_category <- read_csv("Assets/beer_style_category.csv") %>% tbl_df()

# Source
city_coords <- read_csv('Assets/city_coords.csv') %>% tbl_df()

# Source: https://www.usatoday.com/story/money/food/2018/10/17/beer-consumption-these-americas-26-top-selling-beers/38104743/
top_beers <- read_csv('Assets/Top25_Beer_Brands_USA.csv') %>% tbl_df()

US_pop <- read.csv("Assets/US_Population.csv") %>% tbl_df()

# Breweries in North America Only
breweries <- breweries[breweries$country != 'US'| (breweries$country == 'US' & !is.na(breweries$state)), ] %>% 
  filter(!is.na(id) & !is.na(city)) %>% 
  filter(country %in% c('US', 'CA', 'MX')) %>% 
  left_join(city_coords, by =c("city" = "City", "state" = "Abbr")) %>% 
  rename(brewery_id = id, brewery_name = name, brewery_types = types, state_abbr = state, state = State) %>% 
  unique()

# Analysis ----------------------------------------------------------------

# Rankings of top beers year over year
ranking <- reviews_rating_ts %>% 
  inner_join(top_beers, by = 'beer_id') %>% 
  filter(ranking_within <= 3 & year >=2001) %>% 
  group_by(year, beer_name, brewery_name) %>% 
  summarise(score_avg_beer = mean(score_avg)) %>%
  arrange(desc(score_avg_beer)) %>% 
  group_by(year) %>% 
  mutate(rank = row_number())

# Ranking - heavy beer is ranked higher than light
ranking_lightHeavy <- reviews_rating_ts %>% 
  inner_join(top_beers, by = 'beer_id') %>% 
  filter(heavy_light_flag == 1 & year >=2001) %>% 
  arrange(desc(score_avg)) %>% 
  group_by(year) %>% 
  mutate(rank = row_number())

ranking_lightHeavy$beer_name %>% unique()


# Rating - heavy beer is rated higher than light
rating_lightHeavy <- reviews_rating_ts %>% 
  inner_join(top_beers, by = 'beer_id') %>% 
  filter(heavy_light_flag == 1 & year >=2001) %>% 
  mutate(HeavyLight= ifelse(grepl("Light",beer_name)|grepl("Lite",beer_name), "Light", "Heavy")) %>% 
  group_by(HeavyLight,year) %>% 
  summarise(average_rating = mean(score_avg))
  


# Rating - Pale Lager and Pilsner have the lowest rating
category_rating <- beer_list %>% 
  inner_join(reviews_rating, by = "beer_id") %>% 
  group_by(style_category) %>% 
  summarise(score_avg = mean(score)) %>% 
  arrange(desc(score_avg))

# Concentration of beer categories
category_concentration <- beer_list %>% 
  inner_join(select(breweries, -state_abbr, -brewery_types), by = 'brewery_id') %>% 
  select(style_category, brewery_id) %>% 
  unique() %>% 
  group_by(style_category) %>% 
  summarise(num_breweries = n()) %>% 
  arrange(desc(num_breweries))

concentration <- inner_join(category_concentration, category_rating, by = "style_category") 

style_concentration <- beer_list %>% 
  inner_join(select(breweries, -state_abbr, -brewery_types), by = 'brewery_id') %>% 
  select(style, brewery_id) %>% 
  unique() %>% 
  group_by(style) %>% 
  summarise(num_breweries = n()) %>% 
  arrange(desc(num_breweries))

style_rating <- beer_list %>% 
  inner_join(reviews_rating, by = "beer_id") %>% 
  group_by(style_category, style) %>% 
  summarise(score_avg = mean(score)) %>% 
  arrange(desc(score_avg))

# League Table
complete_beer_info <- beer_list %>% 
  inner_join(select(breweries, brewery_id, brewery_name, state_abbr, state, country), by = 'brewery_id') %>% 
  inner_join(reviews_rating, by = "beer_id") %>% 
  mutate(state_country_abbr = ifelse(is.na(state_abbr), country, state_abbr),
         abv = round(abv,1),
         look = round(look, 1),
         smell = round(smell, 1),
         taste = round(taste, 1),
         feel = round(overall, 1),
         overall = round(overall,1),
         score = round(score, 1)) %>% 
  select(beer_name, style, style_category, brewery_name, brewery_id, state_country_abbr, state, abv, look, smell, taste, feel, balance = overall, score, num_reviews)


# breweries and rating by state
rating_brewery<- complete_beer_info %>% 
  group_by(brewery_name, brewery_id, state) %>% 
  summarise(brewery_rating = mean(score))

rating_brewery_state <- rating_brewery %>% 
  group_by(state) %>% 
  summarise(state_rating = mean(brewery_rating), brewery_count = n()) %>% 
  left_join(US_pop, by = c('state'='State')) %>% 
  mutate(breweries_per_100K =brewery_count/(Value/100000)) %>% 
  select(-Value, -brewery_count)

  