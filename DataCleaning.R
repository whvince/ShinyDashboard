
# Set working directory and load packages -----------------------------------------------------------

setwd("/Users/invincible/Desktop/Bootcamp/projects/shiny/beers-breweries-and-beer-reviews")

library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(rlang)
library(timeDate)
library(zoo)
library(lubridate)

# Read and clean raw data -------------------------------------------------

beers <- read_csv("beers.csv") %>% tbl_df()
beer_list <- read_csv("beer_list.csv") %>% tbl_df()
breweries <- read_csv("breweries.csv") %>% tbl_df() %>% select(-notes)
reviews_rating_ts <- read_csv("reviews_rating_ts.csv") %>% tbl_df() 
reviews_rating <- read_csv("reviews_rating.csv") %>% tbl_df() 
beer_category <- read_csv("beer_style_category.csv") %>% tbl_df()
city_coords <- read_csv('city_coords.csv') %>% tbl_df()
top_beers <- read_csv('Top25_Beer_Brands_USA.csv') %>% tbl_df()
US_pop <- read.csv("US_Population.csv") %>% tbl_df()

# Large file (>2GB) - don't reload everytime
# File can be found at https://www.kaggle.com/ehallmar/beers-breweries-and-beer-reviews#breweries.csv
# reviews <- read_csv("reviews.csv") %>% tbl_df()


# Breweries

breweries <- breweries[breweries$country != 'US'| (breweries$country == 'US' & !is.na(breweries$state)), ] %>% 
  filter(!is.na(id) & !is.na(city)) %>% 
  filter(country %in% c('US', 'CA', 'MX')) %>% 
  left_join(city_coords, by =c("city" = "City", "state" = "Abbr")) %>% 
  rename(brewery_id = id, brewery_name = name, brewery_types = types, state_abbr = state, state = State) %>% 
  unique()

# BEERS - ONLY RUN THIS ONCE WHEN CLEANNING THE DATA

beer_list <- beers %>% 
  filter(retired == FALSE, brewery_id %in% unique(breweries$brewery_id)) %>% 
  select(-retired, -notes, - state, -country) %>% 
  rename(beer_id = id, beer_name = name) %>% 
  unique() %>% 
  left_join(beer_category, by = "style")

write_csv(beer_list, path = paste0(getwd(), '/beer_list.csv'))

# REVIEWS - ONLY RUN THIS ONCE WHEN CLEANNING THE DATA
  
  # Only include reviewers who had at least 10 reviews
num_reviews_user <- reviews %>% 
  filter(beer_id %in% unique(beer_list$beer_id)) %>% 
  group_by(username) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))
  
  # Aggregate average ratings per beer that has more than 10 reviews. Then filter out any NA
reviews_rating <- reviews %>% 
  filter(beer_id %in% unique(beer_list$beer_id)) %>% 
  group_by(beer_id) %>% 
  summarise_if(is.numeric, function(x) mean(x, na.rm = T)) %>% 
  left_join(tally(group_by(reviews, beer_id)), by = 'beer_id') %>% 
  filter(n >= 10) %>% 
  rename(num_reviews = n)

reviews_rating <- reviews_rating[rowSums(is.na(reviews_rating[,2:7])) == 0, ]

reviews_rating_ts <- reviews %>% 
  select(-text) %>% 
  filter(beer_id %in% unique(reviews_rating$beer_id)) %>% 
  mutate(year = year(date)) %>% 
  group_by(year, beer_id) %>%
  summarise(score_avg = mean(score)) %>% 
  ungroup()

write_csv(reviews_rating_ts, path = paste0(getwd(), '/reviews_rating_ts.csv'))
write_csv(reviews_rating, path = paste0(getwd(), '/reviews_rating.csv'))

