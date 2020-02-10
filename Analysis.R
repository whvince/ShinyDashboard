# Number of breweries in each category

category_concentration <- beer_list %>% 
  inner_join(select(breweries, -state_abbr, -brewery_types), by = 'brewery_id') %>% 
  select(style_category, brewery_id) %>% 
  unique() %>% 
  group_by(style_category) %>% 
  summarise(num_breweries = n()) %>% 
  arrange(desc(num_breweries))

style_concentration <- beer_list %>% 
  inner_join(select(breweries, -state_abbr, -brewery_types), by = 'brewery_id') %>% 
  select(style, brewery_id) %>% 
  unique() %>% 
  group_by(style) %>% 
  summarise(num_breweries = n()) %>% 
  arrange(desc(num_breweries))

category_rating <- beer_list %>% 
  inner_join(reviews_rating, by = "beer_id") %>% 
  group_by(style_category) %>% 
  summarise(score_avg = mean(score)) %>% 
  arrange(desc(score_avg))

style_rating <- beer_list %>% 
  inner_join(reviews_rating, by = "beer_id") %>% 
  group_by(style_category, style) %>% 
  summarise(score_avg = mean(score)) %>% 
  arrange(desc(score_avg))


# Number of Breweries vs category rating
inner_join(category_concentration, category_rating, by = "style_category") %>% 
  ggplot(aes(x = num_breweries, y = score_avg), log = 'x') + 
  geom_point() + 
  geom_text(aes(label=style_category), hjust=1,vjust=1) +
  labs(title='Beer Category Concentration Analysis',
       x='Number of Breweries',
       y='Average Score') 


# Number of Breweries vs style rating
inner_join(style_concentration, style_rating, by = "style") %>% 
  ggplot(aes(x = num_breweries, y = score_avg)) + 
  geom_point(aes(color = style_category)) + 
  labs(title='Beer Style Concentration Analysis',
       x='Number of Breweries',
       y='Average Score') 
  
# abv vs. rating

complete_beer_info %>% 
  filter(style_category ==  "Pale Lager and Pilsner") %>% 
  ggplot(aes(x = abv, y = score)) +
  geom_point() + 
  geom_smooth()

# Rating distribution
complete_beer_info %>% 
  filter(style ==  "American Adjunct Lager") %>% 
  ggplot(aes(x = score))+
  geom_histogram()


# Top Beer ratings
reviews_rating_ts %>% 
  inner_join(top_beers, by = 'beer_id') %>% 
  group_by(year, brewery_name) %>% 
  summarise(score_avg_company = mean(score_avg)) %>% 
  ggplot(aes(x = year, y = score_avg_company)) + geom_point(aes(color = brewery_name))


beer_list %>% filter(beer_id %in% top_beers$beer_id)

reviews_rating_ts %>% 
  inner_join(top_beers, by = 'beer_id') %>% 
  filter(ranking_within <= 3) %>% 
  group_by(year, beer_name) %>% 
  summarise(score_avg_beer = mean(score_avg)) %>%
  left_join(select(top_beers, beer_name, brewery_name)) %>% 
  ggplot(aes(x = year, y = score_avg_beer)) + 
  geom_line(aes(color = beer_name)) 
  

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


# Brewery beers - not meaningful

brand_count <- beer_list %>% 
  left_join(select(breweries, -state_abbr, -brewery_types), by = 'brewery_id') %>% 
  group_by(brewery_id) %>% 
  summarise(brand_count = n()) %>% 
  arrange(desc(brand_count)) %>% 
  inner_join(rating_brewery, by = 'brewery_id') %>% 
  select(brewery_id, brewery_name, state, everything())

brand_count %>% filter(state == 'Wisconsin') %>% View()

brand_count %>% ggplot(aes(x = brand_count, y = brewery_rating), log = "y") + geom_point()
