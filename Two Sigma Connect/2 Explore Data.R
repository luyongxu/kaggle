source("./Two Sigma Connect/1 Load Data.R")

# 1. Examine data. 
dim(train)
dim(test)

# 2. Encode interest level. 
train <- train %>%
  mutate(interest_level = factor(interest_level, levels = c("low", "medium", "high")))

# 3. Interest level. Class is imbalanced with mostly low interest level. 
ggplot(train, aes(x = interest_level, fill = interest_level)) + 
  geom_bar()

# 4. Bathrooms. Mostly has 1 or 2 bathrooms. 
# Higher interest level in 1 and 2 bathrooms.  
# There are no x.5 rentals with high interest level. 
# Consider converting bathrooms to factor. 
ggplot(train, aes(x = bathrooms)) + 
  geom_bar(fill = "blue") + 
  scale_x_continuous(breaks = seq(0, 10, 0.5))
ggplot(train, aes(x = bathrooms, fill = interest_level)) + 
  geom_bar(position = "dodge", width = 0.2) + 
  scale_x_continuous(breaks = seq(0, 10, 0.5))
ggplot(train, aes(x = bathrooms, fill = interest_level)) + 
  geom_bar(position = "fill", width = 0.2) + 
  scale_x_continuous(breaks = seq(0, 10, 0.5))
table(train$bathrooms, train$interest_level)
round(prop.table(table(train$bathrooms, train$interest_level), 1), 2)

# 5. Bedrooms. Mostly 0, 1, 2, or 3 bedrooms. 
# Higher interest level in 0, 2, 3, or 4 bedrooms. Lower interest level in 1 bedrooms. 
# Consider converting bedrooms to factor. 
ggplot(train, aes(x = bedrooms)) + 
  geom_bar(fill = "blue")
ggplot(train, aes(x = bedrooms, fill = interest_level)) + 
  geom_bar(position = "dodge") + 
  scale_x_continuous(breaks = seq(0, 8, 1))
ggplot(train, aes(x = bedrooms, fill = interest_level)) + 
  geom_bar(position = "fill") + 
  scale_x_continuous(breaks = seq(0, 8, 1))
table(train$bedrooms, train$interest_level)
round(prop.table(table(train$bedrooms, train$interest_level), 1), 2)
table(str_c(train$bedrooms, train$bathrooms), train$interest_level)

# 6. Price. Higher interest level with lower price. 
ggplot(train %>% filter(percent_rank(price) <= 0.99), aes(x = price)) + 
  geom_histogram(binwidth = 100, fill = "blue")
ggplot(train %>% filter(percent_rank(price) <= 0.99), aes(x = price)) + 
  geom_freqpoly(binwidth = 100, aes(colour = interest_level))
ggplot(train %>% filter(percent_rank(price) <= 0.99), aes(x = price)) + 
  geom_density(aes(colour = interest_level))
train %>% 
  group_by(interest_level) %>% 
  summarise(mean_price = mean(price), 
            median_price = median(price))

# 7. Latitude and longitude. Removed some outliers. Hard to see pattern.
ggplot(train %>% filter(percent_rank(latitude) >= 0.001, percent_rank(latitude) <= 0.999), aes(x = latitude)) + 
  geom_histogram(binwidth = 0.001, fill = "blue")
ggplot(train %>% filter(percent_rank(longitude) >= 0.001, percent_rank(longitude) <= 0.999), aes(x = longitude)) + 
  geom_histogram(binwidth = 0.001, fill = "blue")
ggplot(train, aes(x = longitude, y = latitude)) + 
  geom_point(alpha = 0.1) + 
  coord_cartesian(xlim = c(-74.05, -73.75), ylim = c(40.6, 40.9))
train %>% 
  group_by(interest_level) %>% 
  summarise(mean_lat = mean(latitude), 
            median_lat = median(latitude), 
            mean_lon = mean(longitude), 
            median_lon = median(longitude))
nyc <- ggmap::get_map(location = c(-73.95, 40.75), source = "google", zoom = 12)
ggmap(nyc) + 
  geom_point(data = train, aes(x = longitude, y = latitude, colour = interest_level), alpha = 0.5)
ggmap(nyc) + 
  geom_point(data = train, aes(x = longitude, y = latitude, colour = interest_level), alpha = 0.5) + 
  facet_wrap(~ interest_level)

# 8. Created. 
# Seems like at month end and beginning there is higher interest level, especially the 31st and 1st. 
# Seems like higher interest level on Saturdays. 
# Seems like higher interest level for posts at midnight, lowest interest level at 1am. 
# Or just indicators of month end or month start or Friday. 
ggplot(train, aes(x = date(created), fill = interest_level)) + 
  geom_bar()
ggplot(train, aes(x = date(created), fill = interest_level)) + 
  geom_bar(position = "fill")
ggplot(train, aes(x = month(created), fill = interest_level)) + 
  geom_bar(position = "fill")
ggplot(train, aes(x = yday(created), fill = interest_level)) + 
  geom_bar(position = "fill")
ggplot(train, aes(x = day(created), fill = interest_level)) + 
  geom_bar(position = "fill")
ggplot(train, aes(x = wday(created), fill = interest_level)) + 
  geom_bar(position = "fill")
ggplot(train, aes(x = hour(created), fill = interest_level)) + 
  geom_bar(position = "fill")
ggplot(train, aes(x = minute(created), fill = interest_level)) + 
  geom_bar(position = "fill")
ggplot(train, aes(x = second(created), fill = interest_level)) + 
  geom_bar(position = "fill")


# 9. Display address count. 
# Hard to see pattern. 
display_address_count <- train %>% 
  group_by(display_address) %>% 
  summarise(display_address_count = n())
train <- train %>% 
  left_join(display_address_count)
ggplot(display_address_count, aes(x = display_address_count)) + 
  geom_histogram(binwidth = 1, fill = "blue") + 
  scale_y_log10()
ggplot(train, aes(x = display_address_count, fill = interest_level)) + 
  geom_bar()
ggplot(train, aes(x = display_address_count, fill = interest_level)) + 
  geom_bar(position = "fill")

# 10. Photos count. 
# Zero photos associated with low interest. 
# Having between 1 and 15 photos is associated with higher interest. 
# Beyond 15 photos, there is low interest. 
train <- train %>% 
  mutate(photos_count = map_int(photos, length))
ggplot(train, aes(x = photos_count)) + 
  geom_histogram(binwidth = 1, fill = "blue")
ggplot(train, aes(x = photos_count, fill = interest_level)) + 
  geom_bar()
ggplot(train, aes(x = photos_count, fill = interest_level)) + 
  geom_bar(position = "fill")

# 11. Features count. 
# Hard to see a pattern. 
train <- train %>% 
  mutate(features_count = map_int(features, length))
ggplot(train, aes(x = features_count)) + 
  geom_histogram(binwidth = 1, fill = "blue")
ggplot(train, aes(x = features_count, fill = interest_level)) + 
  geom_bar()
ggplot(train, aes(x = features_count, fill = interest_level)) + 
  geom_bar(position = "fill")

# 12. Building id. 
# Lower building id count is associated with higher interest level. 
# Around 8,000 records have a building id associated with 0, around 90% of those have low interest level. 
building_id_count <- train %>% 
  group_by(building_id) %>% 
  summarise(building_id_count = n())
train <- train %>% 
  left_join(building_id_count)
ggplot(train %>% filter(building_id_count <= 200),
       aes(x = building_id_count)) + 
  geom_histogram(binwidth = 1, fill = "blue")
ggplot(train %>% filter(building_id_count <= 200), 
       aes(x = building_id_count, fill = interest_level)) + 
  geom_bar()
ggplot(train %>% filter(building_id_count >= 8000), 
       aes(x = building_id_count, fill = interest_level)) + 
  geom_bar()
ggplot(train %>% filter(building_id_count <= 200), 
       aes(x = building_id_count, fill = interest_level)) + 
  geom_bar(position = "fill")
ggplot(train %>% filter(building_id_count >= 8000), 
       aes(x = building_id_count, fill = interest_level)) + 
  geom_bar(position = "fill")



