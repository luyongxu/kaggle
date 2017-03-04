source("./Two Sigma Connect/1.001 Load Data.R")
source("./Two Sigma Connect/1.001 Engineer Features.R")

# 1. Examine data. 
dim(train)
dim(test)

# 2. Interest level. 
ggplot(train, aes(x = interest_level, fill = interest_level)) + 
  geom_bar()

# 3. Bathrooms. 
ggplot(train, aes(x = bathrooms)) + 
  geom_bar() + 
  scale_x_continuous(breaks = seq(0, 10, 0.5))
ggplot(train, aes(x = bathrooms, fill = interest_level)) + 
  geom_bar(position = "dodge", width = 0.2) + 
  scale_x_continuous(breaks = seq(0, 10, 0.5))
ggplot(train, aes(x = interest_level, y = bathrooms, fill = interest_level)) + 
  geom_violin() + 
  scale_y_continuous(breaks = seq(0, 10, 0.5))

# 4. Bedrooms. 
ggplot(train, aes(x = bedrooms)) + 
  geom_bar()
ggplot(train, aes(x = bedrooms, fill = interest_level)) + 
  geom_bar(position = "dodge") + 
  scale_x_continuous(breaks = seq(0, 8, 1))
ggplot(train, aes(x = interest_level, y = bedrooms, fill = interest_level)) + 
  geom_violin()

# 5. Price. 
ggplot(train, aes(x = price)) + 
  geom_histogram()
ggplot(train %>% filter(percent_rank(price) <= 0.99), aes(x = price)) + 
  geom_histogram(binwidth = 100, fill = "blue")

# 6. Latitude.
ggplot(train, aes(x = latitude)) + 
  geom_histogram()
ggplot(train %>% filter(percent_rank(latitude) >= 0.001, percent_rank(latitude) <= 0.999), aes(x = latitude)) + 
  geom_histogram(binwidth = 0.001, fill = "blue")

# 7. Longitude.
ggplot(train, aes(x = longitude)) + 
  geom_histogram()
ggplot(train %>% filter(percent_rank(longitude) >= 0.001, percent_rank(longitude) <= 0.999), aes(x = longitude)) + 
  geom_histogram(binwidth = 0.001, fill = "blue")

# 8. Map. 
ggplot(train, aes(x = longitude, y = latitude)) + 
  geom_point(alpha = 0.1) + 
  coord_cartesian(xlim = c(-74.05, -73.75), ylim = c(40.6, 40.9))
nyc <- get_map(location = c(-73.95, 40.75), source = "google", zoom = 12)
ggmap(nyc) + 
  geom_point(data = train, aes(x = longitude, y = latitude), alpha = 0.1, size = 0.5, colour = "red")
ggmap(nyc) + 
  geom_point(data = train, aes(x = longitude, y = latitude, colour = interest_level), alpha = 0.5)
ggmap(nyc) + 
  geom_point(data = train, aes(x = longitude, y = latitude, colour = interest_level), alpha = 0.5) + 
  facet_wrap(~ interest_level)

# 9. Created. 
ggplot(train, aes(x = created_day)) + 
  geom_bar()
ggplot(train, aes(x = created_hour)) + 
  geom_bar()

# 10. Display address.
train %>% 
  group_by(display_address) %>% 
  summarise(display_address_count = n()) %>% 
  ggplot(aes(x = display_address_count)) + 
  geom_histogram(binwidth = 1, fill = "blue") + 
  scale_y_log10()

# 11. Photos.
ggplot(train, aes(x = photos_count)) + 
  geom_histogram(binwidth = 1, fill = "blue")
ggplot(train, aes(x = interest_level, y = photos_count, fill = interest_level)) + 
  geom_violin() + 
  coord_cartesian(ylim = c(0, 20))

# 12. Features.
ggplot(train, aes(x = features_count)) + 
  geom_histogram(binwidth = 1, fill = "blue")
ggplot(train, aes(x = interest_level, y = features_count, fill = interest_level)) + 
  geom_violin() + 
  coord_cartesian(ylim = c(0, 20)) 
ggplot(train, aes(x = interest_level, y = features_count, fill = interest_level)) + 
  geom_boxplot() + 
  coord_cartesian(ylim = c(0, 20)) 

