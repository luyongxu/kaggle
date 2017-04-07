source("./Two Sigma Connect/1.001 Load Data.R")
source("./Two Sigma Connect/1.003 Engineer Features.R")

# 1. Bathrooms half, bathrooms zero, and bedrooms one. 
# Good feature. Any listing with x.5 bathrooms has almost all low interest. 
# On RentHop website, unable to filter by x.5 bathrooms. Must be in whole numbers. 
# Good feature. Any listing with zero bathrooms has almost all low interest. 
# On RentHop website, unable to filter by zero bathrooms.
# Good feature. Lower interest level in one bedrooms. 
ggplot(train, aes(x = factor(bathrooms_half), fill = factor(interest_level))) + 
  geom_bar(position = "fill")
table(train$bathrooms, train$interest_level)
table(train$bedrooms, train$interest_level)

# 2. Price per bed. 
# Good feature. Higher interest is associated with lower price per bed. 
ggplot(train %>% filter(percent_rank(price_per_bed) <= 0.99), aes(x = price_per_bed, fill = factor(interest_level))) + 
  geom_histogram(binwidth = 100)
ggplot(train %>% filter(percent_rank(price_per_bed) <= 0.99), aes(x = price_per_bed, colour = factor(interest_level))) + 
  geom_freqpoly(binwidth = 100)
ggplot(train %>% filter(percent_rank(price_per_bed) <= 0.99), aes(x = price_per_bed, colour = factor(interest_level))) + 
  geom_density()

# 3. Price per bath. 
# Good feature. Higher interest is associated with lower price per bath. 
ggplot(train %>% filter(percent_rank(price_per_bath) <= 0.99), aes(x = price_per_bath, fill = factor(interest_level))) + 
  geom_histogram(binwidth = 100)
ggplot(train %>% filter(percent_rank(price_per_bath) <= 0.99), aes(x = price_per_bath, colour = factor(interest_level))) + 
  geom_freqpoly(binwidth = 100)
ggplot(train %>% filter(percent_rank(price_per_bath) <= 0.99), aes(x = price_per_bath, colour = factor(interest_level))) + 
  geom_density()

# 4. Price per room. 
# Good feature. Higher interest is associated with lower price per room. 
ggplot(train %>% filter(percent_rank(price_per_room) <= 0.99), aes(x = price_per_room, fill = factor(interest_level))) + 
  geom_histogram(binwidth = 100)
ggplot(train %>% filter(percent_rank(price_per_room) <= 0.99), aes(x = price_per_room, colour = factor(interest_level))) + 
  geom_freqpoly(binwidth = 100)
ggplot(train %>% filter(percent_rank(price_per_room) <= 0.99), aes(x = price_per_room, colour = factor(interest_level))) + 
  geom_density()

# 5. Bed per bath. 
# Good feature. Higher interest is associated with higher bed per bath. 
ggplot(train, aes(x = bed_per_bath, fill = factor(interest_level))) + 
  geom_histogram(binwidth = 1)
ggplot(train, aes(x = bed_per_bath, colour = factor(interest_level))) + 
  geom_freqpoly(binwidth = 1)
ggplot(train, aes(x = bed_per_bath, colour = factor(interest_level))) + 
  geom_density()

# 6. Bed percentage. 
# Good feature. Higher interest is associated with higher bed percentage. 
ggplot(train, aes(x = bed_perc, fill = factor(interest_level))) + 
  geom_histogram(binwidth = 0.1)
ggplot(train, aes(x = bed_perc, colour = factor(interest_level))) + 
  geom_freqpoly(binwidth = 0.1)
ggplot(train, aes(x = bed_perc, colour = factor(interest_level))) + 
  geom_density()

# 7. Bed bath diff. 
# Good feature. Higher interest is associated with higher bed bath diff. 
ggplot(train, aes(x = bed_bath_diff, fill = factor(interest_level))) + 
  geom_histogram(binwidth = 1)
ggplot(train, aes(x = bed_bath_diff, colour = factor(interest_level))) + 
  geom_freqpoly(binwidth = 1)
ggplot(train, aes(x = bed_bath_diff, colour = factor(interest_level))) + 
  geom_density()

# 8. Bed bath sum. 
# Good feature. Higher interest is associated with bed bath sum of everything except 2. 
ggplot(train, aes(x = bed_bath_sum, fill = factor(interest_level))) + 
  geom_histogram(binwidth = 1)
ggplot(train, aes(x = bed_bath_sum, colour = factor(interest_level))) + 
  geom_freqpoly(binwidth = 1)
ggplot(train, aes(x = bed_bath_sum, colour = factor(interest_level))) + 
  geom_density()

# 9. Type of apartment. 
# Good feature. Some types of apartments have less interest, like one bedroom one bath. 
table(str_c(train$bedrooms, train$bathrooms), train$interest_level)

# 10. Distance center and latitude and longitude.  
# Good feature. Higher interest is associated with locations further away from city center. 
# Good feature. Longitude and latitude show some seperations between interest level. 
ggplot(train %>% filter(distance_center <= 0.25), aes(x = distance_center, colour = factor(interest_level))) + 
  geom_density()
ggplot(train %>% filter(percent_rank(latitude) >= 0.001, percent_rank(latitude) <= 0.999), aes(x = latitude, colour = factor(interest_level))) + 
  geom_density()
ggplot(train %>% filter(percent_rank(longitude) >= 0.001, percent_rank(longitude) <= 0.999), aes(x = longitude, colour = factor(interest_level))) + 
  geom_density()
ggplot(train %>% filter(percent_rank(latitude) >= 0.001, percent_rank(latitude) <= 0.999), aes(x = latitude_1, colour = factor(interest_level))) + 
  geom_density()
ggplot(train %>% filter(percent_rank(longitude) >= 0.001, percent_rank(longitude) <= 0.999), aes(x = longitude_1, colour = factor(interest_level))) + 
  geom_density()

# 11. Photos count. 
# Good feature. Higher interst is associated with one or more photos. 
ggplot(train %>% filter(photos_count <= 20), aes(x = photos_count, colour = factor(interest_level))) + 
  geom_density() + 
  scale_x_continuous(breaks = seq(0, 20, 1))
ggplot(train %>% filter(photos_count <= 20), aes(x = photos_count, fill = factor(interest_level))) + 
  geom_bar(position = "fill") + 
  scale_x_continuous(breaks = seq(0, 20, 1))

# 12. Features count.
# Okay feature. Seems to have a pattern but can't come up with an explanation. 
ggplot(train %>% filter(features_count <= 20), aes(x = features_count, colour = factor(interest_level))) + 
  geom_density() + 
  scale_x_continuous(breaks = seq(0, 20, 1))
ggplot(train %>% filter(features_count <= 20), aes(x = features_count, fill = factor(interest_level))) + 
  geom_bar(position = "fill") + 
  scale_x_continuous(breaks = seq(0, 20, 1))

# 13. Description count and char. 
# Great feature. Zero description count is strongly associated with low interest. 
ggplot(train %>% filter(description_count <= 400), aes(x = description_count, colour = factor(interest_level))) + 
  geom_density()
ggplot(train %>% filter(description_count <= 400), aes(x = description_count, fill = factor(interest_level))) + 
  geom_histogram(binwidth = 5)
table(train$description_count, train$interest_level)
ggplot(train %>% filter(description_nchar <= 2000), aes(x = description_nchar, colour = factor(interest_level))) + 
  geom_density()
ggplot(train %>% filter(description_nchar <= 2000), aes(x = description_nchar, fill = factor(interest_level))) + 
  geom_histogram(binwidth = 10)
table(train$description_nchar, train$interest_level)

# 14. Sentiment. 
ggplot(train, aes(x = positive, colour = factor(interest_level))) + 
  geom_density()
ggplot(train, aes(x = positive, fill = factor(interest_level))) + 
  geom_bar(position = "fill")

# 15. Counts of display_address, manager_id, building_id, and street_address. 
ggplot(train, aes(x = display_address_count, colour = factor(interest_level))) + 
  geom_density()
ggplot(train, aes(x = display_address_count, fill = factor(interest_level))) + 
  geom_histogram(binwidth = 10)
ggplot(train, aes(x = manager_id_count, colour = factor(interest_level))) + 
  geom_density()
ggplot(train, aes(x = manager_id_count, colour = factor(interest_level))) + 
  geom_density() + 
  coord_cartesian(xlim = c(0, 1000))
ggplot(train, aes(x = manager_id_count, fill = factor(interest_level))) + 
  geom_histogram(binwidth = 50)
ggplot(train, aes(x = manager_id_count, fill = factor(interest_level))) + 
  geom_histogram(binwidth = 10) + 
  coord_cartesian(xlim = c(0, 1000))
ggplot(train, aes(x = building_id_count, colour = factor(interest_level))) + 
  geom_density()
ggplot(train, aes(x = building_id_count, fill = factor(interest_level))) + 
  geom_histogram(binwidth = 10) + 
  coord_cartesian(xlim = c(0, 750))
ggplot(train, aes(x = street_address_count, colour = factor(interest_level))) + 
  geom_density()
ggplot(train, aes(x = street_address_count, fill = factor(interest_level))) + 
  geom_histogram(binwidth = 5)



