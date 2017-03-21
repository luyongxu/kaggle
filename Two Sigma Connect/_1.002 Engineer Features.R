source("./Two Sigma Connect/1.001 Load Data.R")

# 1. Combine train and test datasets. 
train <- train %>% mutate(source = "train")
test <- test %>% mutate(source = "test")
combined <- bind_rows(train, test)

# 2. Count of pictures, features, and words in description. 
combined <- combined %>%
  mutate(photos_count = lengths(photos), 
         features_count = lengths(features), 
         description_count = str_count(description, "\\S+"), 
         description_nchar = nchar(description))

# 3. Create created date features.
combined <- combined %>%
  mutate(created = as.POSIXct(created), 
         created_year = year(created), 
         created_yday = yday(created), 
         created_month = month(created), 
         created_day = day(created),
         created_wday = wday(created), 
         created_hour = hour(created))

# 4. Distance to city center. 
combined <- combined %>% 
  mutate(distance_center = sqrt((longitude - -73.968285)^2 + (latitude - 40.785091)^2))

# 5. Fixing outliers. 
# outliers <- combined %>% 
#   filter(distance_city >= 5) %>% 
#   select(street_address) %>% 
#   mutate(street_address = paste0(street_address, ", new york"))
# outliers_location <- geocode(outliers$street_address, source = "google")
# write_csv(outliers_location, "./Two Sigma Connect/Raw Data/outliers_location.csv")
outliers_location <- read_csv("./Two Sigma Connect/Raw Data/outliers_location.csv")
combined[combined$distance_city >= 5, "longitude"] <- outliers_location$lon
combined[combined$distance_city >= 5, "latitude"] <- outliers_location$lat
combined <- combined %>% 
  mutate(distance_city = sqrt((longitude - -73.968285)^2 + (latitude - 40.785091)^2))

# 6. Sentiment analysis. 
sentiment <- get_nrc_sentiment(combined$description)
combined <- bind_cols(combined, sentiment)

# 7. Counts of display_address, manager_id, building_id, and street_address. 
combined <- combined %>% 
  left_join(combined %>% group_by(display_address) %>% summarise(display_address_n = n())) %>% 
  left_join(combined %>% group_by(manager_id) %>% summarise(manager_id_n = n())) %>% 
  left_join(combined %>% group_by(building_id) %>% summarise(building_id_n = n())) %>% 
  left_join(combined %>% group_by(street_address) %>% summarise(street_address_n = n()))

# 8. Convert categorical features to numeric. 
combined <- combined %>% 
  mutate(display_address = as.numeric(factor(display_address)), 
         manager_id = as.numeric(factor(manager_id)), 
         building_id = as.numeric(factor(building_id)), 
         street_address = as.numeric(factor(street_address)))

# 9. Convert interest level to factor. 
combined <- combined %>%
  mutate(interest_level = factor(interest_level, levels = c("low", "medium", "high")))

# 10. Manager skill. 
manager_skill <- lmer(as.numeric(interest_level) ~ (1 | manager_id), data = combined)
combined <- combined %>% 
  mutate(manager_skill_pred = as.numeric(predict(manager_skill, combined, allow.new.levels = TRUE))) %>% 
  mutate(manager_skill_pred = ifelse(manager_id_n <= 20, 
                                     mean(as.numeric(combined$interest_level), na.rm = TRUE), 
                                     manager_skill_pred))


# 11. No building id indicator.  
combined <- combined %>% 
  mutate(no_building_id = ifelse(building_id == 1, 1, 0))

# 12. Listing features.
combined <- combined %>%
  mutate(features = sapply(features, tolower))
features <- data.frame(table(unlist(combined$features))) %>%
  filter(Freq >= 20)
colnames(features) <- c("top_features", "freq")
features_expand <- data.frame(t(sapply(combined$features, function(x) as.numeric(features$top_features %in% x))))
combined <- bind_cols(combined, features_expand)

# 13. Price per bedroom and bathroom.
combined <- combined %>% 
  mutate(price_bedroom = price / bedrooms, 
         price_bathroom = price / bathrooms)

# 14. Generalize latitude and longitude. 
combined <- combined %>% 
  mutate(latitude_3 = round(latitude, 3), 
         latitude_2 = round(latitude, 2), 
         latitude_1 = round(latitude, 1), 
         latitude_0 = round(latitude, 0), 
         longitude_3 = round(longitude, 3), 
         longitude_2 = round(longitude, 2), 
         longitude_1 = round(longitude, 1), 
         longitude_0 = round(longitude, 0))


# 15. Set features to use. 
features_to_use <- c("bathrooms", "bedrooms", "latitude", "longitude", "price", 
                     "photos_count", "features_count", "description_count", 
                     "created_year", "created_yday", "created_month", "created_day", 
                     "created_wday", "created_hour", "distance_center", 
                     "display_address_n", "manager_id_n", "building_id_n", "street_address_n", 
                     "display_address", "manager_id", "building_id", "street_address", "listing_id", 
                     "manager_skill_pred", "no_building_id", "price_bedroom", "price_bathroom", 
                     "latitude_3", "latitude_2", "latitude_1", "latitude_0", 
                     "longitude_3", "longitude_2", "longitude_1", "longitude_0", 
                     colnames(sentiment), colnames(features_expand))

# 16. Save data. 
write_csv(combined, "./Two Sigma Connect/Raw Data/combined.csv")

# 17. Split back into train and test sets.
train <- combined %>% filter(source == "train")
test <- combined %>% filter(source == "test")
train_X = train[, features_to_use]
test_X = test[, features_to_use]
train_y = as.numeric(factor(train$interest_level)) - 1

