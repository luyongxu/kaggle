# 1. Combine train and test datasets. 
train <- train %>% mutate(source = "train")
test <- test %>% mutate(source = "test")
combined <- bind_rows(train, test)

# 2. Convert interest level to numeric. Required for xgboost. 
combined <- combined %>% 
  mutate(interest_level = factor(interest_level, levels = c("low", "medium", "high")), 
         interest_level = as.numeric(interest_level) - 1)

# 3. Distance to city center. 
combined <- combined %>%
  mutate(distance_center = sqrt((longitude - -73.968285)^2 + (latitude - 40.785091)^2))

# 4. Counts of display_address, manager_id, building_id, and street_address.
combined <- combined %>%
  left_join(combined %>% group_by(display_address) %>% summarise(display_address_count = n())) %>%
  left_join(combined %>% group_by(manager_id) %>% summarise(manager_id_count = n())) %>%
  left_join(combined %>% group_by(building_id) %>% summarise(building_id_count = n())) %>%
  left_join(combined %>% group_by(street_address) %>% summarise(street_address_count = n()))

# 5. Create geohash
combined <- combined %>% 
  mutate(geohash = geohash::gh_encode(latitude, longitude, 5))

# 6. Mean encode high cardinality categorical variables. 
mean_encode <- function(df, categorical_var) { 
  df <- df %>% 
    mutate(low = ifelse(interest_level == 0, 1, 0), 
           medium = ifelse(interest_level == 1, 1, 0), 
           high = ifelse(interest_level == 2, 1, 0), 
           fold = c(cut(seq(1, nrow(train)), breaks = 5, labels = FALSE), rep(NA, nrow(test))))
  pred <- data.frame()
  for (i in 1:5) { 
    df_train <- df %>% filter(fold != i)
    df_test <- df %>% filter(fold == i)
    model_low <- lmer(paste("low ~ (1 | ", categorical_var, ")"), data = df_train)
    model_medium <- lmer(paste("medium ~ (1 | ", categorical_var, ")"), data = df_train)
    model_high <- lmer(paste("high ~ (1 | ", categorical_var, ")"), data = df_train)
    df_test <- df_test %>% 
      mutate(categorical_var_low = predict(model_low, df_test, allow.new.levels = TRUE), 
             categorical_var_medium = predict(model_medium, df_test, allow.new.levels = TRUE), 
             categorical_var_high = predict(model_high, df_test, allow.new.levels = TRUE))
    pred <- bind_rows(pred, df_test)
  }
  df_train <- df %>% filter(is.numeric(fold))
  df_test <- df %>% filter(is.na(fold))
  model_low <- lmer(paste("low ~ (1 | ", categorical_var, ")"), data = df_train)
  model_medium <- lmer(paste("medium ~ (1 | ", categorical_var, ")"), data = df_train)
  model_high <- lmer(paste("high ~ (1 | ", categorical_var, ")"), data = df_train)
  df_test <- df_test %>% 
    mutate(categorical_var_low = predict(model_low, df_test, allow.new.levels = TRUE), 
           categorical_var_medium = predict(model_medium, df_test, allow.new.levels = TRUE), 
           categorical_var_high = predict(model_high, df_test, allow.new.levels = TRUE))
  pred <- bind_rows(pred, df_test)
  return(pred)
}
manager_pred <- mean_encode(combined, "manager_id") %>% 
  select(manager_id_low = categorical_var_low, 
         manager_id_medium = categorical_var_medium, 
         manager_id_high = categorical_var_high)
geohash_pred <- mean_encode(combined, "geohash") %>% 
  select(geohash_low = categorical_var_low, 
         geohash_medium = categorical_var_medium, 
         geohash_high = categorical_var_high)
combined <- bind_cols(combined, manager_pred, geohash_pred)

# 7. Label encode categorical features.
combined <- combined %>%
  mutate(display_address = ifelse(display_address_count == 1, "-1", display_address), 
         manager_id = ifelse(manager_id_count == 1, "-1", manager_id), 
         building_id = ifelse(building_id_count == 1, "-1", building_id), 
         street_address = ifelse(street_address_count == 1, "-1", street_address), 
         display_address = as.numeric(factor(display_address)),
         manager_id = as.numeric(factor(manager_id)),
         building_id = as.numeric(factor(building_id)),
         street_address = as.numeric(factor(street_address)))

# 8. Count of pictures, features, words description.
combined <- combined %>%
  mutate(photos_count = lengths(photos),
         features_count = lengths(features),
         description_count = str_count(description, "\\S+"))

# 9. Building id of 0. This is probably where building id is not listed.
# There are 8286 records with building id of 0 and it is strongly associated with low interest.
combined <- combined %>%
  mutate(building_id_zero = ifelse(building_id == "0", 1, 0))

# 10. Features. Select features that have occured in 20 or more listings. Create document term matrix.
features_top <- combined %>%
  mutate(features = ifelse(map(features, is_empty), c("empty"), features)) %>%
  select(listing_id, features, interest_level) %>%
  unnest(features) %>%
  mutate(features = tolower(features)) %>%
  count(features) %>%
  filter(n >= 20)
features_dtm <- combined %>%
  mutate(features = ifelse(map(features, is_empty), c("empty"), features)) %>%
  select(listing_id, features, interest_level) %>%
  unnest(features) %>%
  mutate(features = tolower(features)) %>%
  count(listing_id, features) %>%
  filter(features %in% features_top$features) %>%
  spread(features, n, fill = 0) %>%
  ungroup()
colnames(features_dtm) <- make.names(colnames(features_dtm), unique = TRUE)
combined <- combined %>%
  left_join(features_dtm)
features_dtm <- features_dtm %>% 
  select(-listing_id)

# 11. Date features.
combined <- combined %>%
  mutate(created_week = week(created),
         created_yday = yday(created),
         created_day = day(created),
         created_wday = wday(created),
         created_hour = hour(created))

# 12. Misc features. 
combined <- combined %>%
  mutate(price_per_bed = ifelse(is.finite(price / bedrooms), price / bedrooms, NA), 
         photos_zero = ifelse(photos_count == 0, 1, 0), 
         bathrooms_standard = ifelse(bathrooms %in% c(1, 2, 3), 1, 0))

# 13. Price rank by room and geohash. 
combined <- combined %>% 
  arrange(price) %>% 
  group_by(bathrooms, bedrooms) %>% 
  mutate(price_rank = percent_rank(price))
combined <- combined %>% 
  group_by(geohash) %>% 
  mutate(price_rank2 = percent_rank(price))

# 14. Street address and display address similarity. 
combined <- combined %>% 
  mutate(street_display_sim = RecordLinkage::levenshteinSim(tolower(street_address), tolower(display_address)))

# 15. Split back into train and test sets.
train <- combined %>% filter(source == "train") %>% ungroup()
test <- combined %>% filter(source == "test") %>% ungroup()


