source("./Two Sigma Connect/1.001 Load Data.R")

# 1. Combine train and test datasets. 
train <- train %>% mutate(source = "train")
test <- test %>% mutate(source = "test")
combined <- bind_rows(train, test)

# 2. Convert interest level to factor. 
combined <- combined %>% 
  mutate(interest_level = factor(interest_level, levels = c("low", "medium", "high")), 
         building_id = factor(building_id), 
         manager_id = factor(manager_id)) 

# 3. Parse created date. 
combined <- combined %>% 
  mutate(created = as.POSIXct(created), 
         created_yday = yday(created), 
         created_month = month(created), 
         created_day = day(created), 
         created_wday = wday(created), 
         created_hour = hour(created))

# 4. Count of instances display address appeared. 
display <- combined %>% 
  group_by(display_address) %>% 
  summarise(display_address_count = n())
combined <- combined %>% 
  left_join(display)

# 5. Count of pictures per listing. 
combined <- combined %>% 
  mutate(photos_count = lengths(photos))

# 6. Count of features per listing. 
combined <- combined %>% 
  mutate(features_count = lengths(features))

# 7. Length of description. 
combined <- combined %>% 
  mutate(description_nchar = nchar(description))

# 8. Listing features.
combined <- combined %>% 
  mutate(features = sapply(features, tolower))
features <- data.frame(table(unlist(combined$features))) %>%
  filter(Freq >= 250)
colnames(features) <- c("top_features", "freq")
features_expand <- data.frame(t(sapply(combined$features, function(x) as.numeric(features$top_features %in% x))))
combined <- bind_cols(combined, features_expand)

# 9. Keep model features. 
remove_features <- c("created", "description", "display_address", "features", "photos", "street_address")
combined <- combined[, !(colnames(combined) %in% remove_features)]

# 10. Split back into train and test sets. 
train <- combined %>% filter(source == "train") %>% select(-source, -listing_id)
test <- combined %>% filter(source == "test") %>% select(-source)

