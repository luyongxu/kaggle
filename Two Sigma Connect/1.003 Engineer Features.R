source("./Two Sigma Connect/1.001 Load Data.R")

# 1. Combine train and test datasets. 
train <- train %>% mutate(source = "train")
test <- test %>% mutate(source = "test")
combined <- bind_rows(train, test)

# 2. Convert interest level to numeric. Required for xgboost. 
combined <- combined %>% 
  mutate(interest_level = factor(interest_level, levels = c("low", "medium", "high")), 
         interest_level = as.numeric(interest_level)- 1)

# 3. Date features. 


# 3. Set features to use. 
features_to_use <- c("bathrooms", "bedrooms", "latitude", "longitude", "price")

# 4. Split back into train and test sets.
train <- combined %>% filter(source == "train")
test <- combined %>% filter(source == "test")
