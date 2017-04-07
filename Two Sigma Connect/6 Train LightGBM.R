source("./Two Sigma Connect/1 Load Data.R")
source("./Two Sigma Connect/3 Engineer Features.R")

# 1. Set features to use. 
features_to_use <- c("bathrooms", "bedrooms", "latitude", "longitude", "price", "listing_id", "distance_center", 
                     "display_address_count", "manager_id_count", "building_id_count", "street_address_count", 
                     "display_address", "manager_id", "street_address", 
                     "photos_count", "features_count", "description_count", "building_id_zero", 
                     "manager_id_low", "manager_id_medium", "manager_id_high", 
                     "photos_zero", "bathrooms_standard", "created_hour", "price_rank", "price_rank2", 
                     "street_display_sim", colnames(features_dtm))

# 2. Create lgb object. 
lgbtrain <- lgb.Dataset(data = as.matrix(train[, features_to_use]), 
                       label = as.matrix(train[, "interest_level"]))
colnames(lgbtrain)

# 3. Run cross validation. 
set.seed(55555)
cv <- lgb.cv(data = lgbtrain, 
             # Core parameters
             objective = "multiclass", 
             num_class = 3, 
             nrounds = 10000, 
             learning_rate = 0.01, 
             num_leaves = 110, 
             num_threads = 4, 
             # Learning control parameters
             max_depth = 6, 
             min_data_in_leaf = 100, 
             feature_fraction = 1, 
             bagging_fraction = 0.8, 
             bagging_freq = 1, 
             early_stopping_rounds = 20, 
             # Metric parameters
             metric = "multi_logloss", 
             # CV parameters
             nfold = 5)
# [2386]:	valid's multi_logloss:0.525676+0.00665359

# 4. Train
set.seed(55555)
model_lgb <- lgb.train(data = lgbtrain, 
                       # Core parameters
                       objective = "multiclass", 
                       num_class = 3, 
                       nrounds = 2386, 
                       learning_rate = 0.01, 
                       num_leaves = 110, 
                       num_threads = 4, 
                       # Learning control parameters
                       max_depth = 6, 
                       min_data_in_leaf = 100, 
                       feature_fraction = 1, 
                       bagging_fraction = 0.8, 
                       bagging_freq = 1, 
                       # Metric parameters
                       metric = "multi_logloss")

# 5. Predict
predictions <- predict(model_lgb, as.matrix(test[, features_to_use]), reshape = TRUE) %>% 
  data.frame() %>% 
  mutate(listing_id = test$listing_id) %>% 
  rename(low = X1, medium = X2, high = X3)

# 6. Write data. 
write_csv(predictions, "./Two Sigma Connect/Output/lgb_02.csv")