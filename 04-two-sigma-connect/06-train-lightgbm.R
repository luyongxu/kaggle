source("./04-two-sigma-connect/01-load-data.R")
source("./04-two-sigma-connect/03-engineer-features.R")

# 1. Set features to use.
features_to_use <- c("bathrooms", "bedrooms", "latitude", "longitude", "price", "listing_id", 
                     "distance_center", "display_address_count", "manager_id_count", "building_id_count", 
                     "street_address_count", "display_address", "manager_id", "street_address", 
                     "photos_count", "features_count", "description_count", "building_id_zero", 
                     "manager_id_low", "manager_id_medium", "manager_id_high", "photos_zero", 
                     "bathrooms_standard", "created_hour", "price_rank", "price_rank2", 
                     "street_display_sim", colnames(features_dtm))

# 2. Create lgb objects.
lgbtrain <- lgb.Dataset(data = as.matrix(train[, features_to_use]),
                       label = as.matrix(train[, "interest_level"]))
lgbparams <- list(objective = "multiclass", 
                  # Core parameters
                  num_class = 3, 
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
nrounds <- 2446
set.seed(55555)

# 3. Run cross validation. 
cv <- lgb.cv(data = lgbtrain,
             params = lgbparams,
             nrounds = 10000,
             early_stopping_rounds = 20,
             nfold = 5)
# [2446]:	valid's multi_logloss:0.524691+0.00650659

# 4. Train
model_lgb <- lgb.train(data = lgbtrain, 
                       params = lgbparams, 
                       nrounds = nrounds)

# 5. Predict
predictions <- predict(model_lgb, as.matrix(test[, features_to_use]), reshape = TRUE) %>%
  data.frame() %>%
  mutate(listing_id = test$listing_id) %>%
  rename(low = X1, medium = X2, high = X3)
write_csv(predictions, "./04-two-sigma-connect/output/lgb-03.csv")

# 6. Generate metafeatures for stacking.
train <- train %>%
  mutate(fold = sample(cut(seq(1, nrow(train)), breaks = 5, labels = FALSE)))
predictions_df <- data.frame()
for (i in 1:5) {
  print(str_c("Generating metafeatures for fold ", i, "."))
  meta_train <- train %>% filter(fold != i)
  meta_test <- train %>% filter(fold == i)
  lgbtrain <- lgb.Dataset(data = as.matrix(meta_train[, features_to_use]),
                          label = as.matrix(meta_train[, "interest_level"]))
  model_lgb <- lgb.train(data = lgbtrain, 
                         params = lgbparams, 
                         nrounds = nrounds)
  predictions <- predict(model_lgb, as.matrix(meta_test[, features_to_use]), reshape = TRUE) %>%
    data.frame() %>%
    mutate(listing_id = meta_test$listing_id) %>%
    rename(low = X1, medium = X2, high = X3)
  predictions_df <- bind_rows(predictions_df, predictions)
}
write_csv(predictions_df, "./04-two-sigma-connect/output/lgb-03-meta.csv")

