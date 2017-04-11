source("./Two Sigma Connect/1.001 Load Data.R")
source("./Two Sigma Connect/1.003 Engineer Features.R")

# 1. Set features to use.
features_to_use <- c("bathrooms", "bedrooms", "latitude", "longitude", "price", "listing_id", 
                     "distance_center", "display_address_count", "manager_id_count", "building_id_count", 
                     "street_address_count", "display_address", "manager_id", "street_address", 
                     "photos_count", "features_count", "description_count", "building_id_zero", 
                     "manager_id_low", "manager_id_medium", "manager_id_high", "photos_zero", 
                     "bathrooms_standard", "created_hour", "price_rank", "price_rank2", 
                     "street_display_sim", colnames(features_dtm))

# 2. Create xgb objects.
xgtrain <- xgb.DMatrix(data = as.matrix(train[, features_to_use]),
                       label = as.matrix(train[, "interest_level"]))
xgbparams <- list(booster = "dart", 
                  # Dart booster parameters
                  sample_type = "uniform", 
                  normalize_type = "tree", 
                  rate_drop = 0, 
                  one_drop = 0, 
                  skip_drop = 0, 
                  # Tree booster parameters
                  eta = 0.1,
                  gamma = 1,
                  max_depth = 4,
                  min_child_weight = 1,
                  subsample = 0.7, 
                  colsample_by_tree = 0.5, 
                  # Learning task parameters
                  objective = "multi:softprob", 
                  num_class = 3,
                  eval_metric = "mlogloss")
nrounds <- 566
set.seed(55555)

# 3. Run cross validation.
cv <- xgb.cv(data = xgtrain, 
             params = xgbparams, 
             # Cross validation parameters
             showsd = TRUE,
             early_stopping_rounds = 20,
             print_every_n = 20,
             nfold = 5,
             nrounds = 10000)
# [566]	train-mlogloss:0.401085+0.001849	test-mlogloss:0.524846+0.005378

# 4. Train model.
model_xgb <- xgb.train(data = xgtrain,
                       params = xgbparams, 
                       nrounds = nrounds)
importance <- xgb.importance(model = model_xgb, feature_names = features_to_use)
xgb.plot.importance(importance)

# 5. Generate predictions.
predictions <- predict(model_xgb, as.matrix(test[, features_to_use])) %>%
  matrix(nrow = 3, ncol = nrow(test)) %>%
  t() %>%
  data.frame() %>%
  mutate(listing_id = test$listing_id) %>%
  select(listing_id, X1, X2, X3)
colnames(predictions) <- c("listing_id", "low", "medium", "high")
write_csv(predictions, "./Two Sigma Connect/Output/Base Models/xgb_27.csv")

# 6. Generate metafeatures for stacking. 
train <- train %>%
  mutate(fold = sample(cut(seq(1, nrow(train)), breaks = 5, labels = FALSE)))
predictions_df <- data.frame()
for (i in 1:5) {
  print(str_c("Generating metafeatures for fold ", i, "."))
  meta_train <- train %>% filter(fold != i)
  meta_test <- train %>% filter(fold == i)
  xgtrain <- xgb.DMatrix(data = as.matrix(meta_train[, features_to_use]),
                         label = as.matrix(meta_train[, "interest_level"]))
  model_xgb <- xgb.train(data = xgtrain,
                         params = xgbparams, 
                         nrounds = nrounds)
  predictions <- predict(model_xgb, as.matrix(meta_test[, features_to_use])) %>%
    matrix(nrow = 3, ncol = nrow(meta_test)) %>%
    t() %>%
    data.frame() %>%
    mutate(listing_id = meta_test$listing_id) %>%
    select(listing_id, X1, X2, X3)
  colnames(predictions) <- c("listing_id", "low", "medium", "high")
  predictions_df <- bind_rows(predictions_df, predictions)
}
write_csv(predictions_df, "./Two Sigma Connect/Output/Meta Features/xgb_27_meta.csv")



