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

# # 2. Create xgb object. 
# xgtrain <- xgb.DMatrix(data = as.matrix(train[, features_to_use]), 
#                        label = as.matrix(train[, "interest_level"]))
# colnames(xgtrain)

# # 3. Run cross validation. 
# set.seed(55555)
# cv <- xgb.cv(data = xgtrain, 
#              # General parameters
#              booster = "gbtree", 
#              # Tree booster parameters
#              eta = 0.01, 
#              gamma = 1, 
#              max_depth = 4, 
#              min_child_weight = 1, 
#              subsample = 0.7, 
#              # Learning task parameters
#              objective = "multi:softprob", 
#              num_class = 3, 
#              eval_metric = "mlogloss", 
#              # Cross validation parameters
#              showsd = TRUE, 
#              early_stopping_rounds = 20, 
#              print_every_n = 20, 
#              nfold = 5, 
#              nrounds = 10000)
# 
# # 4. Train model.
# set.seed(55555)
# model_xgb <- xgb.train(data = xgtrain, 
#                        # General parameters
#                        booster = "gbtree", 
#                        # Tree booster parameters
#                        eta = 0.01, 
#                        gamma = 1, 
#                        max_depth = 4, 
#                        min_child_weight = 1, 
#                        subsample = 0.7, 
#                        colsample_by_tree = 0.5, 
#                        # Learning task parameters
#                        objective = "multi:softprob", 
#                        num_class = 3, 
#                        eval_metric = "mlogloss", 
#                        seed = 1, 
#                        # Cross validation parameters
#                        showsd = TRUE, 
#                        print_every_n = 10, 
#                        nrounds = 6115)
# importance <- xgb.importance(model = model_xgb, feature_names = features_to_use)
# xgb.plot.importance(importance)
# 
# # 5. Generate predictions.
# predictions <- predict(model_xgb, as.matrix(test[, features_to_use])) %>% 
#   matrix(nrow = 3, ncol = nrow(test)) %>% 
#   t() %>% 
#   data.frame() %>% 
#   mutate(listing_id = test$listing_id) %>% 
#   select(listing_id, X1, X2, X3)
# colnames(predictions) <- c("listing_id", "low", "medium", "high")
# 
# # 6. Write data. 
# write_csv(predictions, "./Two Sigma Connect/Output/xgb_23.csv")

# 7. Generate metafeatures for stacking. 
set.seed(55555)
train <- train %>% 
  mutate(fold = cut(seq(1, nrow(train)), breaks = 5, labels = FALSE))
predictions_df <- data.frame()
for (i in 1:5) { 
  print(str_c("Generating metafeatures for fold ", i, "."))
  meta_train <- train %>% filter(fold != i)
  meta_test <- train %>% filter(fold == i)
  xgtrain <- xgb.DMatrix(data = as.matrix(meta_train[, features_to_use]),
                         label = as.matrix(meta_train[, "interest_level"]))
  model_xgb <- xgb.train(data = xgtrain,
                         # General parameters
                         booster = "gbtree",
                         # Tree booster parameters
                         eta = 0.01,
                         gamma = 1,
                         max_depth = 4,
                         min_child_weight = 1,
                         subsample = 0.7,
                         colsample_by_tree = 0.5,
                         # Learning task parameters
                         objective = "multi:softprob",
                         num_class = 3,
                         eval_metric = "mlogloss",
                         seed = 1,
                         # Cross validation parameters
                         showsd = TRUE,
                         print_every_n = 10,
                         nrounds = 6115)
  predictions <- predict(model_xgb, as.matrix(meta_test[, features_to_use])) %>% 
    matrix(nrow = 3, ncol = nrow(meta_test)) %>%
    t() %>%
    data.frame() %>%
    mutate(listing_id = meta_test$listing_id) %>%
    select(listing_id, X1, X2, X3)
  colnames(predictions) <- c("listing_id", "low", "medium", "high")
  predictions_df <- bind_rows(predictions_df, predictions)
}
write_csv(predictions_df, "./Two Sigma Connect/Output/xgb_23_metafeatures.csv")


# 7. Tune hyperparemters using random search. 
# 4.1 Initialize output dataframe. 
# random_search <- data.frame()

# 7.2 Random search for best parameters. 
# for (iter in 1:100) {
#   print(str_c("Iteration number ", iter, "."))
#   seed_number = sample(10000, 1)
#   cv <- xgb.cv(data = xgtrain, 
#                # General parameters
#                booster = "gbtree", 
#                # Tree booster parameters
#                eta = runif(1, .01, .2), 
#                gamma = runif(1, 0.0, 0.2), 
#                max_depth = sample(2:10, 1), 
#                min_child_weight = sample(1:20, 1), 
#                subsample = runif(1, .4, 1.0), 
#                colsample_bytree = runif(1, .4, 1.0), 
#                max_delta_step = sample(1:10, 1), 
#                # Learning task parameters
#                objective = "multi:softprob", 
#                num_class = 3, 
#                eval_metric = "mlogloss", 
#                seed = seed_number, 
#                # Cross validation parameters
#                showsd = TRUE, 
#                early_stopping_rounds = 50, 
#                print_every_n = 50, 
#                nfold = 5, 
#                nrounds = 10000) 
#   # Extract best iteration number and best log loss. 
#   temp_df <- data.frame(nrounds = cv[["best_iteration"]], 
#                         logloss = as.numeric(cv[["evaluation_log"]][cv[["best_iteration"]], "test_mlogloss_mean"]), 
#                         param = cv[["params"]], 
#                         seed = seed_number)
#   random_search <- bind_rows(random_search, temp_df)
# }

# 7.3 Save random search results
# write_csv(random_search, str_c("./Two Sigma Connect/Output/", Sys.Date(), " Random Search.csv"))

