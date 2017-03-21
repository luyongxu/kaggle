source("./Two Sigma Connect/1.001 Load Data.R")
source("./Two Sigma Connect/1.003 Engineer Features.R")

# 1. Create xgb object. 
xgtrain <- xgb.DMatrix(data = as.matrix(train[, features_to_use]), 
                       label = as.matrix(train[, "interest_level"]))

# 2. Train model.
model_xgb <- xgb.train(data = xgtrain, 
             # General parameters
             booster = "gbtree", 
             # Tree booster parameters
             eta = 0.1, 
             gamma = 0, 
             max_depth = 6, 
             min_child_weight = 1, 
             subsample = 0.7, 
             colsample_by_tree = 0.7, 
             # Learning task parameters
             objective = "multi:softprob", 
             num_class = 3, 
             eval_metric = "mlogloss", 
             seed = 1, 
             # Cross validation parameters
             showsd = TRUE, 
             early_stopping_rounds = 50, 
             print_every_n = 10, 
             nfold = 10, 
             nrounds = 10000)

# 3. Generate predictions.
predictions <- predict(model_xgb, as.matrix(test[, features_to_use])) %>% 
  matrix(nrow = 3, ncol = nrow(test)) %>% 
  t() %>% 
  data.frame() %>% 
  mutate(listing_id = test$listing_id) %>% 
  select(listing_id, X1, X2, X3)
colnames(predictions) <- c("listing_id", "low", "medium", "high")

# 4. Write data. 
write_csv(predictions, "./Two Sigma Connect/Output/model_xgb.csv")
