# 1. Create development and validation set. 
set.seed(1)
dev_index <- sample(nrow(train), size = nrow(train) * 0.80)
dev_X <- train_X[dev_index, ]
val_X <- train_X[-dev_index, ]
dev_y <- train_y[dev_index]
val_y <- train_y[-dev_index]

# 2. Create development and validation matrix.
xgtrain <- xgb.DMatrix(data = as.matrix(dev_X), label = as.matrix(dev_y))
xgtest = xgb.DMatrix(data = as.matrix(val_X), label = as.matrix(val_y))
watchlist <- list(train = xgtrain, test = xgtest)

# 3. Train XGBoost model on development set, cross validated on validation set. 
# [289]	train-mlogloss:0.341450	test-mlogloss:0.524025
# [254]	train-mlogloss:0.359810	test-mlogloss:0.523627
# [198]	train-mlogloss:0.384498	test-mlogloss:0.519494
# [252]	train-mlogloss:0.352134	test-mlogloss:0.517762
m01 <- xgb.train(data = xgtrain, 
                 watchlist = watchlist, 
                 objective = "multi:softprob", 
                 num_class = 3, 
                 eval_metric = "mlogloss", 
                 nthread = 2, 
                 max.depth = 6, 
                 eta = 0.1, 
                 min_child_weight = 1, 
                 subsample = 0.7, 
                 colsample_bytree = 0.7, 
                 nround = 1000, 
                 verbose = 1, 
                 early_stopping_rounds = 50)

# 4. Create full training matrix. 
xgtrain <- xgb.DMatrix(data = as.matrix(train_X), label = as.matrix(train_y))

# 5. Train XGBoost model on full training set. 
m02 <- xgb.train(data = xgtrain, 
                 objective = "multi:softprob", 
                 num_class = 3, 
                 eval_metric = "mlogloss", 
                 nthread = 2, 
                 max.depth = 6, 
                 eta = 0.1, 
                 min_child_weight = 1, 
                 subsample = 0.7, 
                 colsample_bytree = 0.7, 
                 nround = 300, 
                 verbose = 1)

# 6. Generate predictions.
predictions <- predict(m02, as.matrix(test_X)) %>% 
  matrix(nrow = 3, ncol = nrow(test_X)) %>% 
  t() %>% 
  data.frame() %>% 
  mutate(listing_id = test_X$listing_id) %>% 
  select(listing_id, X1, X2, X3)
colnames(predictions) <- c("listing_id", "low", "medium", "high")

# 7. Write data. 
write_csv(predictions, "./Two Sigma Connect/Output/1.005 XGBoost Model.csv")
