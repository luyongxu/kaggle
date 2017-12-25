#' ---
#' title: "Train XGBoost"
#' author: "Kevin Lu"
#' output: 
#'   html_document: 
#'     toc: true 
#'     toc_float: true
#'     number_sections: true
#' ---

#' # Set features to use.
features <- setdiff(colnames(train), c("id", "timestamp", "price_doc", "price_doc_log"))
# features <- c("full_sq", "life_sq", "kitch_sq", "floor", "max_floor", 
#               "material", "build_year", "sub_area", "product_type", 
#               "kremlin_km", "cpi", "density_raion", "kitch_full_ratio")

#' # Create train and validate sets. 
train_partial <- train %>% 
  filter(timestamp < "2015-01-01")
train_valid <- train %>% 
  filter(timestamp >= "2015-01-01")

#' # Create xgb objects.
xgtrain_partial <- xgb.DMatrix(data = as.matrix(train_partial[, features]),
                       label = as.matrix(train_partial[, "price_doc"] * 0.968 + 10))
xgtrain_full <- xgb.DMatrix(data = as.matrix(train[, features]), 
                            label = as.matrix(train[, "price_doc"] * 0.968 + 10))
xgtrain_valid <- xgb.DMatrix(data = as.matrix(train_valid[, features]), 
                       label = as.matrix(train_valid[, "price_doc"] * 0.968 + 10))
watchlist <- list(train = xgtrain_partial, test = xgtrain_valid)
xgbparams <- list(booster = "gbtree", 
                  # Tree booster parameters
                  eta = 0.01,
                  gamma = 0, 
                  max_depth = 5,
                  min_child_weight = 1,
                  subsample = 0.7, 
                  colsample_by_tree = 0.7, 
                  # Learning task parameters
                  objective = "reg:linear",
                  eval_metric = "rmse")

#' # Run 5-fold cross validation.
set.seed(55555)
cv <- xgb.cv(data = xgtrain_full, 
             params = xgbparams, 
             # Cross validation parameters
             showsd = TRUE,
             early_stopping_rounds = 100,
             print_every_n = 20,
             nfold = 5,
             nrounds = 10000)
#' [364]	train-rmse:1778887.925000+4612.867017	test-rmse:2551081.550000+123599.906793
#' [2041]	train-rmse:1725556.225000+9788.282015	test-rmse:2515037.650000+121741.082300
#' [2746]	train-rmse:1616508.700000+5862.995411	test-rmse:2525008.250000+127734.155903

#' # Run validation set cross validation. 
set.seed(55555)
cv <- xgb.train(data = xgtrain_partial, 
                params = xgbparams, 
                watchlist = watchlist, 
                early_stopping_rounds = 100, 
                print_every_n = 20, 
                nrounds = 10000)
predictions <- predict(cv, as.matrix(train_valid[, features])) %>%
  as_tibble() %>%
  mutate(id = train_valid[["id"]],
         price_doc_pred = value) %>%
  select(-value) %>% 
  left_join(train_valid %>% select(id, timestamp, price_doc))
summary(predictions)
MLmetrics::RMSE(y_pred = predictions$price_doc_pred, y_true = predictions$price_doc)
ggplot(predictions, aes(x = timestamp)) + 
  geom_point(aes(y = price_doc_pred), alpha = 0.2, colour = "blue") + 
  geom_point(aes(y = price_doc), alpha = 0.2, colour = "red") + 
  coord_cartesian(ylim = c(0, 20000000))
ggplot(predictions) + 
  geom_density(aes(x = price_doc_pred), fill = "blue", alpha = 0.5) + 
  geom_density(aes(x = price_doc), fill = "red", alpha = 0.5) + 
  coord_cartesian(xlim = c(0, 20000000))
#' [2377]	train-rmse:1709610.125000	test-rmse:2724591.500000


#' # Train model.
set.seed(55555)
nrounds <- 2750
model_xgb <- xgb.train(data = xgtrain_full,
                       params = xgbparams, 
                       nrounds = nrounds)
importance <- xgb.importance(model = model_xgb, feature_names = features)
xgb.plot.importance(importance, top_n = 50)

#' # Generate predictions.
# predictions <- predict(model_xgb, as.matrix(test[, features])) %>%
#   as_tibble() %>% 
#   mutate(id = test[["id"]], 
#          price_doc = expm1(value)) %>%
#   select(-value)
predictions <- predict(model_xgb, as.matrix(test[, features])) %>%
  as_tibble() %>%
  mutate(id = test[["id"]],
         price_doc = value * 0.995) %>%
  select(-value)
write_csv(predictions, "./06-sberbank/output/xgb-21.csv")
ggplot(predictions %>% left_join(test %>% select(id, timestamp)), aes(x = timestamp, y = price_doc)) + 
  geom_point(alpha = 0.2) + 
  geom_smooth() + 
  coord_cartesian(ylim = c(0, 15000000))
