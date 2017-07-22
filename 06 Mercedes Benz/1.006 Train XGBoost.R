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
features <- setdiff(colnames(train), c("y", "fold", "X0_factor", "X1_factor", "X2_factor", "X3_factor", 
                                       "X4_factor", "X5_factor", "X6_factor", "X8_factor"))
print(features)

#' # Create r2 metric
r2_metric <- function(pred, xgtrain) { 
  labels <- getinfo(xgtrain, "label")
  r2 <- 1 - (sum((labels - pred)^2) / sum((labels-mean(labels))^2))
  return(list(metric = "r2", value = r2))
}

#' # Create xgb objects.
xgtrain <- xgb.DMatrix(data = as.matrix(train[, features]),
                       label = as.matrix(train[, "y"]))
xgtest <- xgb.DMatrix(data = as.matrix(test[, features]))
xgbparams <- list(booster = "gbtree", 
                  # Tree booster parameters
                  eta = 0.005,
                  gamma = 5,
                  max_depth = 2,
                  min_child_weight = 5,
                  subsample = 0.6, 
                  colsample_by_tree = 0.7, 
                  base_score = mean(train$y), 
                  # Learning task parameters
                  objective = "reg:linear", 
                  eval_metric = r2_metric)

#' # Run cross validation. 
cv_results <- tibble()
for (i in 1:10) { 
  set.seed(i)
  print(str_c("Starting iteration ", i, "."))
  cv <- xgb.cv(data = xgtrain, 
               params = xgbparams, 
               # Cross validation parameters
               showsd = TRUE,
               early_stopping_rounds = 50,
               print_every_n = 50, 
               maximize = TRUE, 
               nfold = 5,
               nrounds = 10000, 
               prediction = TRUE)
  cv_results <- bind_rows(cv_results, 
                          tibble(seed = i, 
                                 r2 = MLmetrics::R2_Score(cv$pred, train$y), 
                                 best_iteration = cv$best_iteration))
}
cv_results %>% map_dbl(mean)
nrounds <- cv_results %>% map_dbl(mean) %>% .[["best_iteration"]] %>% round()
# seed             r2 best_iteration 
# 5.5000000      0.5692077     62.8000000 
# seed             r2 best_iteration 
# 5.5000000      0.5681277    122.3000000 
# seed             r2 best_iteration 
# 5.5000000      0.5677339     93.6000000

 
#' # Train model.
predictions_results <- tibble()
for (i in 1:10) { 
  set.seed(i)
  print(str_c("Training model ", i, "."))
  model_xgb <- xgb.train(data = xgtrain,
                         params = xgbparams, 
                         nrounds = cv_results[[i, "best_iteration"]])
  predictions <- predict(model_xgb, xgtest) %>% 
    as_tibble() %>% 
    mutate(ID = test$ID) %>% 
    select(ID, y = value)
  predictions_results <- bind_rows(predictions_results, predictions)
}

#' # Importance. 
importance <- xgb.importance(model = model_xgb, 
                             feature_names = features)

#' # Generate predictions.
predictions_results <- predictions_results %>% 
  group_by(ID) %>% 
  summarise(y = mean(y))
write_csv(predictions, "./Mercedes Benz/Output/Base Models/xgb_16.csv")





