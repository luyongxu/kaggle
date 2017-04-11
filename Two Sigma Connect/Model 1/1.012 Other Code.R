# 8. Tune hyperparemters using random search. 
# 8.1 Initialize output dataframe. 
# random_search <- data.frame()

# 8.2 Random search for best parameters. 
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

# 8.3 Save random search results
# write_csv(random_search, str_c("./Two Sigma Connect/Output/", Sys.Date(), " Random Search.csv"))

# 4. Tune hyperparameters. 
# [Resample] Aggr. Result: logloss.test.mean=0.574
# parameters <- makeParamSet(makeDiscreteParam("num.trees", values = c(10, 30, 100, 200, 300)), 
#                            makeDiscreteParam("mtry", values = c(3, 6, 10, 15, 20, 30)), 
#                            makeDiscreteParam("min.node.size", values = c(1, 3, 10, 30, 100)))
# tune <- tuneParams(learner = learner, 
#                    task = task, 
#                    resampling = resampling, 
#                    measures = logloss, 
#                    par.set = parameters, 
#                    control = makeTuneControlGrid())


# xgb_26 <- read_csv("./Two Sigma Connect/Output/xgb_26.csv") %>% 
#   rename(low_a = low, 
#          medium_a = medium, 
#          high_a = high)
# lgb_03 <- read_csv("./Two Sigma Connect/Output/lgb_03.csv") %>% 
#   rename(low_b = low, 
#          medium_b = medium, 
#          high_b = high)
# en <- xgb_26 %>% 
#   left_join(lgb_03) %>% 
#   mutate(low = 
#            0.5 * low_a + 
#            0.5 * low_b, 
#          medium = 
#            0.5 * medium_a + 
#            0.5 * medium_b, 
#          high = 
#            0.5 * high_a + 
#            0.5 * high_b) %>% 
#   select(listing_id, low, medium, high)
# write_csv(en, "./Two Sigma Connect/Output/ensemble_10.csv")