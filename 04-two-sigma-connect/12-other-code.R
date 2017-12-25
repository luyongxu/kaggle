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
# write_csv(random_search, str_c("./04-two-sigma-connect/output/", Sys.Date(), "-random-search.csv"))

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

