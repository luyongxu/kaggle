source("./Two Sigma Connect/1.001 Load Data.R")
source("./Two Sigma Connect/1.003 Engineer Features.R")

# 1. Create xgb object. 
xgtrain <- xgb.DMatrix(data = as.matrix(train[, features_to_use]), 
                       label = as.matrix(train[, "interest_level"]))

# 2. Run cross validation. 
# [264]	train-mlogloss:0.557695+0.001041	test-mlogloss:0.646833+0.008461
cv <- xgb.cv(data = xgtrain, 
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

# 3. Tune hyperparemters using random search. 
# 3.1 Initialize best objects. 
best_param = list()
best_seednumber = 0
best_logloss = Inf
best_logloss_index = 0

# 3.2 Random search for best parameters. 
for (iter in 1:100) {
  print(str_c("Iteration number ", iter, "."))
  seed_number = sample(10000, 1)
  cv <- xgb.cv(data = xgtrain, 
               # General parameters
               booster = "gbtree", 
               # Tree booster parameters
               eta = runif(1, .01, .3), 
               gamma = runif(1, 0.0, 0.2), 
               max_depth = sample(4:10, 1), 
               min_child_weight = sample(1:40, 1), 
               subsample = runif(1, .5, 1.0), 
               colsample_bytree = runif(1, .5, 1.0), 
               max_delta_step = sample(1:10, 1), 
               # Learning task parameters
               objective = "multi:softprob", 
               num_class = 3, 
               eval_metric = "mlogloss", 
               seed = seed_number, 
               # Cross validation parameters
               showsd = TRUE, 
               early_stopping_rounds = 50, 
               print_every_n = 50, 
               nfold = 10, 
               nrounds = 10000) 
  # Extract best iteration number and best log loss. 
  min_logloss_index = cv[["best_iteration"]]
  min_logloss = as.numeric(cv[["evaluation_log"]][min_logloss_index, "test_mlogloss_mean"])
  # Keep track of which set of random parameters produces best log loss. 
  if (min_logloss < best_logloss) {
    best_logloss = min_logloss
    best_logloss_index = min_logloss_index
    best_seednumber = seed_number
    best_param = cv[["params"]]
  }
}

