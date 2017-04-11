source("./Two Sigma Connect/1.001 Load Data.R")
source("./Two Sigma Connect/1.003 Engineer Features.R")

# 1. Set features to use.
features_to_use <- c("xgb_26_low", "xgb_26_medium", "xgb_26_high", 
                     "xgb_27_low", "xgb_27_medium", "xgb_27_high", 
                     "lgb_03_low", "lgb_03_medium", "lgb_03_high", 
                     "gbm_01_low", "gbm_01_medium", "gbm_01_high", 
                     "ranger_01_low", "ranger_01_medium", "ranger_01_high", 
                     "glmnet_01_low", "glmnet_01_medium", "glmnet_01_high", 
                     "price_rank", "price_rank2")

# 2. Read in meta features. 
read_meta <- function(prefix, source) { 
  if (source == "train") {
    df <- read_csv(str_c("./Two Sigma Connect/Output/Meta Features/", prefix, "_meta.csv")) %>% 
      select(listing_id, low, medium, high)
  }
  if (source == "test") { 
    df <- read_csv(str_c("./Two Sigma Connect/Output/Base Models/", prefix, ".csv")) %>% 
      select(listing_id, low, medium, high)
  }
  colnames(df) <- c("listing_id", 
                    str_c(prefix, "_low"), 
                    str_c(prefix, "_medium"), 
                    str_c(prefix, "_high"))
  return(df)
}

xgb_26_meta <- read_meta("xgb_26", "train")
xgb_27_meta <- read_meta("xgb_27", "train")
lgb_03_meta <- read_meta("lgb_03", "train")
gbm_01_meta <- read_meta("gbm_01", "train")
ranger_01_meta <- read_meta("ranger_01", "train")
glmnet_01_meta <- read_meta("glmnet_01", "train")

xgb_26 <- read_meta("xgb_26", "test") 
xgb_27 <- read_meta("xgb_27", "test") 
lgb_03 <- read_meta("lgb_03", "test")
gbm_01 <- read_meta("gbm_01", "test")
ranger_01 <- read_meta("ranger_01", "test")
glmnet_01 <- read_meta("glmnet_01", "test")

# 3. Join meta features on train and test. 
train <- train %>% 
  left_join(xgb_26_meta) %>%
  left_join(xgb_27_meta) %>% 
  left_join(lgb_03_meta) %>% 
  left_join(gbm_01_meta) %>% 
  left_join(ranger_01_meta) %>% 
  left_join(glmnet_01_meta)
test <- test %>% 
  left_join(xgb_26) %>% 
  left_join(xgb_27) %>% 
  left_join(lgb_03) %>% 
  left_join(gbm_01) %>% 
  left_join(ranger_01) %>% 
  left_join(glmnet_01)

# 4. Create xgb object.
xgtrain <- xgb.DMatrix(data = as.matrix(train[, features_to_use]),
                       label = as.matrix(train[, "interest_level"]))
xgbparams <- list(booster = "gbtree", 
                  # Tree booster parameters
                  eta = 0.1,
                  gamma = 3,
                  max_depth = 2,
                  min_child_weight = 1,
                  subsample = 0.6, 
                  colsample_by_tree = 0.5, 
                  # Learning task parameters
                  objective = "multi:softprob",
                  num_class = 3,
                  eval_metric = "mlogloss")
nrounds <- 129
set.seed(55555)

# 5. Run cross validation.
cv <- xgb.cv(data = xgtrain, 
             params = xgbparams, 
             # Cross validation parameters
             showsd = TRUE,
             early_stopping_rounds = 20,
             print_every_n = 20,
             nfold = 5,
             nrounds = 10000)
# [129]	train-mlogloss:0.508038+0.001348	test-mlogloss:0.517369+0.005456


# 6. Train model.
model_xgb <- xgb.train(data = xgtrain,
                       params = xgbparams, 
                       nrounds = nrounds)
importance <- xgb.importance(model = model_xgb, feature_names = features_to_use)
xgb.plot.importance(importance)

# 7. Generate predictions.
predictions <- predict(model_xgb, as.matrix(test[, features_to_use])) %>%
  matrix(nrow = 3, ncol = nrow(test)) %>%
  t() %>%
  data.frame() %>%
  mutate(listing_id = test$listing_id) %>%
  select(listing_id, X1, X2, X3)
colnames(predictions) <- c("listing_id", "low", "medium", "high")
write_csv(predictions, "./Two Sigma Connect/Output/Ensemble Models/test_ensemble.csv")


