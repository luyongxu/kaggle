source("./04-two-sigma-connect/01-load-data.R")
source("./04-two-sigma-connect/03-engineer-features.R")

# 1. Set features to use.
features_to_use <- c("xgb_26_low", "xgb_26_medium", "xgb_26_high", 
                     "xgb_27_low", "xgb_27_medium", "xgb_27_high", 
                     "lgb_03_low", "lgb_03_medium", "lgb_03_high", 
                     "gbm_01_low", "gbm_01_medium", "gbm_01_high", 
                     "ranger_01_low", "ranger_01_medium", "ranger_01_high", 
                     "glmnet_01_low", "glmnet_01_medium", "glmnet_01_high", 
                     "bathrooms", "bedrooms", "latitude", "longitude", "price", "listing_id", 
                     "distance_center", "display_address_count", "manager_id_count", "building_id_count", 
                     "street_address_count", "display_address", "manager_id", "street_address", 
                     "photos_count", "features_count", "description_count", "building_id_zero", 
                     "manager_id_low", "manager_id_medium", "manager_id_high", "photos_zero", 
                     "bathrooms_standard", "created_hour", "price_rank", "price_rank2", 
                     "street_display_sim", colnames(features_dtm))

# 2. Read in meta features. 
read_meta <- function(prefix, source) { 
  if (source == "train") {
    df <- read_csv(str_c("./04-two-sigma-connect/output/", prefix, "-meta.csv")) %>% 
      select(listing_id, low, medium, high)
  }
  if (source == "test") { 
    df <- read_csv(str_c("./04-two-sigma-connect/output/", prefix, ".csv")) %>% 
      select(listing_id, low, medium, high)
  }
  colnames(df) <- c("listing_id", 
                    str_c(prefix, "_low"), 
                    str_c(prefix, "_medium"), 
                    str_c(prefix, "_high"))
  return(df)
}

xgb_26_meta <- read_meta("xgb-26", "train")
xgb_27_meta <- read_meta("xgb-27", "train")
lgb_03_meta <- read_meta("lgb-03", "train")
gbm_01_meta <- read_meta("gbm-01", "train")
ranger_01_meta <- read_meta("ranger-01", "train")
glmnet_01_meta <- read_meta("glmnet-01", "train")

xgb_26 <- read_meta("xgb-26", "test") 
xgb_27 <- read_meta("xgb-27", "test") 
lgb_03 <- read_meta("lgb-03", "test")
gbm_01 <- read_meta("gbm-01", "test")
ranger_01 <- read_meta("ranger-01", "test")
glmnet_01 <- read_meta("glmnet-01", "test")

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
write_csv(predictions, "./04-two-sigma-connect/output/test-ensemble.csv")


