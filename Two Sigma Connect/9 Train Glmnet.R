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

# 2. Split up into train and test sets. 
train <- train[, c(features_to_use, "interest_level")] %>% 
  mutate(interest_level = factor(interest_level))
train[is.na(train)] <- 0
test <- test[, c(features_to_use, "interest_level")]
test[is.na(test)] <- 0

# 3. Set up mlr objects. 
set.seed(55555)
task <- makeClassifTask(id = "main", 
                        data = train, 
                        target = "interest_level")
learner <- makeLearner("classif.cvglmnet", 
                       alpha = 0, 
                       nlambda = 20, 
                       standardize = TRUE, 
                       predict.type = "prob")
resampling <- makeResampleDesc(method = "CV", 
                               stratify = TRUE, 
                               iters = 5)
resample <- resample(learner = learner, 
                     task = task, 
                     resampling = resampling, 
                     measures = logloss, 
                     keep.pred = FALSE)
# [Resample] Aggr. Result: logloss.test.mean=0.593

# 4. Train model. 
set.seed(55555)
model <- train(learner, task)

# 5. Generate predictions. 
predictions <- predict(model, newdata = test)
predictions <- predictions$data %>% 
  as_tibble() %>% 
  mutate(listing_id = test$listing_id) %>% 
  select(listing_id, low = prob.0, medium = prob.1, high = prob.2)

# 6. Write data.
write_csv(predictions, "./Two Sigma Connect/Output/glmnet_01.csv")
