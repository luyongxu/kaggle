source("./Two Sigma Connect/1.001 Load Data.R")
source("./Two Sigma Connect/1.003 Engineer Features.R")

# 1. Set features to use.
features_to_use <- c("bathrooms", "bedrooms", "latitude", "longitude", "price", "listing_id", 
                     "distance_center", "display_address_count", "manager_id_count", "building_id_count", 
                     "street_address_count", "display_address", "manager_id", "street_address", 
                     "photos_count", "features_count", "description_count", "building_id_zero", 
                     "manager_id_low", "manager_id_medium", "manager_id_high", "photos_zero", 
                     "bathrooms_standard", "created_hour", "price_rank", "price_rank2", 
                     "street_display_sim", colnames(features_dtm))

# 2. Split up into train and test sets. 
train <- train[, c(features_to_use, "interest_level")] %>% mutate(interest_level = factor(interest_level))
test <- test[, c(features_to_use, "interest_level")]

# 3. Set up mlr objects. 
task <- makeClassifTask(id = "main",
                        data = train,
                        target = "interest_level")
learner <- makeLearner("classif.gbm",
                       distribution = "multinomial",
                       n.trees = 100,
                       interaction.depth = 20,
                       n.minobsinnode = 10,
                       shrinkage = 0.1,
                       bag.fraction = 0.5,
                       predict.type = "prob")
resampling <- makeResampleDesc(method = "CV",
                               stratify = TRUE,
                               iters = 5)
set.seed(55555)

# 4. Run cross validation. 
resample <- resample(learner = learner,
                     task = task,
                     resampling = resampling,
                     measures = logloss,
                     keep.pred = FALSE)
# [Resample] Aggr. Result: logloss.test.mean=0.545

# 4. Train model.
model <- train(learner, task)

# 5. Generate predictions.
predictions <- predict(model, newdata = test)
predictions <- predictions$data %>%
  as_tibble() %>%
  mutate(listing_id = test$listing_id) %>%
  select(listing_id, low = prob.0, medium = prob.1, high = prob.2)
write_csv(predictions, "./Two Sigma Connect/Output/Base Models/gbm_01.csv")

# 7. Generate metafeatures for stacking.
train <- train %>%
  mutate(fold = sample(cut(seq(1, nrow(train)), breaks = 5, labels = FALSE)))
predictions_df <- data.frame()
for (i in 1:5) {
  print(str_c("Generating metafeatures for fold ", i, "."))
  meta_train <- train %>% filter(fold != i)
  meta_test <- train %>% filter(fold == i)
  task <- makeClassifTask(id = "main",
                          data = meta_train,
                          target = "interest_level")
  model <- train(learner, task)
  predictions <- predict(model, newdata = meta_test)
  predictions <- predictions$data %>%
    as_tibble() %>%
    mutate(listing_id = meta_test$listing_id) %>%
    select(listing_id, low = prob.0, medium = prob.1, high = prob.2)
  predictions_df <- bind_rows(predictions_df, predictions)
}
write_csv(predictions_df, "./Two Sigma Connect/Output/Meta Features/gbm_01_meta.csv")
