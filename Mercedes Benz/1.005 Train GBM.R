#' ---
#' title: "Train GBM"
#' author: "Kevin Lu"
#' output: 
#'   html_document: 
#'     toc: true 
#'     toc_float: true
#'     number_sections: true
#' ---

#' # Set features to use.
features <- setdiff(colnames(train), c(""))
print(features)
train <- train[, features]
test <- test[, features]

#' # Set up mlr objects. 
task <- makeRegrTask(id = "main",
                     data = train,
                     target = "y")
learner <- makeLearner("regr.gbm", 
                       distribution = "gaussian",
                       n.trees = 600, 
                       interaction.depth = 2, 
                       n.minobsinnode = 15, 
                       shrinkage = 0.01,
                       bag.fraction = 0.95, 
                       train.fraction = 1.0)
resampling <- makeResampleDesc(method = "CV", 
                               iters = 5)

#' # Run cross validation. 
set.seed(5)
resample <- resample(learner = learner,
                     task = task,
                     resampling = resampling,
                     measures = rsq,
                     keep.pred = FALSE)
# [Resample] cross-validation iter 1: rsq.test.mean=0.57
# [Resample] cross-validation iter 2: rsq.test.mean=0.607
# [Resample] cross-validation iter 3: rsq.test.mean=0.596
# [Resample] cross-validation iter 4: rsq.test.mean=0.483
# [Resample] cross-validation iter 5: rsq.test.mean=0.604
# [Resample] Aggr. Result: rsq.test.mean=0.572

#' # Train model. 
model <- train(learner, task)

#' # Generate predictions. 
predictions <- predict(model, newdata = test) %>% 
  as_tibble() %>% 
  mutate(ID = test$ID) %>% 
  select(ID, y = response)
write_csv(predictions, "./Mercedes Benz/Output/Base Models/gbm_01.csv")
