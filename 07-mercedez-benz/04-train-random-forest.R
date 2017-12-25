#' ---
#' title: "Train Random Forest"
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
learner <- makeLearner("regr.randomForest", 
                       ntree = 700, 
                       mtry = 20, 
                       replace = TRUE, 
                       sampsize = ceiling(0.632 * nrow(train)), 
                       nodesize = 30, 
                       importance = FALSE)
resampling <- makeResampleDesc(method = "CV",
                               iters = 5)

#' # Run cross validation. 
set.seed(5)
resample <- resample(learner = learner,
                     task = task,
                     resampling = resampling,
                     measures = rsq,
                     keep.pred = FALSE)
# [Resample] cross-validation iter 1: rsq.test.mean=0.559
# [Resample] cross-validation iter 2: rsq.test.mean=0.586
# [Resample] cross-validation iter 3: rsq.test.mean=0.579
# [Resample] cross-validation iter 4: rsq.test.mean=0.479
# [Resample] cross-validation iter 5: rsq.test.mean=0.589
# [Resample] Aggr. Result: rsq.test.mean=0.558

#' # Train model. 
model <- train(learner, task)

#' # Generate predictions. 
predictions <- predict(model, newdata = test) %>% 
  as_tibble() %>% 
  mutate(ID = test$ID) %>% 
  select(ID, y = response)
write_csv(predictions, "./07-mercedes-benz/output/randomforest-02.csv")

