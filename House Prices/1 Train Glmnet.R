library(tidyverse)
library(purrr)
library(glmnet)

# 1. Load data. 
train <- read_csv("./House Prices/Raw Data/train.csv")
test <- read_csv("./House Prices/Raw Data/test.csv")
combined <- bind_rows(train, test)

# 2. Log transform numerical features. 
combined_numeric <- combined[, combined %>% map_lgl(is.numeric)]
high_skew <- combined_numeric %>% map_dbl(moments::skewness, na.rm = TRUE) >= 0.75
combined_numeric[, high_skew == TRUE] <- log1p(combined_numeric[, high_skew == TRUE])
combined_numeric <- map_df(combined_numeric, function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x))

# 3. Convert categorical features to factor. 
combined_categorical <- combined[, combined %>% map_lgl(is.character)]
combined_categorical <- map_df(combined_categorical, factor)

# 4. One hot encode. 
dummies <- dummyVars(~ ., combined_categorical)
combined_categorical <- data.frame(predict(dummies, combined_categorical))
combined_categorical[is.na(combined_categorical)] <- 0

# 5. Recombine.
combined <- bind_cols(combined_categorical, combined_numeric)

# 6. Split back into train and test sets. 
train <- combined[1:nrow(train), ]
test <- combined[(nrow(train)+1):nrow(combined), ]
train_X <- train[, setdiff(colnames(train), "SalePrice")]
train_y <- train[ , "SalePrice"]
test_X <- test[ , setdiff(colnames(test), "SalePrice")]

# 7. Train ridge.  
set.seed(123)
model_ridge <- train(x = train_X, 
                     y = train_y, 
                     method = "glmnet", 
                     metric = "RMSE", 
                     trControl = trainControl(method = "repeatedcv", 
                                              number = 5, 
                                              repeats = 5), 
                     tuneGrid = expand.grid(alpha = 0, 
                                            lambda = seq(0, 1, 0.001)))
ggplot(model_ridge, aes(x = lambda, y = RMSE)) + geom_line()
mean(model_ridge$resample$RMSE)

set.seed(123)
model_ridge2 <- cv.glmnet(as.matrix(train_X), as.matrix(train_y), lambda = seq(0, 1, 0.001), alpha = 0, nfolds = 5)
plot(model_ridge2)
min(model_ridge2$cvm)^0.5
model_ridge2$lambda.min

# 8. Train lasso. 
set.seed(123)
model_lasso <- train(x = train_X, 
                     y = train_y, 
                     method = "glmnet", 
                     metric = "RMSE", 
                     trControl = trainControl(method = "repeatedcv", 
                                              number = 5, 
                                              repeats = 5), 
                     tuneGrid = expand.grid(alpha = 1, 
                                            lambda = c(1, 0.1, 0.05, 0.01, seq(0.009, 0.001, -0.001), 
                                                       0.00075, 0.0005, 0.0001)))
plot(model_lasso)
mean(model_lasso$resample$RMSE)

set.seed(123)
model_lasso2 <- cv.glmnet(as.matrix(train_X), 
                          as.matrix(train_y), 
                          lambda = c(1, 0.1, 0.05, 0.01, seq(0.009, 0.001, -0.001), 0.00075, 0.0005, 0.0001),  
                          alpha = 1, 
                          nfolds = 5)
plot(model_lasso2)
min(model_lasso2$cvm)^0.5
model_lasso2$lambda.min


# 9. Generate predictions.  
pred <- data.frame(Id = test$Id, SalePrice = as.numeric(predict(model_lasso, test_X))) %>% 
  mutate(SalePrice =  expm1(SalePrice))
write_csv(pred, "./House Prices/Output/pred_03.csv")
