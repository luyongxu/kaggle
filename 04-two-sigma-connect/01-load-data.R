# 1.1 Load data wrangling libraries.
library(tidyverse)
library(lubridate)
library(jsonlite)
library(stringr)
library(tidytext)

# 1.2 Load Machine learning libraries. 
library(lme4)
library(xgboost)
library(lightgbm)
library(mlr)

# 2.1 Load training data. 
train <- fromJSON("./04-two-sigma-connect/data/train.json")
vars <- setdiff(names(train), c("photos", "features"))
train <- map_at(train, vars, unlist) %>% tibble::as_tibble()
str(train[, vars])

# 2.2 Load test data. 
test <- fromJSON("./04-two-sigma-connect/data/test.json")
vars <- setdiff(names(test), c("photos", "features"))
test <- map_at(test, vars, unlist) %>% tibble::as_tibble()
str(test[, vars])

# 3. Clean workspace.
rm(vars)

