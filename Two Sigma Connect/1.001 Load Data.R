# 1. Load libraries.
library(tidyverse)
library(lubridate)
library(jsonlite)
library(xgboost)
library(stringr)
# library(broom)
# library(h2o)
# library(Matrix)
# library(syuzhet)
# library(lme4)

# 2. Load training data. 
train <- fromJSON("./Two Sigma Connect/Raw Data/train.json")
vars <- setdiff(names(train), c("photos", "features"))
train <- map_at(train, vars, unlist) %>% tibble::as_tibble()
str(train[, vars])

# 3. Load test data. 
test <- fromJSON("./Two Sigma Connect/Raw Data/test.json")
vars <- setdiff(names(test), c("photos", "features"))
test <- map_at(test, vars, unlist) %>% tibble::as_tibble()
str(test[, vars])

# 4. Clean workspace.
rm(vars)
