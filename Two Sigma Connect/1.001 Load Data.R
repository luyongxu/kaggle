# 1. Load libraries.
library(jsonlite)
library(purrr)
library(ggplot2)
library(dplyr)
library(ggmap)
library(h2o)
library(lubridate)
library(readr)

# 2. Load training data. 
train <- fromJSON("./Two Sigma Connect/Raw Data/train.json")
vars <- setdiff(names(train), c("photos", "features"))
train <- map_at(train, vars, unlist) %>% tibble::as_tibble()

# 3. Load test data. 
test <- fromJSON("./Two Sigma Connect/Raw Data/test.json")
vars <- setdiff(names(test), c("photos", "features"))
test <- map_at(test, vars, unlist) %>% tibble::as_tibble()

