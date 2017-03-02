# 1. 
library(jsonlite)
library(purrr)

# 2. 
train <- fromJSON("./Two Sigma Connect/Raw Data/train.json")
vars <- setdiff(names(train), c("photos", "features"))
train <- map_at(train, vars, unlist) %>% tibble::as_tibble(.)


