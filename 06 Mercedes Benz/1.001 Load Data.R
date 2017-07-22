#' ---
#' title: "Load Data"
#' author: "Kevin Lu"
#' output: 
#'   html_document: 
#'     toc: true 
#'     toc_float: true
#'     number_sections: true
#' ---

#' # Load data wrangling libraries.
library(tidyverse)
library(lubridate)
library(stringr)
library(tidytext)

#' # Load machine learning libraries. 
library(xgboost)
library(mlr)

#' # Load training data. 
train <- read_csv("./Mercedes Benz/Raw Data/train.csv", guess_max = 10000)

#' # Load test data. 
test <- read_csv("./Mercedes Benz/Raw Data/test.csv", guess_max = 10000)

