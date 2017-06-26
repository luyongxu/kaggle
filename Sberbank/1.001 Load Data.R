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

#' # Load training data. 
train <- read_csv("./Sberbank/Raw Data/train.csv", guess_max = 10000)

#' # Load macro data. 
macro <- read_csv("./Sberbank/Raw Data/macro.csv", guess_max = 10000)

#' # Load test data. 
test <- read_csv("./Sberbank/Raw Data/test.csv", guess_max = 10000)


