#' ---
#' title: "Load Data"
#' author: "Kevin Lu"
#' output: 
#'   html_document: 
#'     toc: true 
#'     toc_float: true
#'     number_sections: true
#' ---

#' # 1. Load data wrangling libraries.
library(tidyverse)
library(lubridate)
library(stringr)
library(tidytext)

#' # 2. Load machine learning libraries. 
library(xgboost)
library(lightgbm)
library(mlr)

#' # 3. Load training data. 
train <- read_csv("./09 Web Traffic/Raw Data/train_1.csv")

#' # 4. Load key data. 
key <- read_csv("./09 Web Traffic/Raw Data/key_1.csv")

