#' ---
#' title: "Plot Data"
#' author: "Kevin Lu"
#' output: 
#'   html_document: 
#'     toc: true 
#'     toc_float: true
#'     number_sections: true
#' ---

#' # 1. Combine data
combined <- bind_rows(train %>% mutate(source = "train"), 
                      test %>% mutate(source = "test"))

