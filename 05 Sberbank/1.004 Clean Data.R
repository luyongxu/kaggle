#' ---
#' title: "Plot Data"
#' author: "Kevin Lu"
#' output: 
#'   html_document: 
#'     toc: true 
#'     toc_float: true
#'     number_sections: true
#' ---

#' # Fix data quality issues
combined <- combined %>% 
  mutate(state = ifelse(state == 33, NA, state), 
         build_year = ifelse(build_year == 1691, 1961, build_year), 
         build_year = ifelse(build_year == 215, 2015, build_year), 
         build_year = ifelse(build_year == 4965, 1965, build_year), 
         build_year = ifelse(build_year == 2, 2014, build_year), 
         build_year = ifelse(build_year == 3, 2013, build_year), 
         build_year = ifelse(build_year == 20, 2014, build_year), 
         build_year = ifelse(build_year == 20052009, 2009, build_year), 
         build_year = ifelse(build_year %in% c(0, 1, 71), NA, build_year), 
         max_floor = ifelse(max_floor > 57, NA, max_floor), 
         max_floor = ifelse(max_floor == 0, NA, max_floor))

#' # Convert character features to numeric
combined <- combined %>%
  map_if(is.character, function(x) as.numeric(factor(x))) %>%
  as_tibble()

# full_sq-life_sq<0 full_sq-kitch_sq<0 life_sq-kitch_sq<0 floor-max_floor<0

#' # Split back into train and test sets
train <- combined %>% 
  filter(source == 2) %>% 
  select(-source)
test <- combined %>% 
  filter(source == 1) %>% 
  select(-source)
