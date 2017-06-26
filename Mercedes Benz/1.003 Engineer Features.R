#' ---
#' title: "Engineer Features"
#' author: "Kevin Lu"
#' output: 
#'   html_document: 
#'     toc: true 
#'     toc_float: true
#'     number_sections: true
#' ---

#' # Combine data.
combined <- bind_rows(train %>% mutate(source = "train"), 
                      test %>% mutate(source = "test"))

#' # Add highly correlated principal components. 
pca <- prcomp(combined %>% select(X10:X385))
combined <- combined %>%
  bind_cols(predict(pca, combined) %>%
              as_tibble() %>%
              select(PC3, PC5, PC10))

#' # Factor encode categorical features. 
combined <- combined %>%
  mutate(X0_factor = factor(X0), 
         X1_factor = factor(X1), 
         X2_factor = factor(X2), 
         X3_factor = factor(X3), 
         X4_factor = factor(X4), 
         X5_factor = factor(X5), 
         X6_factor = factor(X6), 
         X8_factor = factor(X8))

#' # Label encode categorical features.
combined <- combined %>%
  mutate(X0_label = as.numeric(factor(X0)),
         X1_label = as.numeric(factor(X1)),
         X2_label = as.numeric(factor(X2)),
         X3_label = as.numeric(factor(X3)),
         X4_label = as.numeric(factor(X4)),
         X5_label = as.numeric(factor(X5)),
         X6_label = as.numeric(factor(X6)),
         X8_label = as.numeric(factor(X8))) %>% 
  select(-X0, -X1, -X2, -X3, -X4, -X5, -X6, -X8)

#' # Lexical encoding, SVD, distance to cluster centers, RBF

#' # Split back into train and test sets.
train <- combined %>% filter(source == "train") %>% select(-source)
test <- combined %>% filter(source == "test") %>% select(-source)

