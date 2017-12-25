#' # Add first 10 principal components. 
#' Did not improve CV score.
# pca <- prcomp(train %>% select(-y, -ID))
# train <- train %>%
#   bind_cols(predict(pca, train) %>%
#               as_tibble() %>%
#               select(PC1:PC10))
# test <- test %>%
#   bind_cols(predict(pca, test) %>%
#               as_tibble() %>%
#               select(PC1:PC10))

#' # Find most correlated PCA components. 
# pca_corr <- tibble()
# for (i in 1:365) {
#   pca_corr <- pca_corr %>% 
#     bind_rows(tibble(component = i, 
#                      correlation = cor(combined$y, combined[[str_c("PC", i)]], use = "complete.obs")))
# }
# 3, 1, 5, 10, 6, 9, 7

#' # Sum of binary variables. 
#' Did not improve CV score. 
# combined <- combined %>%
#   bind_cols(combined %>%
#               select(X10:X385) %>%
#               mutate(binary_sum = rowSums(.)) %>%
#               select(binary_sum))

#' # One hot encode categorical variables. 
#' Did not improve CV score.
# ohe <- combined %>%
#   mutate_if(is.character, factor) %>%
#   mutate(source = as.character(source)) %>%
#   createDummyFeatures(target = "y", method = "1-of-n") %>%
#   select(X0.a:X8.y)
# combined <- combined %>%
#   bind_cols(ohe)

#' # Frequency encode categorical features. 
#' Did not improve CV score. 
# combined <- combined %>%
#   left_join(combined %>% group_by(X0) %>% summarise(X0_freq = n())) %>%
#   left_join(combined %>% group_by(X1) %>% summarise(X1_freq = n())) %>%
#   left_join(combined %>% group_by(X2) %>% summarise(X2_freq = n())) %>%
#   left_join(combined %>% group_by(X3) %>% summarise(X3_freq = n())) %>%
#   left_join(combined %>% group_by(X4) %>% summarise(X4_freq = n())) %>%
#   left_join(combined %>% group_by(X5) %>% summarise(X5_freq = n())) %>%
#   left_join(combined %>% group_by(X6) %>% summarise(X6_freq = n())) %>%
#   left_join(combined %>% group_by(X8) %>% summarise(X8_freq = n()))

#' # Engineer features. 
# combined <- combined %>% 
#   mutate(Z1 = factor(str_c(X314, X29))) %>% 
#   createDummyFeatures(target = "y", method = "1-of-n")

# 6. Mean encode high cardinality categorical variables. 
# mean_encode <- function(df, categorical_var) { 
#   set.seed(55555)
#   df <- df %>% 
#     mutate(low = ifelse(interest_level == 0, 1, 0), 
#            medium = ifelse(interest_level == 1, 1, 0), 
#            high = ifelse(interest_level == 2, 1, 0), 
#            fold = c(sample(cut(seq(1, nrow(train)), breaks = 5, labels = FALSE)), 
#                     rep(NA, nrow(test))))
#   pred <- data.frame()
#   for (i in 1:5) { 
#     df_train <- df %>% filter(fold != i)
#     df_test <- df %>% filter(fold == i)
#     model_low <- lmer(paste("low ~ (1 | ", categorical_var, ")"), data = df_train)
#     model_medium <- lmer(paste("medium ~ (1 | ", categorical_var, ")"), data = df_train)
#     model_high <- lmer(paste("high ~ (1 | ", categorical_var, ")"), data = df_train)
#     df_test <- df_test %>% 
#       mutate(categorical_var_low = predict(model_low, df_test, allow.new.levels = TRUE), 
#              categorical_var_medium = predict(model_medium, df_test, allow.new.levels = TRUE), 
#              categorical_var_high = predict(model_high, df_test, allow.new.levels = TRUE)) %>% 
#       select(listing_id, categorical_var_low, categorical_var_medium, categorical_var_high)
#     pred <- bind_rows(pred, df_test)
#   }
#   df_train <- df %>% filter(is.numeric(fold))
#   df_test <- df %>% filter(is.na(fold))
#   model_low <- lmer(paste("low ~ (1 | ", categorical_var, ")"), data = df_train)
#   model_medium <- lmer(paste("medium ~ (1 | ", categorical_var, ")"), data = df_train)
#   model_high <- lmer(paste("high ~ (1 | ", categorical_var, ")"), data = df_train)
#   df_test <- df_test %>% 
#     mutate(categorical_var_low = predict(model_low, df_test, allow.new.levels = TRUE), 
#            categorical_var_medium = predict(model_medium, df_test, allow.new.levels = TRUE), 
#            categorical_var_high = predict(model_high, df_test, allow.new.levels = TRUE)) %>% 
#     select(listing_id, categorical_var_low, categorical_var_medium, categorical_var_high)
#   pred <- bind_rows(pred, df_test)
#   return(pred)
# }

#' # Set folds. 
# set.seed(5)
# train <- train %>% 
#   mutate(fold = sample(cut(seq(1, nrow(train)), breaks = 5, labels = FALSE)))

#' # Mean target encode X0.
#' Did not improve CV score. 
# set.seed(5)
# X0_mean <- train %>% 
#   group_by(X0) %>% 
#   summarise(X0_total = sum(y), 
#             X0_count = n(), 
#             X0_mean = mean(y))
# combined <- combined %>% 
#   left_join(X0_mean) %>% 
#   mutate(X0_noise = runif(nrow(combined), 0.99, 1.01), 
#          X0_mean = ifelse(source == "train", ((X0_total - y) / (X0_count - 1)) * X0_noise, X0_mean), 
#          X0_mean = ifelse(is.na(X0_mean), mean(train$y), X0_mean)) %>% 
#   select(-X0_total, -X0_count)
#' # mean encode X0 for train. 
# X0_results <- tibble()
# for (i in 1:5) { 
#   X0_mean <- train %>% 
#     filter(fold != i) %>% 
#     group_by(X0) %>% 
#     summarise(X0_mean = mean(y))
#   train_valid <- train %>% 
#     filter(fold == i) %>% 
#     left_join(X0_mean) %>% 
#     select(ID, X0_mean)
#   X0_results <- bind_rows(X0_results, train_valid)
# }
# train <- train %>% 
#   left_join(X0_results) %>% 
#   mutate(X0_mean = ifelse(is.na(X0_mean), mean(train$y), X0_mean))

#' # mean encode X0 for test.
# X0_mean <- train %>% 
#   group_by(X0) %>% 
#   summarise(X0_mean = mean(y))
# test <- test %>% 
#   left_join(X0_mean) %>% 
#   mutate(X0_mean = ifelse(is.na(X0_mean), mean(train$y), X0_mean))
# rm(train_valid, X0_mean, X0_results, i)

#' # Engineered features. 
# combined <- combined %>% 
#   mutate(Z1 = as.numeric(factor(str_c(X127, X118, X238))), 
#          Z2 = as.numeric(factor(str_c(X314, X29))), 
#          Z3 = as.numeric(factor(str_c(X314, X315))), 
#          Z4 = as.numeric(factor(str_c(X314, X118))), 
#          Z5 = as.numeric(factor(str_c(X314, X127))), 
#          Z6 = as.numeric(factor(str_c(X29, X315))), 
#          Z7 = as.numeric(factor(str_c(X29, X118))), 
#          Z8 = as.numeric(factor(str_c(X29, X127))))

# 1:       Z1 0.7543102830 2.483265e-01 0.175324675
# 2:      X29 0.0851771777 5.928848e-02 0.071428571
# 3:     X118 0.0817716518 6.496982e-02 0.090909091
# 4:       ID 0.0187779011 1.743850e-01 0.175324675
# 5:     X189 0.0173086862 3.441342e-02 0.045454545
# 6: X0_label 0.0087711552 9.954410e-02 0.084415584
# 7:     X115 0.0047329971 1.324184e-02 0.019480519
# 8:     X383 0.0038185355 5.576049e-02 0.038961039
# 9:      X48 0.0033263628 3.587580e-02 0.032467532
# 10:      X71 0.0024588975 1.123472e-02 0.019480519
# 11:     X275 0.0024341406 2.006018e-02 0.025974026
# 12: X3_label 0.0012968469 4.643052e-04 0.012987013
# 13:       Z2 0.0012184103 9.154489e-03 0.006493506
# 14:      X47 0.0010545111 1.310291e-02 0.012987013
# 15:      X12 0.0009368236 1.886834e-02 0.012987013
# 16:      X61 0.0009191822 4.368856e-03 0.006493506
# 17:      X98 0.0008690543 4.292081e-03 0.006493506
# 18:     X306 0.0008529603 9.147177e-03 0.006493506
# 19:     X170 0.0006807227 5.026926e-03 0.006493506
# 20:     X315 0.0006741130 1.142118e-02 0.012987013
# 21: X2_label 0.0006677500 2.412925e-04 0.006493506
# 22:     X276 0.0006444455 4.975743e-03 0.006493506
# 23:     X322 0.0006266439 2.650561e-03 0.006493506
# 24:      X70 0.0005676670 7.933403e-04 0.006493506
# 25:      X58 0.0005650883 3.655946e-05 0.006493506
# 26:     X196 0.0005584960 9.169113e-03 0.006493506
# 27:     X152 0.0004922104 9.351910e-03 0.006493506
# 28:     X316 0.0004865166 2.727336e-03 0.006493506
# 29:     X261 0.0004465015 9.249544e-03 0.006493506
# 30:     X327 0.0004378049 3.480461e-03 0.006493506
# 31:      X21 0.0004191879 7.132751e-03 0.006493506
# 32:     X132 0.0003961120 9.289759e-03 0.006493506
# 33:     X321 0.0003840324 9.242232e-03 0.006493506
# 34:      X20 0.0003227620 9.063091e-03 0.006493506
# 35:     X359 0.0003112291 9.329975e-03 0.006493506
# 36:     X201 0.0002640191 9.176425e-03 0.006493506
# 37:     X177 0.0002630326 1.601304e-03 0.006493506
# 38:     X148 0.0002623893 6.492960e-03 0.006493506
# 39:      X31 0.0002008730 2.522603e-03 0.006493506
# 40:     X283 0.0001756834 3.253792e-04 0.006493506
# 41:      X55 0.0001171424 2.010770e-04 0.006493506
# Feature         Gain        Cover   Frequency