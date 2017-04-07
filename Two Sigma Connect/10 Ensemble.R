xgb_23 <- read_csv("./Two Sigma Connect/Output/xgb_23.csv") %>% 
  rename(low_a = low, 
         medium_a = medium, 
         high_a = high)
lgb_02 <- read_csv("./Two Sigma Connect/Output/lgb_02.csv") %>% 
  rename(low_b = low, 
         medium_b = medium, 
         high_b = high)
xgb_lit <- read_csv("./Two Sigma Connect/Output/it is lit.csv") %>% 
  rename(low_c = low, 
         medium_c = medium, 
         high_c = high)
rf_01 <- read_csv("./Two Sigma Connect/Output/rf_01.csv") %>% 
  rename(low_d = low, 
         medium_d = medium, 
         high_d = high)
en <- xgb_23 %>% 
  left_join(lgb_02) %>% 
  left_join(xgb_lit) %>% 
  left_join(rf_01) %>% 
  mutate(low = 
           0.4 * low_a + 
           0.4 * low_b + 
           0.15 * low_c + 
           0.05 * low_d, 
         medium = 
           0.4 * medium_a + 
           0.4 * medium_b + 
           0.15 * medium_c + 
           0.05 * medium_d, 
         high = 
           0.4 * high_a + 
           0.4 * high_b + 
           0.15 * high_c + 
           0.05 * high_d) %>% 
  select(listing_id, low, medium, high)
write_csv(en, "./Two Sigma Connect/Output/ensemble_08.csv")

# classif.cforest 
# classif.ctree 
# classif.cvglmnet 
# classif.extraTrees 
# classif.glmnet 