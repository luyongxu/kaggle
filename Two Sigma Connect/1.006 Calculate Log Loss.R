# 1. Log loss function. 
log_loss <- function(actual, predicted) {
  eps <- 1e-15 
  predicted[predicted < eps] <- eps
  predicted[predicted > 1 - eps] <- 1 - eps
  return((-1 / nrow(actual)) * sum(actual * log(predicted)))
}

# 2. Generate predictions. 
predictions <- predict(m01, as.matrix(val_X)) %>% 
  matrix(nrow = 3, ncol = nrow(val_X)) %>% 
  t() %>% 
  data.frame() %>% 
  mutate(listing_id = val_X$listing_id) %>% 
  select(listing_id, X1, X2, X3)
colnames(predictions) <- c("listing_id", "low", "medium", "high")
predictions <- bind_cols(predictions, data.frame(val_y)) %>% 
  mutate(low_actual = ifelse(val_y == 0, 1, 0), 
         medium_actual = ifelse(val_y == 1, 1, 0), 
         high_actual = ifelse(val_y == 2, 1, 0))

# 3. Calculate log loss. 
log_loss(actual = cbind(predictions$low_actual, 
                        predictions$medium_actual, 
                        predictions$high_actual), 
         predicted = cbind(predictions$low, 
                           predictions$medium, 
                           predictions$high))
