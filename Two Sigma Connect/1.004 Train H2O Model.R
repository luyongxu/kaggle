source("./Two Sigma Connect/1.001 Load Data.R")
source("./Two Sigma Connect/1.002 Engineer Features.R")

# 1. Initialize h2o instance. Use all cores.
h2o.init(nthreads = -1, max_mem_size = "10G")

# 2. Import R object to H2O instance. 
train_h2o <- as.h2o(train)

# 3. Train model.
m01 <-h2o.gbm(x = setdiff(colnames(train), "interest_level"), 
              y = "interest_level", 
              training_frame = train_h2o, 
              distribution = "multinomial", 
              model_id = "m01", 
              ntrees = 1500, 
              learn_rate = 0.01, 
              max_depth = 7, 
              min_rows = 20, 
              sample_rate = 0.8, 
              col_sample_rate = 0.7, 
              stopping_rounds = 5, 
              stopping_metric = "logloss", 
              stopping_tolerance = 0, 
              seed = 321)

# 4. Import test to H2O instance. 
test_h2o <- as.h2o(test)

# 5. Make predictions. 
predictions <- as.data.frame(h2o.predict(m01, test_h2o))
predictions <- predictions %>% 
  mutate(listing_id = test$listing_id) %>% 
  select(listing_id, low, medium, high)

# 6. Write csv. Scores 0.55474. 
write_csv(predictions, "./Two Sigma Connect/Output/1.004 Train H20 Model.csv")
