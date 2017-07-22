# 1. Load libraries. 
library(readr)
library(lubridate)
library(xgboost)

# 2. Load data. 
train <- read_csv("./Bike Sharing/Raw Data/train.csv") 
test <- read_csv("./Bike Sharing/Raw Data/test.csv") 

# 3. Feature engineering. 
train <- train %>% 
  mutate(hour = hour(datetime), 
         month = month(datetime), 
         year = year(datetime), 
         wday = wday(datetime), 
         count = log1p(count), 
         count_lag1 = lag(count))
test <- test  %>% 
  mutate(hour = hour(datetime), 
         month = month(datetime), 
         year = year(datetime), 
         wday = wday(datetime))

# 4. Train xgboost model. 
X_train <- train %>% 
  select(season, holiday, workingday, weather, temp, atemp, humidity, windspeed, hour, month, year, wday) %>% 
  as.matrix()
y_train <- train %>% 
  select(count) %>% 
  as.matrix()
dtrain <- xgb.DMatrix(X_train, label = y_train)
model <- xgb.train(data = dtrain, 
                   nround = 150, 
                   max_depth = 5, 
                   eta = 0.1, 
                   subsample = 0.9)

# 5.Generate predictions. Scores 0.40065 on public leaderboard. 
X_test <- test %>% 
  select(-datetime) %>% 
  as.matrix()
predictions <- predict(model, X_test) %>% 
  expm1()
submission <- data.frame(datetime = test$datetime, count = predictions)
write.csv(submission, "./Bike Sharing/Output/1.002 XGBoost Benchmark.csv", row.names = FALSE)

# 6. Check plots.
plot <- bind_rows(train %>% mutate(source = "train", count = expm1(count)), submission %>% mutate(source = "test")) 
ggplot(plot, aes(x = datetime, y = count, colour = source)) + 
  geom_point(alpha = 0.5) + 
  geom_smooth()
