# 1. Load libraries. 
library(readr)
library(lubridate)
library(randomForest)

# 2. Load data. 
train <- read_csv("./Bike Sharing/Raw Data/train.csv") 
test <- read_csv("./Bike Sharing/Raw Data/test.csv") 

# 3. Feature engineering. 
combined <- bind_rows(train %>% mutate(source = "train"), 
                      test %>% mutate(source = "test")) %>% 
  mutate(source = factor(source), 
         season = factor(season), 
         weather = factor(weather), 
         hour = as.numeric(hour(datetime)), 
         day = wday(datetime, label = TRUE))

# 4. Plots
ggplot(combined, aes(x = datetime, y = count)) + 
  geom_point(alpha = 0.5) + 
  geom_smooth()
ggplot(combined, aes(x = hour, y = count, colour = temp)) + 
  geom_jitter(alpha = 0.5) + 
  scale_colour_gradient(low = "green", high = "red") + 
  geom_smooth() + 
  facet_grid(workingday ~ .)
ggplot(combined, aes(x = hour, y = count, colour = season)) + 
  geom_smooth(se = FALSE)
ggplot(combined, aes(x = hour, y = count, colour = weather)) + 
  geom_smooth(se = FALSE)
ggplot(combined, aes(x = hour, y = count, colour = day)) + 
  geom_smooth(se = FALSE)

# 5. Random forest benchmark. 
dates <- as.character(seq(as.Date("2011-02-01"), length = 24, by = "1 month") - 1)
submission <- data.frame()
set.seed(1)
for (date in dates) { 
  date <- as.Date(date)
  print(paste0("Training model up to ", date, "."))
  train_df <- combined %>% 
    filter(datetime <= date, 
           source == "train")
  test_df <- combined %>% 
    filter(year(datetime) == year(date) & month(datetime) == month(date), 
           source == "test")
  rf <- randomForest(count ~ 
                       season + 
                       holiday + 
                       workingday + 
                       weather + 
                       temp + 
                       atemp + 
                       humidity + 
                       windspeed + 
                       hour, 
                     data = train_df, 
                     ntree = 100)
  submission_df <- test_df %>% 
    mutate(count = predict(rf, test_df)) %>% 
    select(datetime, count)
  submission <- bind_rows(submission, submission_df)
}

# 6. Check plots.
predictions <- bind_rows(train %>% mutate(source = "train"), submission %>% mutate(source = "test")) 
ggplot(predictions, aes(x = datetime, y = count, colour = source)) + 
  geom_point(alpha = 0.5) + 
  geom_smooth()

# 6. Write submission file. Scores 0.59522 on public leaderboard.
write.csv(submission, "./Bike Sharing/Output/1.001 Random Forest Benchmark.csv", row.names = FALSE)
