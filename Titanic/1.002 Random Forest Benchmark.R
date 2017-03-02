# Code adapted from https://www.kaggle.com/benhamner/titanic/random-forest-benchmark-r/code. 
# Submission scores 0.77033 on public leaderboard.

# 1. Load libraries. 
library(ggplot2)
library(randomForest)

# 2. Load data. 
train <- read_csv("./Titanic/Raw Data/train.csv")
test <- read_csv("./Titanic/Raw Data/test.csv")

# 3. Extract features function. 
extractFeatures <- function(data) {
  features <- c("Pclass",
                "Age",
                "Sex",
                "Parch",
                "SibSp",
                "Fare",
                "Embarked")
  fea <- data[ , features]
  fea$Age[is.na(fea$Age)] <- -1
  fea$Fare[is.na(fea$Fare)] <- median(fea$Fare, na.rm = TRUE)
  fea$Embarked[is.na(fea$Embarked)] <- "S"
  fea$Sex <- as.factor(fea$Sex)
  fea$Embarked <- as.factor(fea$Embarked)
  return(fea)
}

# 4. Train random forest model. 
rf <- randomForest(extractFeatures(train), as.factor(train$Survived), ntree = 100, importance = TRUE)
submission <- data.frame(PassengerId = test$PassengerId)
submission$Survived <- predict(rf, extractFeatures(test))
write_csv(submission, "./Titanic/Output/1.002 Predictions.csv")

# 5. Variable importance.
imp <- importance(rf, type = 1)
featureImportance <- data.frame(Feature = row.names(imp), Importance = imp[ , 1])
ggplot(featureImportance, aes(x = reorder(Feature, Importance), y = Importance)) + 
  geom_bar(stat = "identity") + 
  coord_flip()
