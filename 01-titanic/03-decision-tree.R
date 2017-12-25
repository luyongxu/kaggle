# Code adapted from https://www.kaggle.com/yildirimarda/titanic/decision-tree-visualization-submission/code.
# Submission scores 0.74163 on public leaderboard.

# 1. Load libraries. 
library(rpart)
library(rpart.plot)

# 2. Load data. 
train <- read_csv("./01-titanic/data/train.csv")
test <- read_csv("./01-titanic/data/test.csv")
test$Survived <- 0

# 3. Cleaning data. 
full <- bind_rows(train, test) %>% 
  mutate(Title = sapply(Name, function(x) strsplit(x, split = "[,.]")[[1]][2]), 
         Title = sub(" ", "", Title), 
         Title = ifelse(PassengerId == 797, "Mrs", Title), 
         Title = ifelse(Title %in% c("Lady", "the Countess", "Mlle", "Mee", "Ms"), "Miss", Title), 
         Title = ifelse(Title %in% c("Capt", "Don", "Major", "Sir", "Col", "Jonkheer", "Rev", "Dr", "Master"), "Mr", Title), 
         Title = ifelse(Title %in% c("Dona"), "Mrs", Title), 
         Title = factor(Title), 
         Embarked = ifelse(is.na(Embarked), "S", Embarked), 
         Embarked = factor(Embarked), 
         Fare = ifelse(is.na(Fare), median(Fare, na.rm = TRUE), Fare), 
         family_size = SibSp + Parch + 1)

# 4. Fill in missing Age values. 
predicted_age <- rpart(Age ~ 
                         Pclass + 
                         Sex + 
                         SibSp + 
                         Parch + 
                         Fare + 
                         Embarked + 
                         Title + 
                         family_size, 
                       data = full %>% filter(!is.na(Age)), method = "anova")
full <- full %>% 
  mutate(Age = ifelse(is.na(Age), predict(predicted_age, full %>% filter(is.na(Age))), Age))

# 5. Train model. 
train_clean <- full[1:891, ]
test_clean <- full[892:1309, ] %>% 
  mutate(Survived = NULL, 
         Cabin = ifelse(Cabin == "", "H", "Cabin"), 
         Cabin = factor(Cabin))
train_clean <- train_clean %>% 
  mutate(Cabin = substr(Cabin, 1, 1), 
         Cabin = ifelse(Cabin == "", "H", Cabin), 
         Cabin = ifelse(Cabin == "T", "H", Cabin), 
         Cabin = factor(Cabin))
my_tree <- rpart(Survived ~ 
                   Age + 
                   Sex + 
                   Pclass + 
                   family_size, 
                 data = train_clean, 
                 method = "class", 
                 control = rpart.control(cp = 0.0001))
summary(my_tree)
prp(my_tree, type = 4, extra = 100)

# 6. Make predictions. 
predictions <- predict(my_tree, test_clean, type = "class")
head(predictions)
solutions <- data.frame(PassengerId = test_clean$PassengerId, 
                        Survived = predictions)
write_csv(solutions, "./00-titanic/data/03-redictions.csv")
