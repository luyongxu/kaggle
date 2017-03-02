# Code adapated from https://www.kaggle.com/mrisdal/titanic/exploring-survival-on-the-titanic.
# Submission scores 0.81340 on public leaderboard.

# 1. Load packages. 
library("ggplot2")
library("ggthemes")
library("scales")
library("dplyr")
library("mice")
library("randomForest")
library("readr")

# 2. Load data. 
train <- read_csv("./Titanic/Raw Data/train.csv")
test <- read_csv("./Titanic/Raw Data/test.csv")
full <- bind_rows(train, test)

# 3. Feature engineering. 
# 3.1 Standardize title and name.
rare_title <- c("Dona", "Lady", "the Countess", "Capt", "Col", "Don", "Dr", "Major", "Rev", "Sir", "Jonkheer")
full <- full %>% 
  mutate(Title = gsub("(.*, )|(\\..*)", "", Name), 
         Title = ifelse(Title == "Mlle", "Miss", Title), 
         Title = ifelse(Title == "Ms", "Miss", Title), 
         Title = ifelse(Title == "Mme", "Mrs", Title), 
         Title = ifelse(Title %in% rare_title, "Rare Title", Title), 
         Surname = sapply(Name, function(x) strsplit(x, split = "[,.]")[[1]][1]))
table(full$Sex, full$Title)

# 3.2 Family features. 
full <- full %>% 
  mutate(Fsize = SibSp + Parch + 1, 
         FsizeD = ifelse(Fsize == 1, "singleton", NA), 
         FsizeD = ifelse(Fsize < 5 & Fsize > 1, "small", FsizeD), 
         FsizeD = ifelse(Fsize > 4, "large", FsizeD), 
         Family = paste(Surname, Fsize, sep = "_"))
ggplot(full %>% filter(!is.na(Survived)), aes(x = Fsize, fill = factor(Survived))) + 
  geom_bar(stat = "count", position = "dodge") + 
  scale_x_continuous(breaks = c(1:11)) + 
  labs(x = "Family Size")
mosaicplot(table(full$FsizeD, full$Survived), main = "Family Size by Survival", shade = TRUE)

# 3.3 Cabin features.
full <- full %>% 
  mutate(Deck = factor(sapply(Cabin, function(x) strsplit(x, NULL)[[1]][1])))

# 4. Dealing with missing values. 
# 4.1 Fix missing Embarked.
ggplot(full, aes(x = Embarked, y = Fare, fill = factor(Pclass))) + 
  geom_boxplot() + 
  geom_hline(aes(yintercept = 80), colour = "red")
full <- full %>% 
  mutate(Embarked = ifelse(is.na(Embarked), "C", Embarked)) 

# 4.2 Fix missing Fare.
ggplot(full %>% filter(Pclass == "3", Embarked == "S"), aes(x = Fare)) + 
  geom_density(fill = "blue", alpha = 0.5) + 
  geom_vline(aes(xintercept = median(Fare, na.rm = TRUE)))
temp <- full %>% filter(Pclass == "3", Embarked == "S") %>% select(Fare) %>% as.vector()
median_fare <- median(temp$Fare, na.rm = TRUE)
full <- full %>% 
  mutate(Fare = ifelse(is.na(Fare), median_fare, Fare))

# 4.3 Impute Age. 
factor_vars <- c("PassengerId", "Pclass", "Sex", "Embarked", "Title", "Surname", "Family", "FsizeD")
full[factor_vars] <- lapply(full[factor_vars], function(x) as.factor(x))
set.seed(129)
mice_mod <- mice(full[, !names(full) %in% c("PassengerId", "Name", "Ticker", "Cabin", "Family", "Surname", "Survived")], method = "rf")
mice_output <- complete(mice_mod) %>% mutate(Age_Imputed = Age)
ggplot(full, aes(x = Age)) + geom_histogram(fill = "blue", alpha = 0.5)
ggplot(mice_output, aes(x = Age_Imputed)) + geom_histogram(fill = "red", alpha = 0.5)
full <- full %>% 
  mutate(Age = mice_output$Age_Imputed)

# 5. More feature engineering. 
# 5.1. Create Child indicator.
ggplot(full %>% filter(!is.na(Survived)), aes(Age, fill = factor(Survived))) + 
  geom_histogram() + 
  facet_grid(. ~ Sex)
full <- full %>% 
  mutate(Child = ifelse(Age < 18, "Child", NA), 
         Child = ifelse(Age >= 18, "Adult", Child), 
         Child = factor(Child))
table(full$Child, full$Survived)

# 5.2 Create Mother indicator. 
full <- full %>% 
  mutate(Mother = ifelse(Sex == "female" & Parch > 0 & Age > 18 & Title != "Miss", "Mother", "Not Mother"), 
         Mother = factor(Mother))
table(full$Mother, full$Survived)
md.pattern(full)
VIM::aggr(full)

# 6. Predict
# 6.1 Split into training and test sets. 
train <- full[1:891, ]
test <- full[892:1309, ]

# 6.2 Train random forest model.
set.seed(754)
rf_model <- randomForest(factor(Survived) ~ 
                           Pclass + 
                           Sex + 
                           Age + 
                           SibSp + 
                           Parch + 
                           Fare + 
                           Embarked + 
                           Title + 
                           FsizeD + 
                           Child + 
                           Mother, 
                         data = train)
plot(rf_model)
legend("topright", colnames(rf_model$err.rate), col = 1:3, fill = 1:3)

# 6.3 Variable importance. 
importance <- importance(rf_model)
varImportance <- data.frame(Variables = row.names(importance), 
                            Importance = round(importance[ , "MeanDecreaseGini"], 2))
rankImportance <- varImportance %>% 
  mutate(Rank = paste(dense_rank(desc(Importance))))
ggplot(rankImportance, aes(x = reorder(Variables, Importance), y = Importance, fill = Importance)) + 
  geom_bar(stat = "identity") + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank), colour = "red") + 
  coord_flip()

# 6.4 Make predictions. 
prediction <- predict(rf_model, test)
solution <- data.frame(PassengerId = test$PassengerId, Survived = prediction)
write_csv(solution, "./Titanic/Output/1.001 Predictions.csv")
