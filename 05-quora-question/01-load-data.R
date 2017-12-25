# 1. Load libraries. 
library(tidyverse)
library(stringr)
library(tidytext)

# 2. Load data. 
train <- read_csv("./05-quora-question/data/train.csv")
test <- read_csv("./05-quora-question/data/test.csv")

# 3. Feature engineering. 
questions <- bind_rows(train %>% 
                         select(question = question1) %>% 
                         mutate(source = "train"), 
                       train %>% 
                         select(question = question2) %>% 
                         mutate(source = "train"), 
                       test %>% 
                         select(question = question1) %>% 
                         mutate(source = "test"), 
                       test %>% 
                         select(question = question2) %>% 
                         mutate(source = "test"))
questions <- questions %>% 
  mutate(question_wcount = str_count(question, "\\S+"), 
         question_nchar = nchar(question))
common_ratio <- function(q1, q2) { 
  length(intersect(q1, q2)) / max(length(union(q1, q2)), 1)
}
train <- train %>% 
  mutate(question1_1gram = str_split(question1, "\\s+"), 
         question2_1gram = str_split(question2, "\\s+"), 
         common_1gram = map2(question1_1gram, question2_1gram, intersect), 
         common_1gram_count = map_dbl(common_1gram, length), 
         common_1gram_ratio = unlist(map2(question1_1gram, question2_1gram, common_ratio)))

# 4. Visualize data. 
# 4.1 Distribution of target variable
ggplot(train, aes(x = factor(is_duplicate))) + geom_bar()

# 4.2 Distribution of question word count. 
ggplot(questions, aes(x = question_wcount, fill = source)) + 
  geom_histogram(binwidth = 1, alpha = 0.5) + 
  coord_cartesian(xlim = c(0, 50))
ggplot(questions, aes(x = question_wcount, colour = source)) + 
  geom_density(alpha = 0.5) + 
  coord_cartesian(xlim = c(0, 50))

# 4.3 Distribution of question character count. 
ggplot(questions, aes(x = question_nchar, fill = source)) + 
  geom_histogram(binwidth = 1, alpha = 0.5) + 
  coord_cartesian(xlim = c(0, 200))
ggplot(questions, aes(x = question_nchar, fill = source)) + 
  geom_density(alpha = 0.5) + 
  coord_cartesian(xlim = c(0, 200))

# 4.3 Distribution of common 1grams. 
ggplot(train, aes(x = common_1gram_count, fill = factor(is_duplicate))) + 
  geom_histogram(binwidth = 1, alpha = 0.5) + 
  coord_cartesian(xlim = c(0, 20))
ggplot(train, aes(x = common_1gram_count, fill = factor(is_duplicate))) + 
  geom_histogram(binwidth = 1, alpha = 0.5) + 
  coord_cartesian(xlim = c(0, 20)) + 
  facet_wrap(~ factor(is_duplicate), ncol = 1)

