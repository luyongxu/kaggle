#' ---
#' title: "Plot Data"
#' author: "Kevin Lu"
#' output: 
#'   html_document: 
#'     toc: true 
#'     toc_float: true
#'     number_sections: true
#' ---

#' # combine data
combined <- bind_rows(train %>% mutate(source = "train"), 
                      test %>% mutate(source = "test"))

#' # Target Variable
ggplot(train, aes(x = y)) + 
  geom_histogram(binwidth = 1, fill = "blue")
ggplot(train, aes(x = y)) + 
  geom_histogram(binwidth = 1, fill = "blue") + 
  coord_cartesian(xlim = c(70, 180))

#' # Type of Variables
combined %>% 
  map_chr(class) %>% 
  as_tibble()
combined %>% 
  map_chr(class) %>% 
  as_tibble() %>% 
  count(value)

#' # Missing Values
combined %>% 
  map_dbl(function(x) sum(is.na(x) / length(x))) %>% 
  as_tibble() %>% 
  mutate(column_name = colnames(combined)) %>% 
  filter(value > 0)

#' # X0
ggplot(combined, aes(x = X0, fill = source)) + 
  geom_bar(position = "dodge")
ggplot(train, aes(x = reorder(X0, y, median), y = y)) + 
  geom_point(colour = "blue", alpha = 0.2) + 
  coord_cartesian(ylim = c(70, 180))
ggplot(train, aes(x = reorder(X0, y, median), y = y)) + 
  geom_boxplot(colour = "blue") + 
  coord_cartesian(ylim = c(70, 180))

#' # X1
ggplot(combined, aes(x = X1, fill = source)) + 
  geom_bar(position = "dodge")
ggplot(train, aes(x = reorder(X1, y, median), y = y)) + 
  geom_point(colour = "blue", alpha = 0.2) + 
  coord_cartesian(ylim = c(70, 180))
ggplot(train, aes(x = reorder(X1, y, median), y = y)) + 
  geom_boxplot(colour = "blue") + 
  coord_cartesian(ylim = c(70, 180))

#' # X2
ggplot(combined, aes(x = X2, fill = source)) + 
  geom_bar(position = "dodge")
ggplot(train, aes(x = reorder(X2, y, median), y = y)) + 
  geom_point(colour = "blue", alpha = 0.2) + 
  coord_cartesian(ylim = c(70, 180))
ggplot(train, aes(x = reorder(X2, y, median), y = y)) + 
  geom_boxplot(colour = "blue") + 
  coord_cartesian(ylim = c(70, 180))

#' # X3
ggplot(combined, aes(x = X3, fill = source)) + 
  geom_bar(position = "dodge")
ggplot(train, aes(x = reorder(X3, y, median), y = y)) + 
  geom_point(colour = "blue", alpha = 0.2) + 
  coord_cartesian(ylim = c(70, 180))
ggplot(train, aes(x = reorder(X3, y, median), y = y)) + 
  geom_boxplot(colour = "blue") + 
  coord_cartesian(ylim = c(70, 180))

#' # X4
ggplot(combined, aes(x = X4, fill = source)) + 
  geom_bar(position = "dodge")
ggplot(train, aes(x = reorder(X4, y, median), y = y)) + 
  geom_point(colour = "blue", alpha = 0.2) + 
  coord_cartesian(ylim = c(70, 180))
ggplot(train, aes(x = reorder(X4, y, median), y = y)) + 
  geom_boxplot(colour = "blue") + 
  coord_cartesian(ylim = c(70, 180))

#' # X5
ggplot(combined, aes(x = X5, fill = source)) + 
  geom_bar(position = "dodge")
ggplot(train, aes(x = reorder(X5, y, median), y = y)) + 
  geom_point(colour = "blue", alpha = 0.2) + 
  coord_cartesian(ylim = c(70, 180))
ggplot(train, aes(x = reorder(X5, y, median), y = y)) + 
  geom_boxplot(colour = "blue") + 
  coord_cartesian(ylim = c(70, 180))

#' # X6
ggplot(combined, aes(x = X6, fill = source)) + 
  geom_bar(position = "dodge")
ggplot(train, aes(x = reorder(X6, y, median), y = y)) + 
  geom_point(colour = "blue", alpha = 0.2) + 
  coord_cartesian(ylim = c(70, 180))
ggplot(train, aes(x = reorder(X6, y, median), y = y)) + 
  geom_boxplot(colour = "blue") + 
  coord_cartesian(ylim = c(70, 180))

#' # X8
ggplot(combined, aes(x = X8, fill = source)) + 
  geom_bar(position = "dodge")
ggplot(train, aes(x = reorder(X8, y, median), y = y)) + 
  geom_point(colour = "blue", alpha = 0.2) + 
  coord_cartesian(ylim = c(70, 180))
ggplot(train, aes(x = reorder(X8, y, median), y = y)) + 
  geom_boxplot(colour = "blue") + 
  coord_cartesian(ylim = c(70, 180))

#' # Binary Variables
binary_combined <- combined %>% 
  select_if(is.numeric) %>% 
  select(-ID, -y) %>% 
  gather(feature, value)
binary_train <- train %>% 
  select_if(is.numeric) %>% 
  select(-ID) %>% 
  gather(feature, value, -y)
binary_average <- binary_combined %>% 
  group_by(feature) %>% 
  summarise(average_value = mean(value)) %>% 
  arrange(average_value)
ggplot(binary_average[1:100, ], aes(x = reorder(feature, average_value), y = average_value)) + 
  geom_point() + 
  coord_flip() + 
  theme(axis.text.y = element_text(size = 8))
ggplot(binary_average[101:200, ], aes(x = reorder(feature, average_value), y = average_value)) + 
  geom_point() + 
  coord_flip() + 
  theme(axis.text.y = element_text(size = 8))
ggplot(binary_train[1:500000, ], aes(x = factor(value), y = y, colour = factor(value))) + 
  geom_boxplot() + 
  facet_wrap(~ feature)

#' # ID
ggplot(combined, aes(x = ID, y = y)) + 
  geom_point(alpha = 0.2) + 
  coord_cartesian(ylim = c(70, 180)) + 
  geom_smooth()
ggplot(combined, aes(x = ID, fill = source)) + 
  geom_histogram(binwidth = 20)

#' # X0 and X2
ggplot(train, aes(x = reorder(X0, y, median), y = reorder(X2, y, median))) + 
  geom_count()
