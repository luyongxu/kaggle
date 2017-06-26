#' ---
#' title: "Average Predictions"
#' author: "Kevin Lu"
#' output: 
#'   html_document: 
#'     toc: true 
#'     toc_float: true
#'     number_sections: true
#' ---

#' #
sub_1 <- read_csv("./Mercedes Benz/Output/Base Models/baseline.csv") %>% 
  rename(ID = id)
sub_2 <- read_csv("./Mercedes Benz/Output/Base Models/xgb_10.csv")

#' # 
sub_3 <- bind_rows(sub_1, sub_2) %>% 
  group_by(ID) %>% 
  summarise(y = mean(y)) %>% 
  ungroup()
write_csv(sub_3, "./Mercedes Benz/Output/Base Models/sub_01.csv")


sub_1 <- sub_1 %>% 
  arrange(ID)
sub_2 <- sub_2 %>% 
  arrange(ID) 
sub_3 <- sub_3 %>% 
  arrange(ID)
ggplot(sub_1, aes(x = y, y = sub_3$y)) + geom_point(alpha = 0.2)
