#' ---
#' title: "Plot Data"
#' author: "Kevin Lu"
#' output: 
#'   html_document: 
#'     toc: true 
#'     toc_float: true
#'     number_sections: true
#' ---

#' # Sale Price
#' There doesn't seem to be any outliers in the data. 
ggplot(combined, aes(x = price_doc, fill = source)) + 
  geom_histogram(alpha = 0.5, binwidth = 100000) + 
  coord_cartesian(xlim = c(0, 20000000))
ggplot(combined, aes(x = price_doc_log, fill = source)) + 
  geom_histogram(alpha = 0.5, binwidth = 0.1)

#' # Sale Price by Product Type
ggplot(combined, aes(x = price_doc, fill = product_type)) + 
  geom_histogram(alpha = 0.5, binwidth = 100000) + 
  coord_cartesian(xlim = c(0, 20000000))


#' # Type of Features
type <- map_chr(combined, class) %>% 
  as_tibble() %>% 
  count(value)
print(type)

#' # Missing Values
miss_train <- map_dbl(combined %>% filter(source == "train"), function(x) sum(is.na(x) / length(x))) 
miss_train <- tibble(feature = attr(miss_train, "names"), miss_perc = miss_train) %>% 
  filter(miss_perc > 0)
miss_test <- map_dbl(combined %>% filter(source == "test"), function(x) sum(is.na(x) / length(x))) 
miss_test <- tibble(feature = attr(miss_test, "names"), miss_perc = miss_test) %>% 
  filter(miss_perc > 0)
miss <- bind_rows(miss_train %>% mutate(source = "train"), 
                  miss_test %>% mutate(source = "test"))
ggplot(miss, aes(x = reorder(feature, miss_perc), y = miss_perc)) + 
  geom_bar(stat = "identity", fill = "blue") + 
  coord_flip() + 
  facet_wrap(~ source)

#' # full_sq
#' total area in square meters, including loggias, balconies and other non-residential 
ggplot(combined, aes(x = log(full_sq), fill = source)) + 
  geom_density(alpha = 0.5)
ggplot(combined, aes(x = log(full_sq), y = price_doc_log)) + 
  geom_point(alpha = 0.1) + 
  geom_smooth()

#' # life_sq
#' living area in square meters, excluding loggias, balconies and other non-residential areas
ggplot(combined, aes(x = log(life_sq), fill = source)) + 
  geom_density(alpha = 0.5)
ggplot(combined, aes(x = log(life_sq), y = price_doc_log)) + 
  geom_point(alpha = 0.1) + 
  geom_smooth()

#' # kitch_sq
#' kitchen area
ggplot(combined, aes(x = log(kitch_sq), fill = source)) + 
  geom_density(alpha = 0.5)
ggplot(combined, aes(x = log(kitch_sq), y = price_doc_log)) + 
  geom_point(alpha = 0.1) + 
  geom_smooth()

#' # floor
#' for apartments, floor of the building
ggplot(combined, aes(x = floor, fill = source)) + 
  geom_density(alpha = 0.5, binwidth = 1)
ggplot(combined, aes(x = factor(floor), y = price_doc_log)) + 
  geom_jitter(alpha = 0.1) + 
  geom_boxplot()
ggplot(combined, aes(x = floor, y = price_doc_log)) + 
  geom_point(alpha = 0.1) + 
  geom_smooth()

#' # max_floor
#' number of floors in the building
ggplot(combined, aes(x = max_floor, fill = source)) + 
  geom_density(alpha = 0.5)
ggplot(combined, aes(x = factor(max_floor), y = price_doc_log)) + 
  geom_jitter(alpha = 0.1) + 
  geom_boxplot()
ggplot(combined, aes(x = max_floor, y = price_doc_log)) + 
  geom_point(alpha = 0.1) + 
  geom_smooth()

#' # floor greater than max_floor
ggplot(combined, aes(x = floor, y = max_floor)) + 
  geom_point(alpha = 0.1) + 
  geom_abline(slope = 1, intercept = 0) + 
  facet_wrap(~ source)

#' # num_room
#' number of living rooms
ggplot(combined, aes(x = num_room, fill = source)) + 
  geom_density(alpha = 0.5)
ggplot(combined, aes(x = factor(num_room), y = price_doc_log)) + 
  geom_jitter(alpha = 0.1) + 
  geom_boxplot()
ggplot(combined, aes(x = num_room, y = price_doc_log)) + 
  geom_point(alpha = 0.1) + 
  geom_smooth()

#' # product_type
#' owner-occupier purchase or investment
ggplot(combined, aes(x = factor(product_type), fill = source)) + 
  geom_histogram(alpha = 0.5, position = "identity", stat = "count")
ggplot(combined, aes(x = price_doc_log, fill = factor(product_type))) + 
  geom_density(alpha = 0.5)

#' # build_year
#' year built
ggplot(combined %>% filter(build_year > 1900, build_year < 2018), 
       aes(x = build_year, fill = source)) + 
  geom_density(alpha = 0.5)
ggplot(combined %>% filter(build_year > 1900, build_year < 2018), 
       aes(x = factor(build_year), y = price_doc_log)) + 
  geom_jitter(alpha = 0.1) + 
  geom_boxplot()
ggplot(combined %>% filter(build_year > 1900, build_year < 2018), 
       aes(x = build_year, y = price_doc_log)) + 
  geom_point(alpha = 0.1) + 
  geom_smooth()

#' # timestamp
#' date of transaction
ggplot(combined, aes(x = timestamp, fill = source)) + 
  geom_density(alpha = 0.5)
ggplot(combined, aes(x = timestamp, fill = source)) + 
  geom_bar()
ggplot(combined, aes(x = timestamp, y = price_doc_log)) + 
  geom_point(alpha = 0.1) + 
  geom_smooth()
ggplot(combined, aes(x = timestamp, y = price_doc)) + 
  geom_point(alpha = 0.1) + 
  geom_smooth() + 
  coord_cartesian(ylim = c(0, 10000000))

#' # month
ggplot(combined, aes(x = factor(month), fill = source)) + 
  geom_histogram(alpha = 0.5, position = "identity", stat = "count")
ggplot(combined, aes(x = factor(month), y = price_doc_log)) + 
  geom_jitter(alpha = 0.1) + 
  geom_boxplot()

#' # state
#' apartment condition
ggplot(combined, aes(x = factor(state), fill = source)) + 
  geom_histogram(alpha = 0.5, position = "identity", stat = "count")
ggplot(combined, aes(x = factor(state), y = price_doc_log)) + 
  geom_jitter(alpha = 0.1) + 
  geom_boxplot()

#' # material
#' wall material
ggplot(combined, aes(x = factor(material), fill = source)) + 
  geom_histogram(alpha = 0.5, position = "identity", stat = "count")
ggplot(combined, aes(x = factor(material), y = price_doc_log)) + 
  geom_jitter(alpha = 0.1) + 
  geom_boxplot()

#' # density
#' population density
ggplot(combined, aes(x = density, fill = source)) + 
  geom_density(alpha = 0.5)
ggplot(combined, aes(x = density, y = price_doc_log)) + 
  geom_point(alpha = 0.1) + 
  geom_smooth()

#' # sub_area
#' name of the district
ggplot(combined, aes(x = factor(sub_area), y = price_doc_log)) + 
  geom_jitter(alpha = 0.1) + 
  geom_boxplot()

#' # work_share
#' working population percentage
ggplot(combined, aes(x = work_share, fill = source)) + 
  geom_density(alpha = 0.5, binwidth = 0.01)
ggplot(combined, aes(x = work_share, y = price_doc_log)) + 
  geom_point(alpha = 0.1) + 
  geom_smooth()

#' # univeristy_top_20_raion
#' number of higher education institutions in the top ten ranking of the Federal rank
ggplot(combined, aes(x = university_top_20_raion, fill = source)) + 
  geom_histogram(alpha = 0.5, position = "identity", stat = "count")
ggplot(combined, aes(x = factor(university_top_20_raion), y = price_doc_log)) + 
  geom_jitter(alpha = 0.1) + 
  geom_boxplot()

#' # sport_objects_raion
#' number of sport objects
ggplot(combined, aes(x = sport_objects_raion, fill = source)) + 
  geom_density(alpha = 0.5)
ggplot(combined, aes(x = sport_objects_raion, y = price_doc_log)) + 
  geom_point(alpha = 0.1) + 
  geom_smooth()

#' # culture_objects_top_25_raion
#' number of  objects of cultural heritage
ggplot(combined, aes(x = culture_objects_top_25_raion, fill = source)) + 
  geom_density(alpha = 0.5)
ggplot(combined, aes(x = culture_objects_top_25_raion, y = price_doc_log)) + 
  geom_point(alpha = 0.1) + 
  geom_smooth(method = "lm")

#' # park_km
ggplot(combined, aes(x = park_km, fill = source)) + 
  geom_density(alpha = 0.5)
ggplot(combined, aes(x = park_km, y = price_doc_log)) + 
  geom_point(alpha = 0.1) + 
  geom_smooth()

#' # kremlin_km
ggplot(combined, aes(x = kremlin_km, fill = source)) + 
  geom_density(alpha = 0.5)
ggplot(combined, aes(x = kremlin_km, y = price_doc_log)) + 
  geom_point(alpha = 0.1) + 
  geom_smooth()

#' # Correlation between home characteristics
corrplot::corrplot(cor(combined[, c("full_sq", "life_sq", "floor", "max_floor", "build_year", 
                             "num_room", "kitch_sq", "state", "price_doc", "price_doc_log")], 
                       use = "complete.obs"))

#' # Correlation between demographics
corrplot::corrplot(cor(combined[, c("area_m", "raion_popul", "full_all", "male_f", "female_f", 
                                 "young_all", "young_female", "work_all", "work_male", "work_female", 
                                 "price_doc", "price_doc_log")], 
                       use = "complete.obs"))

#' # Correlation between school characteristics
corrplot::corrplot(cor(combined[, c("children_preschool", "preschool_quota", "preschool_education_centers_raion", 
                                 "children_school", "school_quota", "school_education_centers_raion", 
                                 "school_education_centers_top_20_raion", "university_top_20_raion", 
                                 "additional_education_raion", "additional_education_km", "university_km", 
                                 "price_doc", "price_doc_log")], 
                       use = "complete.obs"))

#' # Correlation between cultural and recreational characteristics
corrplot::corrplot(cor(combined[, c("sport_objects_raion", "culture_objects_top_25_raion", "shopping_centers_raion", 
                                "park_km", "fitness_km", "swim_pool_km", "ice_rink_km", "stadium_km", 
                                "basketball_km", "shopping_centers_km", "big_church_km", "church_synagogue_km", 
                                "mosque_km", "theater_km", "museum_km", "exhibition_km", "catering_km", 
                                "price_doc", "price_doc_log")], 
                      use = "complete.obs"))

#' # Correlation between infrastructure characteristics
corrplot::corrplot(cor(combined[, c("nuclear_reactor_km", "thermal_power_plant_km", "power_transmission_line_km", 
                                 "incineration_km", "water_treatment_km", "incineration_km", "railroad_station_walk_km", 
                                 "railroad_station_walk_min", "railroad_station_avto_km", "railroad_station_avto_min", 
                                 "public_transport_station_km", "public_transport_station_min_walk", "water_km", 
                                 "mkad_km", "ttk_km", "sadovoe_km", "bulvar_ring_km", "kremlin_km", 
                                 "price_doc", "price_doc_log")], 
                       use = "complete.obs"))

#' Macro
ggplot(macro, aes(x = timestamp, y = oil_urals)) + 
  geom_point() + 
  geom_line()
ggplot(macro, aes(x = timestamp, y = ppi)) + 
  geom_point() + 
  geom_line()
ggplot(macro, aes(x = timestamp, y = balance_trade)) + 
  geom_point() + 
  geom_line()
ggplot(macro, aes(x = timestamp, y = brent)) + 
  geom_point() + 
  geom_line()
ggplot(macro, aes(x = timestamp, y = usdrub)) + 
  geom_point() + 
  geom_line()
ggplot(macro, aes(x = timestamp, y = brent_rub)) + 
  geom_point() + 
  geom_line()
ggplot(macro, aes(x = timestamp, y = TTR::EMA(brent_rub, 30))) + 
  geom_point() + 
  geom_line()
ggplot(macro, aes(x = timestamp, y = TTR::EMA(oil_urals_rub, 30))) + 
  geom_point() + 
  geom_line()

train %>% 
  group_by(timestamp) %>% 
  summarise(median_price = median(as.numeric(price_doc))) %>% 
  ggplot(aes(x = timestamp, y = TTR::SMA(median_price, 10))) + 
  geom_point() + 
  geom_line() + 
  geom_smooth() + 
  coord_cartesian(ylim = c(0, 10000000))
train %>% 
  group_by(timestamp) %>% 
  summarise(median_price = median(as.numeric(price_doc))) %>% 
  ggplot(aes(x = timestamp, y = median_price)) + 
  geom_point() + 
  geom_smooth() + 
  coord_cartesian(ylim = c(0, 10000000))


temp <- macro %>% 
  mutate(test = TTR::EMA(rent_price_4.room_bus, 180))
ggplot(temp, aes(x = timestamp, y = rent_price_4.room_bus)) + 
  geom_point() + 
  geom_line() + 
  geom_line(aes(y = test))

# "oil_urals"
# "gdp_quart_growth"
# "cpi change"
# "ppi change"
# "eurrub change" 
# "usdrub change"
# "brent change"
# "mortgage_growth"
# "mortgage_rate"
# "rent_price_4.room_bus, seasonally adjusted"
# "rent_price_3room_bus, seasonally adjusted"
# "rent_price_2room_bus, seasonally adjusted"
# "rent_price_1room_bus, seasonally adjusted"
# "rent_price_3room_eco, seasonally adjusted"
# "rent_price_2room_eco, seasonaly adjusted, fix outlier"
# "rent_price_1room_eco, seasonally adjusted, fix outlier"
                   
        