#' ---
#' title: "Engineer Features"
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

#' # Clean macro data
colnames(macro) <- make.names(colnames(macro))
macro <- macro %>% 
  mutate(rent_price_2room_eco = ifelse(rent_price_2room_eco < 5, NA, rent_price_2room_eco), 
         rent_price_1room_eco = ifelse(rent_price_1room_eco < 5, NA, rent_price_1room_eco), 
         rent_price_2room_eco = zoo::na.locf(rent_price_2room_eco, na.rm = FALSE), 
         rent_price_1room_eco = zoo::na.locf(rent_price_1room_eco, na.rm = FALSE))

#' # Rent prices
macro <- macro %>% 
  mutate(rent_price_4.room_bus_ema180 = TTR::EMA(rent_price_4.room_bus, 180), 
         rent_price_3room_bus_ema180 = TTR::EMA(rent_price_3room_bus, 180), 
         rent_price_2room_bus_ema180 = TTR::EMA(rent_price_2room_bus, 180), 
         rent_price_1room_bus_ema180 = TTR::EMA(rent_price_1room_bus, 180), 
         rent_price_3room_eco_ema180 = TTR::EMA(rent_price_3room_eco, 180), 
         rent_price_2room_eco_ema180 = TTR::EMA(rent_price_2room_eco, 180), 
         rent_price_1room_eco_ema180 = TTR::EMA(rent_price_1room_eco, 180))

#' # Oil prices in rubles
macro <- macro %>% 
  mutate(oil_urals_rub = oil_urals * usdrub, 
         brent_rub = brent * usdrub)

#' # Select macro data
macro <- macro %>% 
  select(oil_urals, gdp_quart_growth, brent, 
         mortgage_growth, mortgage_rate, oil_urals_rub, brent_rub, 
         rent_price_4.room_bus_ema180, rent_price_3room_bus_ema180, 
         rent_price_2room_bus_ema180, rent_price_1room_bus_ema180, 
         rent_price_3room_eco_ema180, rent_price_2room_eco_ema180, 
         rent_price_1room_eco_ema180, timestamp)
# removed eurrub, usdrub, cpi, ppi, 

#' # Join macro data
# combined <- combined %>%
#   left_join(macro)

#' # Convert sale price to log. 
combined <- combined %>% 
  mutate(price_doc_log = log1p(price_doc))

#' # Density
combined <- combined %>% 
  mutate(area_km = area_m / 1000000, 
         density_raion = raion_popul / area_km)

#' # Kitchen Area Ratio
combined <- combined %>% 
  mutate(kitch_full_ratio = kitch_sq / full_sq)


#' # features that did not improve
#' # full_sq * cpi
# combined <- combined %>%
#   mutate(full_sq_cpi = full_sq * cpi)
#' # cpi change
# cpi <- macro %>% 
#   group_by(year = year(timestamp), month = month(timestamp)) %>% 
#   summarise(cpi = mean(cpi)) %>% 
#   ungroup() %>% 
#   mutate(cpi_change_01 = cpi / lag(cpi, 1) - 1, 
#          cpi_change_03 = cpi / lag(cpi, 3) - 1, 
#          cpi_change_12 = cpi / lag(cpi, 12) - 1) %>% 
#   select(year, month, cpi_change_01, cpi_change_03, cpi_change_12)
# combined <- combined %>% 
#   left_join(cpi)
#' # Parse timestamp
# combined <- combined %>% 
#   mutate(day = day(timestamp), 
#          wday = wday(timestamp), 
#          yday = yday(timestamp), 
#          week = week(timestamp), 
#          month = month(timestamp), 
#          year = year(timestamp), 
#          year_month = year * 100 + month, 
#          year_week = year * 100 + week)
#' # Count of transactions in year month and year week.
# combined <- combined %>% 
#   left_join(combined %>% 
#               count(year_month) %>% 
#               rename(year_month_count = n)) %>% 
#   left_join(combined %>% 
#               count(year_week) %>% 
#               rename(year_week_count = n))
#' # log full_sq, life_sq, kitch_sq
# combined <- combined %>% 
#   mutate(full_sq_log = log1p(full_sq), 
#          life_sq_log = log1p(life_sq), 
#          kitch_sq_log = log1p(kitch_sq))
#' # Living Area Ratio, Kitchen Area Ratio, Room Size
# combined <- combined %>% 
#   mutate(life_ratio = life_sq / full_sq, 
#          kitch_full_ratio = kitch_sq / full_sq, 
#          kitch_life_ratio = kitch_sq / life_sq, 
#          room_size = life_sq / num_room, 
#          non_life_sq = full_sq - life_sq)
#' # Working Population Percentage
# combined <- combined %>% 
#   mutate(work_share = work_all / raion_popul)
#' # Floors From Max Floor
# combined <- combined %>% 
#   mutate(floor_from_max = max_floor - floor, 
#          floor_ratio = floor / max_floor)
#' # Age of building
# combined <- combined %>% 
#   mutate(age = year - build_year)
#' # NA count
# combined <- combined %>% 
#   mutate(na_count = rowSums(is.na(combined)))
#' # School features
# combined <- combined %>% 
#   mutate(preschool_ratio = children_preschool / preschool_quota, 
#          school_ratio = children_school / school_quota)
#' # Apartment
# combined <- combined %>% 
#   mutate(apartment = str_c(sub_area, metro_km_avto))
#' # Sub area price
# sub_area_price <- combined %>% 
#   mutate(full_sq_price = price_doc / full_sq) %>% 
#   group_by(sub_area) %>% 
#   summarise(sub_area_price_mean = mean(full_sq_price, na.rm = TRUE), 
#             sub_area_price_median = median(full_sq_price, na.rm = TRUE))
# combined <- combined %>% 
#   left_join(sub_area_price)
#' # Join latitude longitude data. 
# combined <- combined %>% 
#   left_join(longlat %>% select(id, lat, lon))
#' Close to Kremlin
# combined <- combined %>% 
#   mutate(kremlin_close = ifelse(kremlin_km <= 0.10, 1, 0))
