test_ensemble <- read_csv("./04-two-sigma-connect/output/test-ensemble.csv") %>%
  rename(low_a = low,
         medium_a = medium,
         high_a = high)
it_is_lit <- read_csv("./04-two-sigma-connect/output/it-is-lit.csv") %>%
  rename(low_b = low,
         medium_b = medium,
         high_b = high)
en <- test_ensemble %>%
  left_join(it_is_lit) %>%
  mutate(low =
           0.6 * low_a +
           0.4 * low_b,
         medium =
           0.6 * medium_a +
           0.4 * medium_b,
         high =
           0.6 * high_a +
           0.4 * high_b) %>%
  select(listing_id, low, medium, high)
write_csv(en, "./04-two-sigma-connect/output/ensemble-01.csv")
