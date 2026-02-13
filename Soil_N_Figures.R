RROC_NO3_Grouped <- RROC_Soil %>%
  group_by(Treatment) %>%
  summarise_at(vars(Ch1_NO3.Primary_540nm),list(mean_no3_ppm = mean))

RROC_NH4_Grouped <- RROC_Soil %>%
  group_by(Treatment) %>%
  summarise_at(vars(Ch3_NH4.Primary_670nm),list(mean_nh4_ppm = mean))


ggplot(RROC_NO3_Grouped, aes(Treatment,mean_no3_ppm)) +
  geom_col() +
  scale_x_discrete(labels = label_wrap(7))

ggplot(RROC_NH4_Grouped, aes(Treatment,mean_nh4_ppm)) +
  geom_col() +
  scale_x_discrete(labels = label_wrap(7))
