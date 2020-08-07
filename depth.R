df_depth <- df %>%
  filter(str_detect(flags, "depth_exceeds_bath")) %>%
  select(dataset_id, flags, depth, minimumDepthInMeters, maximumDepthInMeters, bathymetry)
df_depth$d <- pmax(test$minimumDepthInMeters, test$maximumDepthInMeters, na.rm = TRUE)

ggplot() +
  geom_point(data = df_depth, aes(bathymetry, d)) +
  geom_abline(color = "coral3", linetype = "dashed") +
  theme_minimal() +
  ylab("depth")
