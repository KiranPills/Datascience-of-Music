# 03_advanced.R - R Graph Gallery Style Visualizations

source("visualizations/scripts/01b_load_cleaned_data.R")

# Ridgeline Plots

# Plot 1: Emotional Intensity distributions across songs
p_ridgeline_emotion <- ggplot(
  survey_long,
  aes(x = emotional_num, y = song, fill = song)
) +
  geom_density_ridges(
    alpha = 0.7,
    scale = 1.2,
    rel_min_height = 0.01,
    quantile_lines = TRUE,
    quantiles = 2
  ) +
  scale_fill_viridis_d(option = "D") +
  labs(
    title = "Distribution of Emotional Intensity Across Songs",
    subtitle = "Ridgeline plot with median lines",
    x = "Emotional Intensity (1-7)",
    y = "Song"
  ) +
  theme(legend.position = "none")

save_plot(p_ridgeline_emotion, "01_ridgeline_emotion.png", plots_advanced, height = 8)

# Heatmaps

# Plot 5: Heatmap of mean scores (Song x Measure)
heatmap_data <- song_summary %>%
  select(song, mean_emotional, mean_familiarity, mean_valence, mean_comprehension) %>%
  pivot_longer(-song, names_to = "measure", values_to = "mean_score") %>%
  mutate(
    measure = str_remove(measure, "mean_"),
    measure = str_to_title(measure)
  )

p_heatmap_songs <- ggplot(
  heatmap_data,
  aes(x = measure, y = song, fill = mean_score)
) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = round(mean_score, 2)), color = "white", size = 4) +
  scale_fill_viridis_c(option = "magma") +
  labs(
    title = "Mean Scores Heatmap: Songs x Measures",
    x = "Measure",
    y = "Song",
    fill = "Mean Score"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  )

save_plot(p_heatmap_songs, "05_heatmap_songs.png", plots_advanced, width = 8, height = 6)

# Violin Plots

# Plot 7: Basic violin with box plot inside
p_violin_basic <- ggplot(
  survey_long,
  aes(x = song, y = emotional_num, fill = song)
) +
  geom_violin(alpha = 0.7, trim = FALSE) +
  geom_boxplot(width = 0.15, fill = "white", alpha = 0.8) +
  scale_fill_viridis_d() +
  labs(
    title = "Emotional Intensity Distribution by Song",
    subtitle = "Violin plots with embedded box plots",
    x = "Song",
    y = "Emotional Intensity"
  ) +
  theme(legend.position = "none")

save_plot(p_violin_basic, "07_violin_basic.png", plots_advanced)

# Dumbbell Charts

# Plot 10: Dumbbell comparing two groups
dumbbell_data <- survey_long %>%
  filter(listening_freq %in% c("Daily", "Less than weekly")) %>%
  group_by(song, listening_freq) %>%
  summarize(mean_emotional = mean(emotional_num, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = listening_freq, values_from = mean_emotional) %>%
  rename(daily = Daily, less_weekly = `Less than weekly`) %>%
  filter(!is.na(daily), !is.na(less_weekly))

p_dumbbell <- ggplot(dumbbell_data) +
  geom_segment(
    aes(x = daily, xend = less_weekly, y = song, yend = song),
    color = "gray60",
    linewidth = 1.5
  ) +
  geom_point(aes(x = daily, y = song), color = "#2E86AB", size = 5) +
  geom_point(aes(x = less_weekly, y = song), color = "#F18F01", size = 5) +
  labs(
    title = "Emotional Intensity: Daily vs. Less Than Weekly Listeners",
    subtitle = "Blue = Daily, Orange = Less than weekly",
    x = "Mean Emotional Intensity",
    y = "Song"
  ) +
  theme(panel.grid.major.y = element_blank())

save_plot(p_dumbbell, "10_dumbbell.png", plots_advanced, height = 6)
