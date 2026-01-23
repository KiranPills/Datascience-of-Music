# ============================================================
# 03_advanced.R - R Graph Gallery Style Visualizations
# ============================================================

source("visualizations/scripts/01_data_cleaning.R")

cat("\n=== Creating Advanced Gallery Plots ===\n")

# ============================================================
# A. RIDGELINE PLOTS (ggridges)
# ============================================================

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

# Plot 2: Ridgeline by training
p_ridgeline_training <- ggplot(
  survey_long %>% filter(!is.na(training_ordered)),
  aes(x = valence_num, y = training_ordered, fill = training_ordered)
) +
  geom_density_ridges(alpha = 0.7, scale = 1.5) +
  scale_fill_brewer(palette = "Blues") +
  labs(
    title = "Valence Distribution by Musical Training",
    x = "Valence",
    y = "Years of Musical Training"
  ) +
  theme(legend.position = "none")

save_plot(p_ridgeline_training, "02_ridgeline_training.png", plots_advanced)

# Plot 3: Ridgeline with gradient fill
p_ridgeline_gradient <- ggplot(
  survey_long,
  aes(x = valence_num, y = song, fill = after_stat(x))
) +
  geom_density_ridges_gradient(
    scale = 1.2,
    rel_min_height = 0.01
  ) +
  scale_fill_viridis_c(option = "C", name = "Valence") +
  labs(
    title = "Valence Distribution with Gradient Fill",
    x = "Valence (1 = Negative, 6 = Very Positive)",
    y = "Song"
  )

save_plot(p_ridgeline_gradient, "03_ridgeline_gradient.png", plots_advanced, height = 8)

# ============================================================
# B. HEATMAPS
# ============================================================

# Plot 4: Correlation heatmap (using ggplot2)
correlation_data <- survey_long %>%
  select(emotional_num, familiarity_num, valence_num, comprehension_num) %>%
  drop_na()

cor_matrix <- cor(correlation_data, use = "pairwise.complete.obs")
colnames(cor_matrix) <- c("Emotional", "Familiarity", "Valence", "Comprehension")
rownames(cor_matrix) <- colnames(cor_matrix)

# Convert to long format for ggplot
cor_long <- as.data.frame(as.table(cor_matrix)) %>%
  rename(Var1 = Var1, Var2 = Var2, correlation = Freq)

p_correlation <- ggplot(cor_long, aes(x = Var1, y = Var2, fill = correlation)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = round(correlation, 2)), color = "black", size = 5) +
  scale_fill_gradient2(low = "#3B4CC0", mid = "white", high = "#B40426",
                       midpoint = 0, limits = c(-1, 1)) +
  labs(
    title = "Correlation Matrix of Survey Measures",
    x = "", y = "", fill = "Correlation"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  ) +
  coord_fixed()

save_plot(p_correlation, "04_correlation_heatmap.png", plots_advanced, width = 8)

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

# Plot 6: Heatmap by demographic group
group_heatmap <- survey_long %>%
  filter(!is.na(listening_freq)) %>%
  group_by(listening_freq, song) %>%
  summarize(mean_emotional = mean(emotional_num, na.rm = TRUE), .groups = "drop")

p_heatmap_group <- ggplot(
  group_heatmap,
  aes(x = song, y = listening_freq, fill = mean_emotional)
) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = round(mean_emotional, 1)), size = 3.5) +
  scale_fill_gradient2(
    low = "#3B4CC0", mid = "white", high = "#B40426",
    midpoint = 4
  ) +
  labs(
    title = "Emotional Intensity: Listening Frequency x Song",
    x = "Song",
    y = "Listening Frequency",
    fill = "Mean Score"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

save_plot(p_heatmap_group, "06_heatmap_group.png", plots_advanced, height = 5)

# ============================================================
# C. VIOLIN PLOTS
# ============================================================

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

# Plot 8: Split violin by group
p_violin_split <- ggplot(
  survey_long %>% filter(listening_freq %in% c("Daily", "Less than weekly")),
  aes(x = song, y = valence_num, fill = listening_freq)
) +
  geom_violin(
    position = position_dodge(width = 0.7),
    alpha = 0.7,
    trim = FALSE
  ) +
  scale_fill_manual(values = c("Daily" = "#2E86AB", "Less than weekly" = "#F18F01")) +
  labs(
    title = "Valence Distribution: Daily vs. Less Than Weekly Listeners",
    x = "Song",
    y = "Valence",
    fill = "Listening Frequency"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

save_plot(p_violin_split, "08_violin_split.png", plots_advanced, width = 12)

# Plot 9: Violin with beeswarm overlay
p_violin_beeswarm <- ggplot(
  survey_long %>% filter(!is.na(training_ordered)),
  aes(x = training_ordered, y = comprehension_num, fill = training_ordered)
) +
  geom_violin(alpha = 0.5, trim = FALSE) +
  geom_quasirandom(alpha = 0.6, size = 2, width = 0.2) +
  scale_fill_brewer(palette = "Blues") +
  labs(
    title = "Lyric Comprehension by Musical Training",
    subtitle = "Violin with beeswarm overlay",
    x = "Years of Musical Training",
    y = "Comprehension Score"
  ) +
  theme(legend.position = "none")

save_plot(p_violin_beeswarm, "09_violin_beeswarm.png", plots_advanced)

# ============================================================
# D. DUMBBELL CHARTS
# ============================================================

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

# ============================================================
# E. LOLLIPOP CHARTS
# ============================================================

# Plot 11: Basic lollipop chart
p_lollipop <- ggplot(
  song_summary,
  aes(x = reorder(song, mean_emotional), y = mean_emotional)
) +
  geom_segment(
    aes(x = reorder(song, mean_emotional), xend = reorder(song, mean_emotional),
        y = 0, yend = mean_emotional),
    color = "gray50",
    linewidth = 1
  ) +
  geom_point(aes(color = mean_emotional), size = 6) +
  scale_color_viridis_c(option = "plasma") +
  coord_flip() +
  labs(
    title = "Mean Emotional Intensity by Song",
    subtitle = "Lollipop chart ranked by intensity",
    x = "Song",
    y = "Mean Emotional Intensity",
    color = "Intensity"
  )

save_plot(p_lollipop, "11_lollipop.png", plots_advanced, width = 8, height = 6)

# Plot 12: Lollipop with multiple measures
measures_summary <- song_summary %>%
  select(song, mean_emotional, mean_familiarity, mean_valence) %>%
  pivot_longer(-song, names_to = "measure", values_to = "mean_score") %>%
  mutate(measure = str_remove(measure, "mean_"))

p_lollipop_multi <- ggplot(
  measures_summary,
  aes(x = song, y = mean_score, color = measure)
) +
  geom_segment(
    aes(x = song, xend = song, y = 0, yend = mean_score),
    position = position_dodge(width = 0.5),
    linewidth = 1
  ) +
  geom_point(position = position_dodge(width = 0.5), size = 4) +
  scale_color_brewer(palette = "Set1") +
  labs(
    title = "Multiple Measures by Song",
    x = "Song",
    y = "Mean Score",
    color = "Measure"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

save_plot(p_lollipop_multi, "12_lollipop_multi.png", plots_advanced)

# ============================================================
# F. ADDITIONAL GALLERY PLOTS
# ============================================================

# Plot 13: Circular bar chart (for element influence)
element_counts <- survey_long %>%
  filter(!is.na(element)) %>%
  count(element) %>%
  mutate(pct = n / sum(n) * 100)

p_circular_bar <- ggplot(element_counts, aes(x = element, y = pct, fill = element)) +
  geom_bar(stat = "identity", width = 0.8) +
  coord_polar(start = 0) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Which Element Influenced Emotion Most?",
    subtitle = "Circular bar chart",
    y = "Percentage"
  ) +
  theme(
    axis.text.x = element_text(size = 10),
    axis.title.x = element_blank(),
    legend.position = "none"
  )

save_plot(p_circular_bar, "13_circular_bar.png", plots_advanced, width = 8, height = 8)

# Plot 14: Slope chart (changes across two songs)
slope_data <- survey_long %>%
  filter(song %in% c("Song 1", "Song 6")) %>%
  select(response_id, song, emotional_num) %>%
  filter(!is.na(emotional_num))

p_slope <- ggplot(
  slope_data,
  aes(x = song, y = emotional_num, group = response_id)
) +
  geom_line(alpha = 0.4, color = "gray60") +
  geom_point(aes(color = song), size = 3) +
  scale_color_manual(values = c("Song 1" = "#2E86AB", "Song 6" = "#F18F01")) +
  labs(
    title = "Individual Changes: Song 1 to Song 6",
    subtitle = "Each line represents one participant",
    x = "Song",
    y = "Emotional Intensity"
  ) +
  theme(legend.position = "none")

save_plot(p_slope, "14_slope_chart.png", plots_advanced, width = 8)

# Plot 15: Radar-style plot using ggplot (polar coordinates)
radar_data <- song_summary %>%
  select(song, mean_emotional, mean_familiarity, mean_valence, mean_comprehension) %>%
  pivot_longer(-song, names_to = "measure", values_to = "score") %>%
  mutate(measure = str_remove(measure, "mean_") %>% str_to_title())

p_radar_style <- ggplot(radar_data, aes(x = measure, y = score, group = song, color = song)) +
  geom_polygon(aes(fill = song), alpha = 0.1) +
  geom_point(size = 3) +
  geom_line(linewidth = 1) +
  coord_polar() +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  facet_wrap(~ song, ncol = 3) +
  labs(
    title = "Song Profiles: Radar-Style Comparison",
    subtitle = "Each panel shows one song's profile across all measures",
    x = "", y = "Score"
  ) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 8)
  )

save_plot(p_radar_style, "15_radar_style.png", plots_advanced, width = 12, height = 10)

cat("\n=== Advanced gallery plots complete! ===\n")
cat("Output folder:", plots_advanced, "\n")
