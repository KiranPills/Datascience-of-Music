# ============================================================
# 02_multivariate.R - Multivariate Visualizations
# ============================================================

source("visualizations/scripts/01_data_cleaning.R")

cat("\n=== Creating Multivariate Plots ===\n")

# ============================================================
# A. SCATTER PLOTS WITH COLOR BY GROUP
# ============================================================

# Plot 1: Emotional Intensity vs Valence, colored by Listening Frequency
p_scatter_emotion_valence <- ggplot(
  survey_long,
  aes(x = valence_num, y = emotional_num, color = listening_freq)
) +
  geom_jitter(alpha = 0.6, size = 3, width = 0.2, height = 0.2) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +
  scale_color_manual(values = palette_listening_freq, na.value = "gray50") +
  labs(
    title = "Emotional Intensity vs. Valence by Listening Frequency",
    subtitle = "Each point represents one song rating",
    x = "Valence (1 = Negative, 6 = Very Positive)",
    y = "Emotional Intensity (1-7)",
    color = "Listening Frequency"
  )

save_plot(p_scatter_emotion_valence, "01_scatter_emotion_valence.png", plots_multivariate)

# Plot 2: Familiarity vs Comprehension, colored by English Level
p_scatter_familiar_comp <- ggplot(
  survey_long,
  aes(x = comprehension_num, y = familiarity_num, color = english_simple)
) +
  geom_jitter(alpha = 0.7, size = 3, width = 0.15, height = 0.15) +
  scale_color_viridis_d(option = "plasma", na.value = "gray70") +
  labs(
    title = "Familiarity vs. Lyric Comprehension by English Proficiency",
    x = "Lyric Comprehension (1-5)",
    y = "Familiarity (1-7)",
    color = "English Level"
  )

save_plot(p_scatter_familiar_comp, "02_scatter_familiar_comp.png", plots_multivariate)

# Plot 3: Scatter with shape AND color (two grouping variables)
p_scatter_dual_group <- ggplot(
  survey_long %>% filter(!is.na(listening_freq), !is.na(age_range)),
  aes(x = valence_num, y = emotional_num,
      color = listening_freq, shape = age_range)
) +
  geom_jitter(alpha = 0.7, size = 3.5, width = 0.15, height = 0.15) +
  scale_color_manual(values = palette_listening_freq) +
  scale_shape_manual(values = c(16, 17, 15, 18)) +
  labs(
    title = "Emotional Response Patterns by Demographics",
    subtitle = "Color = Listening Frequency, Shape = Age Range",
    x = "Valence",
    y = "Emotional Intensity",
    color = "Listening Freq",
    shape = "Age"
  ) +
  theme(legend.box = "vertical")

save_plot(p_scatter_dual_group, "03_scatter_dual_group.png", plots_multivariate)

# ============================================================
# B. FACETED PLOTS
# ============================================================

# Plot 4: Faceted by Song - Emotional vs Valence
p_facet_by_song <- ggplot(
  survey_long,
  aes(x = valence_num, y = emotional_num, color = listening_freq)
) +
  geom_jitter(alpha = 0.6, size = 2, width = 0.2, height = 0.2) +
  facet_wrap(~ song, ncol = 3) +
  scale_color_manual(values = palette_listening_freq, na.value = "gray50") +
  labs(
    title = "Emotional Intensity vs. Valence Across All Songs",
    x = "Valence",
    y = "Emotional Intensity",
    color = "Listening Freq"
  )

save_plot(p_facet_by_song, "04_facet_by_song.png", plots_multivariate, width = 12, height = 8)

# Plot 5: Faceted density plots by training
p_facet_density <- ggplot(
  survey_long %>% filter(!is.na(training_ordered)),
  aes(x = emotional_num, fill = training_ordered)
) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ song, ncol = 2) +
  scale_fill_brewer(palette = "Blues") +
  labs(
    title = "Emotional Intensity Distributions by Musical Training",
    x = "Emotional Intensity",
    y = "Density",
    fill = "Years Training"
  )

save_plot(p_facet_density, "05_facet_density_training.png", plots_multivariate, height = 12)

# ============================================================
# C. GROUPED BAR CHARTS
# ============================================================

# Prepare summary for bar charts
bar_summary <- survey_long %>%
  filter(!is.na(listening_freq)) %>%
  group_by(song, listening_freq) %>%
  summarize(
    mean_emotional = mean(emotional_num, na.rm = TRUE),
    se_emotional = sd(emotional_num, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

# Plot 6: Grouped bar chart with error bars
p_grouped_bar <- ggplot(
  bar_summary,
  aes(x = song, y = mean_emotional, fill = listening_freq)
) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(
    aes(ymin = mean_emotional - se_emotional,
        ymax = mean_emotional + se_emotional),
    position = position_dodge(width = 0.8),
    width = 0.25
  ) +
  scale_fill_manual(values = palette_listening_freq) +
  labs(
    title = "Mean Emotional Intensity by Song and Listening Frequency",
    subtitle = "Error bars show standard error",
    x = "Song",
    y = "Mean Emotional Intensity",
    fill = "Listening Frequency"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

save_plot(p_grouped_bar, "06_grouped_bar_emotional.png", plots_multivariate)

# Plot 7: Stacked bar chart for element influence
element_summary <- survey_long %>%
  filter(!is.na(element)) %>%
  count(song, element) %>%
  group_by(song) %>%
  mutate(pct = n / sum(n) * 100)

p_stacked_bar <- ggplot(
  element_summary,
  aes(x = song, y = pct, fill = element)
) +
  geom_col(position = "stack") +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Which Element Influenced Emotion Most?",
    subtitle = "Percentage breakdown by song",
    x = "Song",
    y = "Percentage",
    fill = "Element"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

save_plot(p_stacked_bar, "07_stacked_bar_elements.png", plots_multivariate)

# ============================================================
# D. BOX PLOTS BY GROUP
# ============================================================

# Plot 8: Box plot by listening frequency with jittered points
p_box_listening <- ggplot(
  survey_long %>% filter(!is.na(listening_freq)),
  aes(x = listening_freq, y = emotional_num, fill = listening_freq)
) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(alpha = 0.3, width = 0.2, size = 1.5) +
  scale_fill_manual(values = palette_listening_freq) +
  labs(
    title = "Emotional Intensity by Listening Frequency",
    x = "Listening Frequency",
    y = "Emotional Intensity"
  ) +
  theme(legend.position = "none")

save_plot(p_box_listening, "08_box_listening_freq.png", plots_multivariate, width = 8, height = 6)

# Plot 9: Box plot faceted by song, grouped by age
p_box_age_song <- ggplot(
  survey_long %>% filter(!is.na(age_range)),
  aes(x = age_range, y = valence_num, fill = age_range)
) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap(~ song, ncol = 3) +
  scale_fill_manual(values = palette_age) +
  labs(
    title = "Valence Ratings by Age Range Across Songs",
    x = "Age Range",
    y = "Valence"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    legend.position = "none"
  )

save_plot(p_box_age_song, "09_box_age_song.png", plots_multivariate, width = 12, height = 8)

# Plot 10: Comparative box plots (multiple DVs side by side)
comparison_long <- survey_long %>%
  filter(!is.na(listening_freq)) %>%
  select(response_id, song, listening_freq,
         emotional_num, familiarity_num, valence_num, comprehension_num) %>%
  pivot_longer(
    cols = ends_with("_num"),
    names_to = "measure",
    values_to = "score"
  ) %>%
  mutate(
    measure = case_match(
      measure,
      "emotional_num" ~ "Emotional\nIntensity",
      "familiarity_num" ~ "Familiarity",
      "valence_num" ~ "Valence",
      "comprehension_num" ~ "Comprehension"
    )
  )

p_box_comparison <- ggplot(
  comparison_long,
  aes(x = measure, y = score, fill = listening_freq)
) +
  geom_boxplot(alpha = 0.7, position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = palette_listening_freq) +
  labs(
    title = "All Measures Compared by Listening Frequency",
    x = "Measure",
    y = "Score",
    fill = "Listening Frequency"
  )

save_plot(p_box_comparison, "10_box_comparison.png", plots_multivariate)

# Plot 11: Memory trigger by song and listening frequency
memory_summary <- survey_long %>%
  filter(!is.na(memory), !is.na(listening_freq)) %>%
  count(song, listening_freq, memory) %>%
  group_by(song, listening_freq) %>%
  mutate(pct = n / sum(n) * 100)

p_memory_stacked <- ggplot(
  memory_summary,
  aes(x = song, y = pct, fill = memory)
) +
  geom_col(position = "stack") +
  facet_wrap(~ listening_freq, ncol = 1) +
  scale_fill_manual(values = c("Yes" = "#2E86AB", "No" = "#F18F01", "Maybe" = "#A23B72")) +
  labs(
    title = "Did Songs Trigger Personal Memories?",
    subtitle = "By listening frequency group",
    x = "Song",
    y = "Percentage",
    fill = "Memory Triggered"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

save_plot(p_memory_stacked, "11_memory_trigger.png", plots_multivariate, height = 10)

cat("\n=== Multivariate plots complete! ===\n")
cat("Output folder:", plots_multivariate, "\n")
