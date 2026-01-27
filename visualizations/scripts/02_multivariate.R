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
# B. STACKED BAR CHARTS
# ============================================================

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
# C. LANGUAGE GROUP COMPARISON (B1+ English with French/Spanish)
# ============================================================

# Define palette for language groups
palette_language <- c(
  "English only" = "#2E86AB",
  "English + French" = "#A23B72",
  "English + Spanish" = "#F18F01"
)

# Plot 12: Box plot comparing all measures by language group
language_comparison <- survey_long %>%
  filter(!is.na(language_group)) %>%
  select(response_id, song, language_group,
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

p_language_comparison <- ggplot(
  language_comparison,
  aes(x = measure, y = score, fill = language_group)
) +
  geom_boxplot(alpha = 0.7, position = position_dodge(width = 0.8)) +
  geom_point(
    position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.8),
    alpha = 0.3, size = 1.5
  ) +
  scale_fill_manual(values = palette_language) +
  labs(
    title = "Music Responses by Language Background",
    subtitle = "Comparing B1+ English speakers: English only vs. with French vs. with Spanish",
    x = "Measure",
    y = "Score",
    fill = "Language Group"
  )

save_plot(p_language_comparison, "12_language_group_comparison.png", plots_multivariate)

cat("\n=== Multivariate plots complete! ===\n")
cat("Output folder:", plots_multivariate, "\n")
