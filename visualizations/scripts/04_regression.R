# 04_regression.R - Regression Analysis: Comprehension Effects

source("visualizations/scripts/01b_load_cleaned_data.R")

output_dir <- file.path(project_dir, "selected_plots")

# Helper function for regression plots
create_regression_plot <- function(data, x_var, y_var, x_label, y_label,
                                   title, subtitle, point_color,
                                   x_breaks, y_breaks, x_limits, y_limits,
                                   label_x, label_y) {
  model <- lm(as.formula(paste(y_var, "~", x_var)), data = data)
  intercept <- coef(model)[1]
  slope <- coef(model)[2]
  r_squared <- summary(model)$r.squared
  p_value <- summary(model)$coefficients[2, 4]

  label <- sprintf("y = %.2f + %.2fx\nR² = %.3f, p = %.3f",
                   intercept, slope, r_squared, p_value)

  ggplot(data, aes(x = .data[[x_var]], y = .data[[y_var]])) +
    geom_jitter(width = 0.15, height = 0.15, alpha = 0.6, color = point_color, size = 2) +
    geom_abline(intercept = intercept, slope = slope,
                color = "#4B0082", linewidth = 1.2) +
    annotate("text", x = label_x, y = label_y, label = label,
             hjust = 0, size = 4, color = "gray30") +
    scale_x_continuous(breaks = x_breaks, limits = x_limits) +
    scale_y_continuous(breaks = y_breaks, limits = y_limits) +
    labs(title = title, subtitle = subtitle, x = x_label, y = y_label) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(color = "gray40"),
      panel.grid.minor = element_blank()
    )
}

# Chart 1: Comprehension vs Emotional Intensity
p1 <- create_regression_plot(
  data = survey_long,
  x_var = "comprehension_num",
  y_var = "emotional_num",
  x_label = "Lyric Comprehension (1-5)",
  y_label = "Emotional Intensity (1-7)",
  title = "Lyric Comprehension vs Emotional Intensity",
  subtitle = "Does understanding lyrics affect emotional response strength?",
  point_color = "#7B68EE",
  x_breaks = 1:5, y_breaks = 1:7,
  x_limits = c(0.5, 5.5), y_limits = c(0.5, 7.5),
  label_x = 1.5, label_y = 6.8
)

ggsave(file.path(output_dir, "13_regression_comprehension_emotion.png"),
       p1, width = 8, height = 6, dpi = 300)
cat("Saved: 13_regression_comprehension_emotion.png\n")

# Chart 2: Comprehension vs Valence
p2 <- create_regression_plot(
  data = survey_long,
  x_var = "comprehension_num",
  y_var = "valence_num",
  x_label = "Lyric Comprehension (1-5)",
  y_label = "Valence (1-6)",
  title = "Lyric Comprehension vs Emotional Valence",
  subtitle = "Does understanding lyrics affect positivity of response?",
  point_color = "#9370DB",
  x_breaks = 1:5, y_breaks = 1:6,
  x_limits = c(0.5, 5.5), y_limits = c(0.5, 6.5),
  label_x = 1.5, label_y = 5.8
)

ggsave(file.path(output_dir, "14_regression_comprehension_valence.png"),
       p2, width = 8, height = 6, dpi = 300)
cat("Saved: 14_regression_comprehension_valence.png\n")
