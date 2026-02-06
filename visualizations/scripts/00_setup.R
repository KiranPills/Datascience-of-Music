# 00_setup.R - Package Loading and Global Theme Settings

# Core packages
library(tidyverse)
library(readxl)
library(here)

# Visualization extensions
library(ggridges)
library(ggbeeswarm)
library(corrplot)
library(viridis)
library(scales)
library(patchwork)

# Color Palettes (light-to-dark purple: weaker → stronger)

palette_listening_freq <- c(
  "Less than weekly" = "#D7B8E8",
  "4-6 times a week" = "#8E44AD",
  "Daily" = "#4A0072"
)

palette_musical_training <- c(
  "0" = "#F0E0FF",
  "1-2" = "#C89BE0",
  "3-5" = "#8E44AD",
  "6+" = "#4A0072"
)

palette_age <- c(
  "18-20" = "#F0E0FF",
  "21-25" = "#C89BE0",
  "26-30" = "#8E44AD",
  "41+" = "#4A0072"
)

palette_language <- c(
  "English only" = "#D7B8E8",
  "English + French" = "#8E44AD",
  "English + Spanish" = "#4A0072"
)

palette_english_level <- c(
  "A1" = "#F0E0FF",
  "A2" = "#D4B3F7",
  "B1" = "#B87DD8",
  "B2" = "#9B4DCA",
  "C1" = "#7B2FA0",
  "C2" = "#4A0072"
)

palette_elements <- c(
  "Lyrics" = "#4A0072",
  "Melody" = "#9B4DCA",
  "Voice/timbre" = "#D4B3F7",
  "Not sure" = "#B8A9C9",
  "Other" = "#E8BFD8"
)

palette_songs <- c(
  "#F0E0FF", "#D4B3F7", "#B87DD8",
  "#8E44AD", "#6A1B9A", "#4A0072"
)

# Global ggplot Theme

theme_music <- theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "gray40"),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold")
  )

theme_set(theme_music)

# Export Settings

export_width <- 10
export_height <- 7
export_dpi <- 300

# Project paths
project_dir <- here::here()
plots_multivariate <- file.path(project_dir, "visualizations/plots/multivariate")
plots_advanced <- file.path(project_dir, "visualizations/plots/advanced")

# Helper Functions

save_plot <- function(plot, filename, folder,
                      width = export_width,
                      height = export_height,
                      dpi = export_dpi) {
  filepath <- file.path(folder, filename)
  ggsave(filepath, plot, width = width, height = height, dpi = dpi)
  cat("Saved:", filename, "\n")
}
