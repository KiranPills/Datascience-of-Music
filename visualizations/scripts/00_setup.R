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

# Color Palettes

palette_listening_freq <- c(
  "Daily" = "#2E86AB",
  "4-6 times a week" = "#A23B72",
  "Less than weekly" = "#F18F01"
)

palette_musical_training <- c(
  "0" = "#E8E8E8",
  "1-2" = "#B8D4E3",
  "3-5" = "#7FB3D5",
  "6+" = "#2874A6"
)

palette_age <- c(
  "18-20" = "#F4D35E",
  "21-25" = "#EE964B",
  "26-30" = "#F95738",
  "41+" = "#083D77"
)

palette_language <- c(
  "English only" = "#2E86AB",
  "English + French" = "#A23B72",
  "English + Spanish" = "#F18F01"
)

palette_songs <- viridis::viridis(6, option = "D")

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
