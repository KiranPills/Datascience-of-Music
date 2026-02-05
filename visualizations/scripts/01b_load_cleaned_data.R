# 01b_load_cleaned_data.R - Load Pre-Cleaned Data
# Run 01_data_cleaning.R first to generate the CSV files.

source("visualizations/scripts/00_setup.R")

# Load Cleaned Data

data_dir <- file.path(project_dir, "visualizations/data")

survey_long <- read_csv(file.path(data_dir, "survey_long_cleaned.csv"), show_col_types = FALSE)
participant_summary <- read_csv(file.path(data_dir, "participant_summary.csv"), show_col_types = FALSE)
song_summary <- read_csv(file.path(data_dir, "song_summary.csv"), show_col_types = FALSE)

# Restore Factor Levels

song_names <- c(
  "Solo - Clean Bandit (Original)",
  "Solo (Spanish Cover)",
  "Lost on You - LP (Original)",
  "Lost on You (Spanish Cover)",
  "Issues - Julia Michaels (Original)",
  "Issues (French Cover)"
)

survey_long <- survey_long %>%
  mutate(
    song = factor(song, levels = song_names),
    listening_freq = factor(listening_freq,
                            levels = c("Daily", "4-6 times a week", "Less than weekly")),
    age_range = factor(age_range,
                       levels = c("18-20", "21-25", "26-30", "31-40", "41+")),
    english_simple = factor(english_simple,
                            levels = c("A1", "A2", "B1", "B2", "C1", "C2")),
    training_ordered = factor(training_ordered,
                              levels = c("0", "1-2", "3-5", "6+"), ordered = TRUE),
    language_group = factor(language_group,
                            levels = c("English only", "English + French", "English + Spanish"))
  )

participant_summary <- participant_summary %>%
  mutate(
    listening_freq = factor(listening_freq,
                            levels = c("Daily", "4-6 times a week", "Less than weekly")),
    age_range = factor(age_range,
                       levels = c("18-20", "21-25", "26-30", "31-40", "41+")),
    english_simple = factor(english_simple,
                            levels = c("A1", "A2", "B1", "B2", "C1", "C2")),
    training_ordered = factor(training_ordered,
                              levels = c("0", "1-2", "3-5", "6+"), ordered = TRUE),
    language_group = factor(language_group,
                            levels = c("English only", "English + French", "English + Spanish"))
  )

song_summary <- song_summary %>%
  mutate(
    song = factor(song, levels = song_names)
  )
