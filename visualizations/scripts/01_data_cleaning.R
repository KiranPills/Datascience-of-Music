# ============================================================
# 01_data_cleaning.R - Data Import and Likert Conversions
# ============================================================

source("visualizations/scripts/00_setup.R")

# ============================================================
# Load Raw Data
# ============================================================

raw_data <- suppressMessages(read_excel(
  file.path(project_dir, "Everyday data science of music listening_January 21, 2026_09.38 5.xlsx")
))

# Filter to completed responses (Finished is a string "True" in this dataset)
survey_data <- raw_data %>%
  filter(Finished == "True" | Finished == TRUE, Progress == 100)

# ============================================================
# LIKERT CONVERSION FUNCTIONS
# ============================================================

# 7-Point Emotional Intensity / Familiarity Scale (1-7)
convert_7point_agreement <- function(x) {
  case_match(
    x,
    "Strongly agree" ~ 7,
    "Agree" ~ 6,
    "Somewhat agree" ~ 5,
    "Neither agree nor disagree" ~ 4,
    "Somewhat disagree" ~ 3,
    "Disagree" ~ 2,
    "Strongly disagree" ~ 1,
    .default = NA_real_
  )
}

# 6-Point Valence Scale (1-6)
convert_valence <- function(x) {
  case_match(
    x,
    "Very Positive" ~ 6,
    "Positive" ~ 5,
    "Slightly Positive" ~ 4,
    "Neutral" ~ 3.5,
    "Slightly Negative" ~ 3,
    "Negative" ~ 2,
    "Very Negative" ~ 1,
    .default = NA_real_
  )
}

# 5-Point Lyric Comprehension Scale (1-5)
convert_comprehension <- function(x) {
  case_match(
    x,
    "I understood all" ~ 5,
    "I understood most" ~ 4,
    "I understood some" ~ 3,
    "I understood a little" ~ 2,
    "I did not understand any lyrics" ~ 1,
    .default = NA_real_
  )
}

# English Proficiency (simplified)
convert_english_level <- function(x) {
  case_when(
    str_detect(x, "^A1") ~ "A1",
    str_detect(x, "^A2") ~ "A2",
    str_detect(x, "^B1") ~ "B1",
    str_detect(x, "^B2") ~ "B2",
    str_detect(x, "^C1") ~ "C1",
    str_detect(x, "^C2") ~ "C2",
    TRUE ~ NA_character_
  )
}

# Musical Training ordering
order_musical_training <- function(x) {
  factor(x, levels = c("0", "1-2", "3-5", "6+"), ordered = TRUE)
}

# ============================================================
# RESHAPE DATA: Wide to Long Format for Songs
# ============================================================

song_names <- c("Song 1", "Song 2", "Song 3", "Song 4", "Song 5", "Song 6")

# The columns are renamed by R with ...N suffixes
# Song 1: columns 25-32 (no suffix in original, but R adds ...25, etc.)
# Song 2: columns 33-40
# etc.

# Use column indices for selection, more robust
survey_long <- survey_data %>%
  select(
    response_id = `Response ID`,
    # Demographics
    listening_freq = `How often do you listen to music?`,
    musical_training = `How many years of formal musical training do you have?`,
    english_level = `What is your English listening level? Choose the option that fits you best.`,
    age_range = `What is your age range?`,
    # Song 1 (columns 25-31)
    emotional_1 = 25,
    familiarity_1 = 26,
    valence_1 = 27,
    comprehension_1 = 28,
    element_1 = 29,
    memory_1 = 31,
    # Song 2 (columns 33-39)
    emotional_2 = 33,
    familiarity_2 = 34,
    valence_2 = 35,
    comprehension_2 = 36,
    element_2 = 37,
    memory_2 = 39,
    # Song 3 (columns 41-47)
    emotional_3 = 41,
    familiarity_3 = 42,
    valence_3 = 43,
    comprehension_3 = 44,
    element_3 = 45,
    memory_3 = 47,
    # Song 4 (columns 49-55)
    emotional_4 = 49,
    familiarity_4 = 50,
    valence_4 = 51,
    comprehension_4 = 52,
    element_4 = 53,
    memory_4 = 55,
    # Song 5 (columns 57-63)
    emotional_5 = 57,
    familiarity_5 = 58,
    valence_5 = 59,
    comprehension_5 = 60,
    element_5 = 61,
    memory_5 = 63,
    # Song 6 (columns 65-71)
    emotional_6 = 65,
    familiarity_6 = 66,
    valence_6 = 67,
    comprehension_6 = 68,
    element_6 = 69,
    memory_6 = 71
  ) %>%
  # Pivot to long format
  pivot_longer(
    cols = matches("_(1|2|3|4|5|6)$"),
    names_to = c(".value", "song_num"),
    names_pattern = "(.+)_(\\d)"
  ) %>%
  # Apply conversions
  mutate(
    song = factor(paste("Song", song_num), levels = song_names),
    emotional_num = convert_7point_agreement(emotional),
    familiarity_num = convert_7point_agreement(familiarity),
    valence_num = convert_valence(valence),
    comprehension_num = convert_comprehension(comprehension),
    english_simple = convert_english_level(english_level),
    training_ordered = order_musical_training(musical_training),
    # Factor ordering for plots
    listening_freq = factor(listening_freq,
                            levels = c("Daily", "4-6 times a week", "Less than weekly")),
    age_range = factor(age_range,
                       levels = c("18-20", "21-25", "26-30", "41+")),
    english_simple = factor(english_simple,
                            levels = c("A1", "A2", "B1", "B2", "C1", "C2"))
  )

# ============================================================
# Summary Tables
# ============================================================

# Participant-level summary (means across songs)
participant_summary <- survey_long %>%
  group_by(response_id, listening_freq, musical_training, training_ordered,
           english_level, english_simple, age_range) %>%
  summarize(
    mean_emotional = mean(emotional_num, na.rm = TRUE),
    mean_familiarity = mean(familiarity_num, na.rm = TRUE),
    mean_valence = mean(valence_num, na.rm = TRUE),
    mean_comprehension = mean(comprehension_num, na.rm = TRUE),
    .groups = "drop"
  )

# Song-level summary
song_summary <- survey_long %>%
  group_by(song) %>%
  summarize(
    mean_emotional = mean(emotional_num, na.rm = TRUE),
    sd_emotional = sd(emotional_num, na.rm = TRUE),
    mean_familiarity = mean(familiarity_num, na.rm = TRUE),
    sd_familiarity = sd(familiarity_num, na.rm = TRUE),
    mean_valence = mean(valence_num, na.rm = TRUE),
    sd_valence = sd(valence_num, na.rm = TRUE),
    mean_comprehension = mean(comprehension_num, na.rm = TRUE),
    sd_comprehension = sd(comprehension_num, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

cat("Data cleaning complete!\n")
cat("- survey_long:", nrow(survey_long), "rows (participants x songs)\n")
cat("- participant_summary:", nrow(participant_summary), "rows\n")
cat("- song_summary:", nrow(song_summary), "rows\n")
