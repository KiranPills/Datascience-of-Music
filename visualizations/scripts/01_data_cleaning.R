# 01_data_cleaning.R - Data Import and Likert Conversions

source("visualizations/scripts/00_setup.R")

# Load Raw Data

csv_path <- file.path(project_dir, "Everyday data science of music listening_February 4, 2026_07.43.csv")

# Read row 2 for column names (Qualtrics format)
header_row <- read_csv(csv_path, skip = 1, n_max = 1, col_names = FALSE, show_col_types = FALSE)
col_names <- as.character(header_row[1, ])

# Read data starting from row 4
raw_data <- suppressMessages(read_csv(
  csv_path,
  skip = 3,
  col_names = col_names
))

# Filter to completed responses
survey_data <- raw_data %>%
  filter(Finished == 1, Progress == 100)

# Numeric Code Mappings

# Listening frequency: 1=Daily, 2=4-6 times a week, 5=Less than weekly
map_listening_freq <- function(x) {
  case_match(
    as.integer(x),
    1L ~ "Daily",
    2L ~ "4-6 times a week",
    5L ~ "Less than weekly",
    .default = NA_character_
  )
}

# Musical training: 1=0, 2=1-2, 3=3-5, 4=6+
map_musical_training <- function(x) {
  case_match(
    as.integer(x),
    1L ~ "0",
    2L ~ "1-2",
    3L ~ "3-5",
    4L ~ "6+",
    .default = NA_character_
  )
}

# English level: 1=A1, 2=A2, 3=B1, 4=B2, 5=C1, 6=C2
map_english_level <- function(x) {
  case_match(
    as.integer(x),
    1L ~ "A1",
    2L ~ "A2",
    3L ~ "B1",
    4L ~ "B2",
    5L ~ "C1",
    6L ~ "C2",
    .default = NA_character_
  )
}

# Age range: 1=18-20, 2=21-25, 3=26-30, 4=31-40, 6=41+
map_age_range <- function(x) {
  case_match(
    as.integer(x),
    1L ~ "18-20",
    2L ~ "21-25",
    3L ~ "26-30",
    4L ~ "31-40",
    6L ~ "41+",
    .default = NA_character_
  )
}

# Yes/No: 1=Yes, 2=No
map_yes_no <- function(x) {
  case_match(
    as.integer(x),
    1L ~ "Yes",
    2L ~ "No",
    .default = NA_character_
  )
}

# Element: 1=Melody, 2=Lyrics, 3=Rhythm/beat, 5=Other
map_element <- function(x) {
  case_match(
    as.integer(x),
    1L ~ "Melody",
    2L ~ "Lyrics",
    3L ~ "Rhythm/beat",
    5L ~ "Other",
    .default = NA_character_
  )
}

order_musical_training <- function(x) {
  factor(x, levels = c("0", "1-2", "3-5", "6+"), ordered = TRUE)
}

# Reshape Data: Wide to Long

song_names <- c(
  "Solo - Clean Bandit (Original)",
  "Solo (Spanish Cover)",
  "Lost on You - LP (Original)",
  "Lost on You (Spanish Cover)",
  "Issues - Julia Michaels (Original)",
  "Issues (French Cover)"
)

survey_long <- survey_data %>%
  select(
    response_id = `Response ID`,
    # Demographics (numeric codes)
    listening_freq_code = 19,
    musical_training_code = 20,
    english_level_code = 21,
    age_range_code = 22,
    french_b1_code = 23,
    spanish_b1_code = 24,
    # Song 1 (columns 25-31)
    emotional_1 = 25,
    familiarity_1 = 26,
    valence_1 = 27,
    comprehension_1 = 28,
    element_code_1 = 29,
    memory_1 = 31,
    # Song 2 (columns 33-39)
    emotional_2 = 33,
    familiarity_2 = 34,
    valence_2 = 35,
    comprehension_2 = 36,
    element_code_2 = 37,
    memory_2 = 39,
    # Song 3 (columns 41-47)
    emotional_3 = 41,
    familiarity_3 = 42,
    valence_3 = 43,
    comprehension_3 = 44,
    element_code_3 = 45,
    memory_3 = 47,
    # Song 4 (columns 49-55)
    emotional_4 = 49,
    familiarity_4 = 50,
    valence_4 = 51,
    comprehension_4 = 52,
    element_code_4 = 53,
    memory_4 = 55,
    # Song 5 (columns 57-63)
    emotional_5 = 57,
    familiarity_5 = 58,
    valence_5 = 59,
    comprehension_5 = 60,
    element_code_5 = 61,
    memory_5 = 63,
    # Song 6 (columns 65-71)
    emotional_6 = 65,
    familiarity_6 = 66,
    valence_6 = 67,
    comprehension_6 = 68,
    element_code_6 = 69,
    memory_6 = 71
  ) %>%
  mutate(
    listening_freq = map_listening_freq(listening_freq_code),
    musical_training = map_musical_training(musical_training_code),
    english_level = map_english_level(english_level_code),
    age_range = map_age_range(age_range_code),
    french_b1 = map_yes_no(french_b1_code),
    spanish_b1 = map_yes_no(spanish_b1_code)
  ) %>%
  select(-ends_with("_code")) %>%
  pivot_longer(
    cols = matches("_(1|2|3|4|5|6)$"),
    names_to = c(".value", "song_num"),
    names_pattern = "(.+)_(\\d)"
  ) %>%
  mutate(
    song = factor(song_names[as.integer(song_num)], levels = song_names),
    emotional_num = as.numeric(emotional),
    familiarity_num = as.numeric(familiarity),
    valence_num = as.numeric(valence),
    comprehension_num = as.numeric(comprehension),
    element = map_element(element_code),
    training_ordered = order_musical_training(musical_training),
    listening_freq = factor(listening_freq,
                            levels = c("Daily", "4-6 times a week", "Less than weekly")),
    age_range = factor(age_range,
                       levels = c("18-20", "21-25", "26-30", "31-40", "41+")),
    english_simple = factor(english_level,
                            levels = c("A1", "A2", "B1", "B2", "C1", "C2")),
    language_group = case_when(
      english_simple %in% c("B1", "B2", "C1", "C2") &
        french_b1 == "No" & spanish_b1 == "No" ~ "English only",
      english_simple %in% c("B1", "B2", "C1", "C2") &
        french_b1 == "Yes" & spanish_b1 == "No" ~ "English + French",
      english_simple %in% c("B1", "B2", "C1", "C2") &
        french_b1 == "No" & spanish_b1 == "Yes" ~ "English + Spanish",
      TRUE ~ NA_character_
    ),
    language_group = factor(language_group,
                            levels = c("English only", "English + French", "English + Spanish"))
  ) %>%
  select(-element_code)

# Summary Tables

participant_summary <- survey_long %>%
  group_by(response_id, listening_freq, musical_training, training_ordered,
           english_level, english_simple, age_range, french_b1, spanish_b1, language_group) %>%
  summarize(
    mean_emotional = mean(emotional_num, na.rm = TRUE),
    mean_familiarity = mean(familiarity_num, na.rm = TRUE),
    mean_valence = mean(valence_num, na.rm = TRUE),
    mean_comprehension = mean(comprehension_num, na.rm = TRUE),
    .groups = "drop"
  )

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
cat("- survey_long:", nrow(survey_long), "rows\n")
cat("- participant_summary:", nrow(participant_summary), "rows\n")
cat("- song_summary:", nrow(song_summary), "rows\n")

# Export Cleaned Data

data_dir <- file.path(project_dir, "visualizations/data")

write_csv(survey_long, file.path(data_dir, "survey_long_cleaned.csv"))
write_csv(participant_summary, file.path(data_dir, "participant_summary.csv"))
write_csv(song_summary, file.path(data_dir, "song_summary.csv"))

cat("Exported to visualizations/data/\n")
