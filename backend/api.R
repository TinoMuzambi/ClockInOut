library(tidyverse)
library(lubridate)

# Read data
data <- read_csv("backend/data/data.csv", col_types = "cccc") %>% as_tibble()

# Print data
# print(data)

# Rename columns using dplyr
data <- data %>% rename(
  date = Date,
  clock.in = "Clock In Time",
  clock.out = "Clock Out Time",
  notes = Notes
)

# Add id column
data <- data %>% 
  mutate(id = rownames(data))

# Cast date, clock.in and clock,out using lubridate and dplyr
data <- data %>% 
  mutate(date = lubridate::parse_date_time(date, orders = "dmy"),
         clock.in = parse_date_time(clock.in, orders = "%H:%M"),
         clock.out = parse_date_time(clock.out, orders = "%H:%M"))

# Rename notes fields
data.tidy <- data %>% 
  mutate(notes = gsub(" ", "_", notes)) %>% 
  mutate(notes = gsub(":", "_", notes)) %>% 
  mutate(notes = gsub("-", "_", notes))

# Separate options from notes into separate columns
data.tidy <- data.tidy %>% separate_rows(notes, sep = ",_")

# List out note options
# notes.options <- unique(data.tidy$notes)[-1]
# notes.options <- c("Work_from_home", "Post_Work_Commitment",
#                    "Pre_Work_Commitment", "16_00_lecture", "14_00_lecture",
#                    "Public_Holiday", "Annual_leave", "11_00_lecture",
#                    "Study_leave", "Conference", "Sick_leave")

# data.tidy <- data.tidy %>% 
#   mutate(across(all_of(notes.options), ~
#                   if_else(notes == ., TRUE, FALSE),
#                 .names = "{.col}"))

# Create indicator columns for note options
data.tidy <- data.tidy %>% 
  mutate(note_wfh = if_else(notes == "Work_from_home", TRUE, FALSE),
         note_post.wc = if_else(notes == "Post_Work_Commitment", TRUE, FALSE),
         note_pre.wc = if_else(notes == "Pre_Work_Commitment", TRUE, FALSE),
         note_lecture.1600 = if_else(notes == "16_00_lecture", TRUE, FALSE),
         note_lecture.1400 = if_else(notes == "14_00_lecture", TRUE, FALSE),
         note_lecture.1100 = if_else(notes == "11_00_lecture", TRUE, FALSE),
         note_annual.leave = if_else(notes == "Annual_leave", TRUE, FALSE),
         note_sick.leave = if_else(notes == "Sick_leave", TRUE, FALSE),
         note_study.leave = if_else(notes == "Study_leave", TRUE, FALSE),
         note_holiday = if_else(notes == "Public_Holiday", TRUE, FALSE),
         note_conference = if_else(notes == "Conference", TRUE, FALSE),)

# View structure of data
str(data)

# View summary of data
data %>% summarise(
  n = n(),
  mean.clock.in = mean(clock.in, na.rm = T),
  mean.clock.out = mean(clock.out, na.rm = T)
)
