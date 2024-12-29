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
notes.options <- unique(data.tidy$notes)[-1]

# Field-friendly names
notes.options.field.names <- gsub(" ", "_", notes.options)

data.tidy <- data.tidy %>% 
  mutate(across(all_of(notes.options), ~
                  if_else(notes == ., TRUE, FALSE),
                .names = "{.col}"))
data.tidy

data.tidy <- data.tidy %>% 
  mutate(wfh = if_else(notes == "Work from home", TRUE, FALSE))
data.tidy

# View structure of data
str(data)

# View summary of data
data %>% summarise(
  n = n(),
  mean.clock.in = mean(clock.in, na.rm = T),
  mean.clock.out = mean(clock.out, na.rm = T)
)
