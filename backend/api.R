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

# Cast date, clock.in and clock,out using lubridate and dplyr
data <- data %>% mutate(date = lubridate::parse_date_time(date, orders = "dmy"),
                        clock.in = parse_date_time(clock.in, orders = "%H:%M"),
                        clock.out = parse_date_time(clock.out, orders = "%H:%M"))

# View structure of data
str(data)

# View summary of data
data %>% summarise(
  n = n(),
  mean.clock.in = mean(clock.in, na.rm = T),
  mean.clock.out = mean(clock.out, na.rm = T)
)
