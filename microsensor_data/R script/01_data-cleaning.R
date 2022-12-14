# Load packages ----

library(here)
library(readr)
library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(parallel)

# Load all datasets ----
files_cbass <- list.files(here::here("data", "raw_data", "data_h2o2_adjusted"))
raw_data <- lapply(files_cbass, function(x)
  readr::read_csv(here::here("data", "raw_data", "data_h2o2_adjusted", x)))
files_temp <- list.files(here::here("data", "raw_data", "hobo_temp_data"))
temp <- lapply(files_temp, function(x)
  readxl::read_xlsx(here::here("data", "raw_data", "hobo_temp_data", x), sheet = "Data"))

# Add datasets names
files_cbass <- gsub(".csv", "", files_cbass)
names(raw_data) <- files_cbass
length(raw_data) # 17 datasets
files_temp <- gsub(".xlsx", "", files_temp)
names(temp) <- files_temp
length(temp) # 16 datasets - missing 39-F5

# Clean CBASS and light data ----

# Fill spaces in column names and make all lower-case
raw_data <- lapply(raw_data, function(x)
  rename_with(x, ~ tolower(gsub(" ", "_", .x, fixed = TRUE))))

# Remove empty columns
raw_data <- parallel::mclapply(raw_data, function(x)
  x %>%
    select_if(~!(all(is.na(.)) | all(. == ""))) %>%
    rename(cal = names(.)[ncol(.)])
  , mc.cores = 4)

# Separate light, CBASS data, and HOBO data
light <- lapply(raw_data, function(x) select(x, light, time))
cbass <- lapply(raw_data, function(x) select(x, time, o2_signal, h2o2_signal))

# Extract intercept and slopes for o2 and h2o2 calibration
cal <- lapply(raw_data, function(x) tibble(b0_o2 = x$cal[10],
                                           b1_o2 = x$cal[9],
                                           b0_h2o2 = x$cal[6],
                                           b1_h2o2 = x$cal[5]))

# Clean from NAs and comments
light <- lapply(light, function(x)
  x %>%
    filter(!stringr::str_detect(time, "#") & !is.na(light) & !is.na(time))
  )

cbass <- lapply(cbass, function(x)
  x %>%
    filter(!stringr::str_detect(time, "#") & !is.na(time) & !is.na(o2_signal) & !is.na(h2o2_signal))
  )

# Split time for cbass and light
cbass <- parallel::mclapply(cbass, function(x)
  x %>%
    mutate(time = as.character(time),
           hour = unlist(lapply(strsplit(time, ":"), function(x) x[1])),
           minute = unlist(lapply(strsplit(time, ":"), function(x) x[2])),
           second = unlist(lapply(strsplit(time, ":"), function(x) x[3])),
           hour = as.numeric(hour),
           minute = as.numeric(minute),
           second = as.numeric(second))
  , mc.cores = 4)

light <- parallel::mclapply(light, function(x)
  x %>%
    mutate(time = as.character(time),
           hour = unlist(lapply(strsplit(time, ":"), function(x) x[1])),
           minute = unlist(lapply(strsplit(time, ":"), function(x) x[2])),
           second = unlist(lapply(strsplit(time, ":"), function(x) x[3])),
           hour = as.numeric(hour),
           minute = as.numeric(minute),
           second = as.numeric(second))
  , mc.cores = 4)

# Add column with daily seconds
cbass <- lapply(cbass, function(x)
  x %>%
    mutate(day_sec = hour * 3600 + minute * 60 + second,
           day_sec = if_else(hour < hour[1], day_sec + 24 * 3600, day_sec))
  )

light <- lapply(light, function(x)
  x %>%
    mutate(day_sec = hour * 3600 + minute * 60 + second,
           day_sec = if_else(hour < hour[1], day_sec + 24 * 3600, day_sec))
  )

# Correct "duplicated" time points
# Sometimes the data report more than one value for a specific time point (second),
# however, in this cases a value for the second before or after is generally missing

# cbass
cbass <- lapply(cbass, function(x)
  x %>%
    mutate(interval = day_sec - lag(day_sec, default = first(day_sec) - 1),
           # 1 - when there are two values for one second, but no values for the second before, shift the first time below one second
           second = if_else(interval == 2 & lead(interval, default = last(interval)) == 0, second - 1, second),
           # correct daily seconds and re-compute interval
           day_sec = hour * 3600 + minute * 60 + second,
           day_sec = if_else(hour < hour[1], day_sec + 24 * 3600, day_sec),
           interval = day_sec - lag(day_sec, default = first(day_sec) - 1),
           # 2 - when there are two values for one second, but no values for the second after, shift the second time up one second
           second = if_else(interval == 0 & lead(interval, default = last(interval)) == 2, second + 1, second),
           # correct daily seconds and re-compute interval
           day_sec = hour * 3600 + minute * 60 + second,
           day_sec = if_else(hour < hour[1], day_sec + 24 * 3600, day_sec),
           interval = day_sec - lag(day_sec, default = first(day_sec) - 1),
           # 3 - when there are two values for one second, but no values for two seconds before, shift the second time below one second, also for the second before
           second = case_when(interval == 2 & lead(interval, default = last(interval)) == 1 & lead(interval, n = 2) == 0 ~ second - 1,
                              interval == 1 & lag(interval, default = first(interval)) == 2 & lead(interval, default = last(interval)) == 0 ~ second - 1,
                              TRUE ~ second),
           # correct daily seconds and re-compute interval
           day_sec = hour * 3600 + minute * 60 + second,
           day_sec = if_else(hour < hour[1], day_sec + 24 * 3600, day_sec),
           interval = day_sec - lag(day_sec, default = first(day_sec) - 1))
  )

# There are still a few duplicates, average o2 and h2o2 signal
cbass <- parallel::mclapply(cbass, function(x)
  x %>%
    group_by(time, hour, minute, second, day_sec) %>%
    summarise(o2_signal = mean(as.numeric(o2_signal), na.rm = TRUE),
              h2o2_signal = mean(as.numeric(h2o2_signal), na.rm = TRUE)) %>%
    ungroup() %>%
    arrange(day_sec) %>%
    # re-compute daily seconds and interval
    mutate(day_sec = hour * 3600 + minute * 60 + second,
           day_sec = if_else(hour < hour[1], day_sec + 24 * 3600, day_sec),
           interval = day_sec - lag(day_sec, default = first(day_sec) - 1))
  , mc.cores = 4)

# Now all intervals should be 1 or higher
lapply(cbass, function(x)
  x %>% filter(interval == 0)
  )
# ok

# light
light <- lapply(light, function(x)
  x %>%
    mutate(interval = day_sec - lag(day_sec, default = first(day_sec) - 1),
           # 1 - when there are two values for one second, but no values for the second before, shift the first time below one second
           second = if_else(interval == 2 & lead(interval, default = last(interval)) == 0, second - 1, second),
           # correct daily seconds and re-compute interval
           day_sec = hour * 3600 + minute * 60 + second,
           day_sec = if_else(hour < hour[1], day_sec + 24 * 3600, day_sec),
           interval = day_sec - lag(day_sec, default = first(day_sec) - 1),
           # 2 - when there are two values for one second, but no values for the second after, shift the second time up one second
           second = if_else(interval == 0 & lead(interval, default = last(interval)) == 2, second + 1, second),
           # correct daily seconds and re-compute interval
           day_sec = hour * 3600 + minute * 60 + second,
           day_sec = if_else(hour < hour[1], day_sec + 24 * 3600, day_sec),
           interval = day_sec - lag(day_sec, default = first(day_sec) - 1),
           # 3 - when there are two values for one second, but no values for two seconds before, shift the second time below one second, also for the second before
           second = case_when(interval == 2 & lead(interval, default = last(interval)) == 1 & lead(interval, n = 2) == 0 ~ second - 1,
                              interval == 1 & lag(interval, default = first(interval)) == 2 & lead(interval, default = last(interval)) == 0 ~ second - 1,
                              TRUE ~ second),
           # correct daily seconds and re-compute interval
           day_sec = hour * 3600 + minute * 60 + second,
           day_sec = if_else(hour < hour[1], day_sec + 24 * 3600, day_sec),
           interval = day_sec - lag(day_sec, default = first(day_sec) - 1))
  )

# There are still a few duplicates, average o2 and h2o2 signal
light <- parallel::mclapply(light, function(x)
  x %>%
    group_by(time, hour, minute, second, day_sec) %>%
    summarise(light = mean(as.numeric(light), na.rm = TRUE)) %>%
    ungroup() %>%
    arrange(day_sec) %>%
    # re-compute daily seconds and interval
    mutate(day_sec = hour * 3600 + minute * 60 + second,
           day_sec = if_else(hour < hour[1], day_sec + 24 * 3600, day_sec),
           interval = day_sec - lag(day_sec, default = first(day_sec) - 1))
  , mc.cores = 4)

# Now all intervals should be 1 or higher
lapply(light, function(x)
  x %>% filter(interval == 0)
)
# ok

# Compute o2 and h2o2 concentrations
# Divide deltas by interval
cbass <- lapply(1:length(cbass), function(x)
  bind_cols(cbass[[x]], cal[[x]]) %>%
    mutate(o2_signal = as.numeric(o2_signal),
           h2o2_signal = as.numeric(h2o2_signal),
           o2_conc = b0_o2 + b1_o2 * o2_signal,
           h2o2_conc = b0_h2o2 + b1_h2o2 * h2o2_signal)
  )
names(cbass) <- files_cbass

# Add columns with treatment and fragment
cbass <- lapply(1:length(cbass), function(x)
  cbass[[x]] %>%
    mutate(treatment = unlist(lapply(strsplit(files_cbass[x], "_"), function(x) x[2])),
           fragment = unlist(lapply(strsplit(files_cbass[x], "_"), function(x) x[3])))
  )
names(cbass) <- files_cbass

light <- lapply(1:length(light), function(x)
  light[[x]] %>%
    mutate(treatment = unlist(lapply(strsplit(files_cbass[x], "_"), function(x) x[2])),
           fragment = unlist(lapply(strsplit(files_cbass[x], "_"), function(x) x[3])))
  )
names(light) <- files_cbass

# Clean temperature data ----

temp <- lapply(temp, function(x)
  x %>%
    select(-Nr) %>%
    rename(hobo_time = Date.Time, temperature = Temperature))

# Split date and time
temp <- parallel::mclapply(temp, function(x)
  x %>%
    mutate(hobo_time = as.character(hobo_time),
           date = unlist(lapply(strsplit(hobo_time, " "), function(x) x[1])),
           time = unlist(lapply(strsplit(hobo_time, " "), function(x) x[2])),
           hour = unlist(lapply(strsplit(time, ":"), function(x) x[1])),
           minute = unlist(lapply(strsplit(time, ":"), function(x) x[2])),
           second = unlist(lapply(strsplit(time, ":"), function(x) x[3])))
  , mc.cores = 4)

# Add column with daily seconds
temp <- lapply(temp, function(x)
  x %>%
    mutate(hour = as.numeric(hour),
           minute = as.numeric(minute),
           second = as.numeric(second),
           day_sec = hour * 3600 + minute * 60 + second,
           day_sec = if_else(hour < hour[1], day_sec + 24 * 3600, day_sec),
           interval = day_sec - lag(day_sec, default = first(day_sec) - 1))
  )

# Check
lapply(temp, function(x)
  x %>%
    filter(interval != 1)
  )
# ok

# Add columns with treatment and fragment
temp <- lapply(1:length(temp), function(x)
  temp[[x]] %>%
    mutate(treatment = unlist(lapply(strsplit(files_temp[x], "_"), function(x) x[2])),
           fragment = unlist(lapply(strsplit(files_temp[x], "_"), function(x) x[3])))
  )
names(temp) <- files_temp

# Save data ----
#dir.create(here::here("data", "cleaned_data"))
#dir.create(here::here("data", "cleaned_data", "h2o2"))
#dir.create(here::here("data", "cleaned_data", "light"))
#dir.create(here::here("data", "cleaned_data", "temperature"))
lapply(1:length(cbass), function(x)
  readr::write_csv(cbass[[x]],
                   here::here("data", "cleaned_data", "h2o2", paste(files_cbass[x], "h2o2.csv", sep = "_")))
  )

lapply(1:length(light), function(x)
  readr::write_csv(light[[x]],
                   here::here("data", "cleaned_data", "light", paste(files_cbass[x], "light.csv", sep = "_")))
  )

lapply(1:length(temp), function(x)
  readr::write_csv(temp[[x]],
                   here::here("data", "cleaned_data", "temperature", paste(files_temp[x], "temp.csv", sep = "_")))
  )

rm(raw_data, cal, files_temp, files_cbass, cbass, temp, light)
