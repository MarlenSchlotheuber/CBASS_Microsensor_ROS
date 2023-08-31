# Load packages ----

library(here)
library(readr)
library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(parallel)

# Load all datasets ----
# cbass
files_cbass <- list.files(here::here("data", "raw_data", "data_h2o2"))
cbass <- lapply(files_cbass, function(x)
  readr::read_csv(here::here("data", "raw_data", "data_h2o2", x)))
# temperature
files_temp <- list.files(here::here("data", "raw_data", "hobo_temp_data"))
temp <- lapply(files_temp, function(x)
  readxl::read_xlsx(here::here("data", "raw_data", "hobo_temp_data", x), sheet = "Data"))
# pam, vba and cell count
files_pam_vba_cc <- list.files(here::here("data", "raw_data", "pam_vba_cellcount"))
pam_vba_cc <- lapply(files_pam_vba_cc, function(x)
  readr::read_delim(here::here("data", "raw_data", "pam_vba_cellcount", x)))

# Add datasets names
files_cbass <- gsub(".csv", "", files_cbass)
names(cbass) <- files_cbass
length(cbass) # 17 datasets
files_temp <- gsub(".xlsx", "", files_temp)
names(temp) <- files_temp
length(temp) # 17 datasets
files_pam_vba_cc <- gsub(".csv", "", files_pam_vba_cc)
names(pam_vba_cc) <- files_pam_vba_cc
length(pam_vba_cc) # 5 datasets

# Clean microsensor and light data ----

# Fill spaces in column names and make all lower-case
cbass <- lapply(cbass, function(x)
  rename_with(x, ~ tolower(gsub(" ", "_", .x, fixed = TRUE))))

# Remove empty columns
cbass <- parallel::mclapply(cbass, function(x)
  x %>%
    select_if(~!(all(is.na(.)) | all(. == ""))) %>%
    rename(cal = names(.)[ncol(.)])
  , mc.cores = 4)

# Separate light and microsensor data
light <- lapply(cbass, function(x) select(x, light, time))
microsensor <- lapply(cbass, function(x) select(x, time, o2_signal, h2o2_signal))

# Extract intercept and slopes for o2 and h2o2 calibration
cal <- lapply(cbass, function(x) tibble(b0_o2 = x$cal[10],
                                        b1_o2 = x$cal[9],
                                        b0_h2o2 = x$cal[6],
                                        b1_h2o2 = x$cal[5],
                                        # additional parameters to correct drift in 39-F3
                                        b0_drift_slope = x$cal[14],
                                        b1_drift_slope = x$cal[13],
                                        b0_drift_intercept = x$cal[18],
                                        b1_drift_intercept = x$cal[17])
              )

# Clean from NAs and comments
light <- lapply(light, function(x)
  x %>%
    filter(!stringr::str_detect(time, "#") & !is.na(light) & !is.na(time))
  )

microsensor <- lapply(microsensor, function(x)
  x %>%
    filter(!stringr::str_detect(time, "#") & !is.na(time) & !is.na(o2_signal) & !is.na(h2o2_signal))
  )

# Split time for microsensor and light
microsensor <- parallel::mclapply(microsensor, function(x)
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
microsensor <- lapply(microsensor, function(x)
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

# microsensor
microsensor <- lapply(microsensor, function(x)
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
microsensor <- parallel::mclapply(microsensor, function(x)
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
lapply(microsensor, function(x)
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
microsensor <- lapply(1:length(microsensor), function(x)
  bind_cols(microsensor[[x]], cal[[x]]) %>%
    mutate(o2_signal = as.numeric(o2_signal),
           h2o2_signal = as.numeric(h2o2_signal),
           o2_conc = b0_o2 + b1_o2 * o2_signal,
           # Correct drift in 39-F3
           b0_h2o2 = if_else(is.na(b0_drift_intercept),
                             b0_h2o2,
                             b0_drift_intercept + b1_drift_intercept * day_sec),
           b1_h2o2 = if_else(is.na(b0_drift_slope),
                             b1_h2o2,
                             b0_drift_slope + b1_drift_slope * day_sec),
           h2o2_conc = b0_h2o2 + b1_h2o2 * h2o2_signal) %>%
    select(-c(b0_drift_intercept, b1_drift_intercept, b0_drift_slope, b1_drift_slope))
  )
names(microsensor) <- files_cbass

# Add columns with treatment and fragment
microsensor <- lapply(1:length(microsensor), function(x)
  microsensor[[x]] %>%
    mutate(treatment = unlist(lapply(strsplit(files_cbass[x], "_"), function(x) x[2])),
           fragment = unlist(lapply(strsplit(files_cbass[x], "_"), function(x) x[3])))
  )
names(microsensor) <- files_cbass

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

# Clean pam, vba and cellcount data----

# Fill spaces in column names and make all lower-case
pam_vba_cc <- lapply(pam_vba_cc, function(x)
  rename_with(x, ~ tolower(gsub(" ",
                                "_",
                                gsub(".", "_", .x, fixed = TRUE),
                                fixed = TRUE))))

names(pam_vba_cc) <- files_pam_vba_cc

# Save data ----
#dir.create(here::here("data", "cleaned_data"))
#dir.create(here::here("data", "cleaned_data", "h2o2"))
#dir.create(here::here("data", "cleaned_data", "light"))
#dir.create(here::here("data", "cleaned_data", "temperature"))
#dir.create(here::here("data", "cleaned_data", "pam_vba_cellcount"))

lapply(1:length(microsensor), function(x)
  readr::write_csv(microsensor[[x]],
                   here::here("data", "cleaned_data", "h2o2",
                              paste(files_cbass[x], "h2o2.csv", sep = "_")))
  )

lapply(1:length(light), function(x)
  readr::write_csv(light[[x]],
                   here::here("data", "cleaned_data", "light",
                              paste(files_cbass[x], "light.csv", sep = "_")))
  )

lapply(1:length(temp), function(x)
  readr::write_csv(temp[[x]],
                   here::here("data", "cleaned_data", "temperature",
                              paste(files_temp[x], "temp.csv", sep = "_")))
  )

lapply(1:length(pam_vba_cc), function(x)
  readr::write_csv(pam_vba_cc[[x]],
                   here::here("data", "cleaned_data", "pam_vba_cellcount",
                              paste0(files_pam_vba_cc[x], ".csv")))
  )

rm(cbass, pam_vba_cc, cal, files_temp, files_cbass, files_pam_vba_cc, microsensor, temp, light)
