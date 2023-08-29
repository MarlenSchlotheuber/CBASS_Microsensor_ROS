# Load packages ----

library(here)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(ggtext)

# Load all datasets ----
files <- list.files(here::here("data", "cleaned_data", "h2o2"))
microsensor <- lapply(files, function(x)
  readr::read_csv(here::here("data", "cleaned_data", "h2o2", x)))
files <- gsub(".csv", "", files)
names(microsensor) <- files

# Function to find interferences
find_interf <- function(data) {

  # Compute rate of change and delta rate of change
  df <- data %>%
    mutate(interval = day_sec - dplyr::lag(day_sec, default = first(day_sec) - 1),
           d_h2o2 = (h2o2_conc - dplyr::lag(h2o2_conc, n = 1, default = first(h2o2_conc))) / interval,
           d_d_h2o2 = abs(d_h2o2 - dplyr::lag(d_h2o2, n = 1, default = first(d_h2o2))))

  # Identify interferences and shift by one second
  df <- df %>%
    mutate(int = if_else(d_d_h2o2 > 5, 1, 0),
           int = if_else(lead(int) == 1, 1, 0)
           )

  # Interference also all data after actual interference,
  # until values goes back to +5 from the median of the 10 seconds before
  df <- df %>%
    mutate(lag_median = apply(sapply(1:10, dplyr::lag, x = h2o2_conc), 1, median, na.rm = TRUE),
           ref_h2o2 = if_else(int == 1, lag_median, NULL)) %>%
    tidyr::fill(ref_h2o2) %>%
    mutate(int2 = case_when(h2o2_conc > (ref_h2o2 + 5.01) ~ 1,
                            int == 1 ~ 1,
                            TRUE ~ 0)) %>%
    tidyr::replace_na(list(int2 = 0)) %>%
    mutate(int2 = if_else(int == int2, int, NULL)) %>%
    tidyr::fill(int2)

  # Remove unnecessary columns
  df %>%
    select(-d_h2o2, -d_d_h2o2, -int, -lag_median, -ref_h2o2) %>%
    rename(interference = int2)
}

# Apply to all datasets (twice)
microsensor1 <- lapply(microsensor, find_interf)
microsensor1 <- lapply(microsensor1, function(x)
  x %>%
    filter(interference == 0) %>%
    select(-interference))
microsensor2 <- lapply(microsensor1, find_interf)
microsensor2 <- lapply(microsensor2, function(x)
  x %>%
    filter(interference == 0) %>%
    select(-interference))

# Make plots to compare raw and clean data
plots <- lapply(1:length(microsensor), function(x) {
  df <- microsensor[[x]]
  df1 <- microsensor1[[x]]
  df2 <- microsensor2[[x]]
  # data frame for x axis breaks
  x_breaks <- df %>%
    filter(minute == 0 & second == 0) %>% # only full hours
    filter(hour %in% c(0, 2 * (1:11)))
  # plotting function
  p <- function(df, subtitle) {
    ggplot(df, aes(x = day_sec, y = h2o2_conc)) +
      geom_line(size = 0.5) +
      scale_x_continuous(breaks = x_breaks$day_sec, labels = x_breaks$hour) +
      theme_classic() +
      labs(x = "Time (h)",
           y = "H<sub>2</sub>O<sub>2</sub> (&mu;M)",
           title = paste("T =", df$treatment[1], "°C -", gsub("_.*", "", df$fragment[1])),
           subtitle = subtitle) +
      theme(axis.title.y = element_markdown())
  }
  # plot
  p(df, "Raw data") + p(df1, "First clean") + p(df2, "Second clean")
  })

#dir.create(here::here("figures"))
plots[[1]] / plots[[2]] / plots[[3]] / plots[[4]]
ggsave(here::here("figures", "clean_cbass30.png"),
       width = 22, height = 20, unit = "cm", dpi = 600, type = "cairo")
ggsave(here::here("figures", "clean_cbass30.pdf"),
       width = 22, height = 20, unit = "cm", device = cairo_pdf)

plots[[5]] / plots[[6]] / plots[[7]] / plots[[8]]
ggsave(here::here("figures", "clean_cbass33.png"),
       width = 22, height = 20, unit = "cm", dpi = 600, type = "cairo")
ggsave(here::here("figures", "clean_cbass33.pdf"),
       width = 22, height = 20, unit = "cm", device = cairo_pdf)

plots[[9]] / plots[[10]] / plots[[11]] / plots[[12]]
ggsave(here::here("figures", "clean_cbass36.png"),
       width = 22, height = 20, unit = "cm", dpi = 600, type = "cairo")
ggsave(here::here("figures", "clean_cbass36.pdf"),
       width = 22, height = 20, unit = "cm", device = cairo_pdf)

plots[[13]] / plots[[14]] / plots[[15]] / plots[[16]]
ggsave(here::here("figures", "clean_cbass39.png"),
       width = 22, height = 20, unit = "cm", dpi = 600, type = "cairo")
ggsave(here::here("figures", "clean_cbass39.pdf"),
       width = 22, height = 20, unit = "cm", device = cairo_pdf)

# Save data ----
#dir.create(here::here("data", "cleaned_data", "h2o2_no_interferences"))
lapply(1:length(microsensor2), function(x)
  readr::write_csv(microsensor2[[x]],
                   here::here("data", "cleaned_data", "h2o2_no_interferences",
                              paste(files[x], "clean.csv", sep = "_")))
  )

rm(microsensor, microsensor1, microsensor2, files, find_interf, plots)
