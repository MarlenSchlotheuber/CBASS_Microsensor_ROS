# Load packages ----
library(here)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggtext)

# Load all datasets ----
# H2O2
files_h2o2 <- list.files(paste0("data/cleaned_data/h2o2_no_interferences/"))
h2o2 <- lapply(files_h2o2, function(x)
  readr::read_csv(paste0("data/cleaned_data/h2o2_no_interferences/", x)))
files_h2o2 <- gsub("_h2o2_clean.csv", "", files_h2o2)
names(h2o2) <- files_h2o2
# light
files_light <- list.files(paste0("data/cleaned_data/light/"))
light <- lapply(files_light, function(x)
  readr::read_csv(paste0("data/cleaned_data/light/", x)))
files_light <- gsub("_light.csv", "", files_light)
names(light) <- files_light
# temperature
files_temp <- list.files(paste0("data/cleaned_data/temperature/"))
temp <- lapply(files_temp, function(x)
  readr::read_csv(paste0("data/cleaned_data/temperature/", x)))
files_temp <- gsub("_temp.csv", "", files_temp)
names(temp) <- files_temp

# Join data
data <- lapply(1:16, function(x)
  h2o2[[x]] %>%
    full_join(light[[x]] %>%
                dplyr::select(light, hour, minute, second, day_sec, treatment, fragment)) %>%
    full_join(temp[[x]] %>%
                dplyr::select(temperature, hour, minute, second, day_sec, treatment, fragment)) %>%
    arrange(day_sec)
  )

# Plotting function
plot_combined <- function(data) {

  # x limits (11am to 8am)
  x_min <- 11*60*60
  x_max <- (8*60*60) + (24*60*60)

  # Filter data
  data <- data %>% filter(day_sec >= x_min & day_sec <= x_max)

  # Create data frame for x axis breaks
  x_breaks <- data %>%
    filter(minute == 0 & second == 0) %>%
    filter(hour %in% c(0, 2*(1:11))) %>%
    select(day_sec, hour) %>%
    unique()

  # Get max values to adjust scales
  max_h2o2 <- max(data$h2o2_conc, na.rm = TRUE)
  max_t <- 40 # set to 40 so that all plots have same scale
  max_light <- max(data$light, na.rm = TRUE)

  # data frame for annotating light values
  df_light <- tibble(x = c(15*60*60,
                           19*60*60 + 15*60,
                           24*60*60 + 2*60*60,
                           24*60*60 + 7*60*60 + 30*60),
                     value = c(138, 69, 0, 138),
                     y = (value)*max_h2o2/max_light)

  # Plot
  p <- ggplot(data, aes(x = day_sec)) +
    geom_line(aes(y = (temperature-25)*max_h2o2/max_t*3)) +
    geom_line(aes(y = h2o2_conc), color = "firebrick") +
    geom_line(aes(y = (light+10)*max_h2o2/max_light), linetype = "dotted") +# add 10 for annotations
    scale_x_continuous(breaks = x_breaks$day_sec, labels = x_breaks$hour) +
    scale_y_continuous(sec.axis = sec_axis(~(./max_h2o2*max_t/3)+25, name = "Temperature [&#176;C]")) +
    labs(x = "Time (h)",
         y = "H<sub>2</sub>O<sub>2</sub> [&mu;M]",
         title = paste("CBASS", data$treatment[1], "°C -", data$fragment[1])) +
    theme_bw() +
    theme(axis.title.y.left = element_markdown(),
          axis.title.y.right = element_markdown(),
          panel.grid = element_blank(),
          axis.text = element_text(color = "black"),
          plot.title = element_text(size = 12))

  p + annotate(geom = "text", x = df_light$x, y = df_light$y,
               label = df_light$value, size = 3)
}

# Plot
ggpubr::ggarrange(plotlist = lapply(c(3, 8, 12, 14),
                                    function(x) plot_combined(data[[x]])),
                  nrow = 4)
ggsave(paste0("figures/treatment_comparison.png"),
       width = 18, height = 18, unit = "cm", dpi = 600, type = "cairo")
