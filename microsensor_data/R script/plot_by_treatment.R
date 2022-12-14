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
    arrange(day_sec) %>%
    pivot_longer(cols = c(light, o2_conc, h2o2_conc, temperature),
                 names_to = "var",
                 values_to = "value") %>%
    mutate(var = factor(var,
                        levels = c("light", "temperature", "h2o2_conc", "o2_conc"),
                        labels = c("Light [&mu;mol m<sup>-2</sup> s<sup>-1</sup>]",
                                   "Temperature [&#176;C]",
                                   "H<sub>2</sub>O<sub>2</sub> [&mu;M]",
                                   "O<sub>2</sub> [&mu;mol L<sup>-1</sup>]"))
           )
  )

# Function to make multiplot for each treatment
plot_by_treatment <- function(data) {

  # x limits (11am to 8am)
  x_min <- 11*60*60
  x_max <- (8*60*60) + (24*60*60)

  # Filter data
  data <- data %>% filter(day_sec >= x_min & day_sec <= x_max)
  #data <- data %>% filter(h2o2_conc )  # I want: take out only H2O2 data points between 18:00:00 and 18:30:00


  # Create data frame for x axis breaks
  x_breaks <- data %>%
    filter(minute == 0 & second == 0) %>% # only full hours
    filter(hour %in% c(0, 2*(1:11))) %>%
    dplyr::select(day_sec, hour) %>%
    unique()

  # Plot
  data %>%
    ggplot(aes(x = day_sec, y = value)) +
    geom_line(aes(colour = var), size = 0.5) +
    scale_x_continuous(breaks = x_breaks$day_sec, labels = x_breaks$hour) +
    facet_grid(rows = vars(var), cols = vars(fragment), scales = "free_y") +
    scale_color_manual(values = c("gold", "grey", "firebrick", "dark blue"), guide = "none") +
    labs(x = "Time (h)",
         y = "",
         title = paste("CBASS", data$treatment[1], "°C")) +
    theme_bw() +
    ylim(0,40)+
    theme(strip.text.y = element_markdown(color = "black", face = "bold"),
          strip.text.x = element_text(color = "black", face = "bold"),
          panel.grid = element_blank(),
          axis.text = element_text(color = "black"))
}

plot_by_treatment(data[[1]])

# 30
plot_by_treatment(bind_rows(data[1:4]))
ggsave(paste0("figures/cbass30.png"), width = 24, height = 18, units = "cm", dpi = 600, type = "cairo")

# 33
plot_by_treatment(bind_rows(data[5:8]))
ggsave(paste0("figures/cbass33.png"), width = 24, height = 18, units = "cm", dpi = 600, type = "cairo")

# 36
plot_by_treatment(bind_rows(data[9:12]))
ggsave(paste0("figures/cbass36_ylim40.png"), width = 24, height = 18, units = "cm", dpi = 600, type = "cairo")

# 36 F234
plot_by_treatment(bind_rows(data[10:12]))
ggsave(paste0("figures/cbass36_F234.png"), width = 24, height = 18, units = "cm", dpi = 600, type = "cairo")

# 39
plot_by_treatment(bind_rows(data[13:16]))
ggsave(paste0("figures/cbass39.png"), width = 24, height = 18, units = "cm", dpi = 600, type = "cairo")
