# Load packages ----
library(here)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggtext)

# Load all datasets ----

# H2O2
files_h2o2 <- list.files(here::here("data", "cleaned_data", "h2o2_no_interferences"))
h2o2 <- lapply(files_h2o2, function(x)
  readr::read_csv(here::here("data", "cleaned_data", "h2o2_no_interferences", x)))
files_h2o2 <- gsub("_h2o2_clean.csv", "", files_h2o2)
names(h2o2) <- files_h2o2
# light
files_light <- list.files(here::here("data", "cleaned_data", "light"))
light <- lapply(files_light, function(x)
  readr::read_csv(here::here("data", "cleaned_data", "light", x)))
files_light <- gsub("_light.csv", "", files_light)
names(light) <- files_light
# temperature
files_temp <- list.files(here::here("data", "cleaned_data", "temperature"))
temp <- lapply(files_temp, function(x)
  readr::read_csv(here::here("data", "cleaned_data", "temperature", x)))
files_temp <- gsub("_temp.csv", "", files_temp)
names(temp) <- files_temp

# Merge data ----

data <- lapply(1:length(h2o2), function(x)
  h2o2[[x]] %>%
    full_join(light[[x]] %>%
                select(light, hour, minute, second, day_sec, treatment, fragment)) %>%
    full_join(temp[[x]] %>%
                select(temperature, hour, minute, second, day_sec, treatment, fragment)) %>%
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

# Plot by treatment ----

# Function to make multiplot for each treatment
plot_by_treatment <- function(data) {

  # x limits (11am to 8am)
  x_min <- 11 * 60 * 60
  x_max <- (8 * 60 * 60) + (24 * 60 * 60)

  # Filter data
  data <- data %>% filter(between(day_sec, x_min, x_max))

  # Create data frame for x axis breaks
  x_breaks <- data %>%
    filter(minute == 0 & second == 0) %>% # only full hours
    filter(hour %in% c(0, 2 * (1:11))) %>%
    select(day_sec, hour) %>%
    unique()

  # Plot
  data %>%
    ggplot(aes(x = day_sec, y = value)) +
    geom_line(aes(colour = var), size = 0.5) +
    scale_x_continuous(breaks = x_breaks$day_sec, labels = x_breaks$hour) +
    facet_grid(rows = vars(var), cols = vars(fragment), scales = "free_y", switch = "y") +
    scale_color_manual(values = c("gold", "grey30", "firebrick", "dark blue"), guide = "none") +
    labs(x = "Time (h)",
         y = "",
         title = paste("CBASS", data$treatment[1], "&#176;C")) +
    theme_bw() +
    theme(strip.text.y.left = element_markdown(color = "black"),
          strip.text.x = element_text(color = "black", face = "bold"),
          strip.background.y = element_blank(),
          strip.placement.y = "outside",
          plot.title = element_markdown(),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(color = "grey95"),
          axis.text = element_text(size = 7))
}

# Test
plot_by_treatment(data[[1]])

# 30
plot_by_treatment(bind_rows(data[1:4]))
ggsave(here::here("figures", "cbass30.png"),
       width = 18, height = 16, units = "cm", dpi = 600, type = "cairo")
ggsave(here::here("figures", "cbass30.pdf"),
       width = 18, height = 16, units = "cm", device = cairo_pdf)

# 33
plot_by_treatment(bind_rows(data[5:8]))
ggsave(here::here("figures", "cbass33.png"),
       width = 18, height = 16, units = "cm", dpi = 600, type = "cairo")
ggsave(here::here("figures", "cbass33.pdf"),
       width = 18, height = 16, units = "cm", device = cairo_pdf)

# 36
plot_by_treatment(bind_rows(data[9:12]))
ggsave(here::here("figures", "cbass36.png"),
       width = 18, height = 16, units = "cm", dpi = 600, type = "cairo")
ggsave(here::here("figures", "cbass36.pdf"),
       width = 18, height = 16, units = "cm", device = cairo_pdf)

# 39
plot_by_treatment(bind_rows(data[13:16]))
ggsave(here::here("figures", "cbass39.png"),
       width = 18, height = 16, units = "cm", dpi = 600, type = "cairo")
ggsave(here::here("figures", "cbass39.pdf"),
       width = 18, height = 16, units = "cm", device = cairo_pdf)

# Plot across treatments ----

# Function to make multiplot for each treatment
plot_across_treatments <- function(data) {

  # x limits (11am to 8am)
  x_min <- 11 * 60 * 60
  x_max <- (8 * 60 * 60) + (24 * 60 * 60)

  # Filter data
  data <- data %>% filter(between(day_sec, x_min, x_max))

  # Create data frame for x axis breaks
  x_breaks <- data %>%
    filter(minute == 0 & second == 0) %>% # only full hours
    filter(hour %in% c(0, 2 * (1:11))) %>%
    select(day_sec, hour) %>%
    unique()

  # Plot
  data %>%
    ggplot(aes(x = day_sec, y = value)) +
    geom_line(aes(colour = var), size = 0.5) +
    scale_x_continuous(breaks = x_breaks$day_sec, labels = x_breaks$hour) +
    facet_grid(rows = vars(var), cols = vars(treatment), scales = "free_y", switch = "y") +
    scale_color_manual(values = c("gold", "grey30", "firebrick", "dark blue"), guide = "none") +
    labs(x = "Time (h)",
         y = "") +
    theme_bw() +
    theme(strip.text.y.left = element_markdown(color = "black"),
          strip.text.x = element_markdown(color = "black", face = "bold"),
          strip.background.y = element_blank(),
          strip.placement.y = "outside",
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(color = "grey95"),
          axis.text = element_text(size = 7))
}

# 30F3 - 33F4 - 36F1 - 39F1
plot_across_treatments(data[c(3, 8, 9, 13)] %>%
                         bind_rows() %>%
                         mutate(treatment = paste(treatment, "&#176;C")))
ggsave(here::here("figures", "cbass_30F3_33F4_36F1_39F1.png"),
       width = 18, height = 16, units = "cm", dpi = 600, type = "cairo")
ggsave(here::here("figures", "cbass_30F3_33F4_36F1_39F1.pdf"),
       width = 18, height = 16, units = "cm", device = cairo_pdf)

# 30F3 - 33F4 - 36F4 - 39F2
plot_across_treatments(data[c(3, 8, 12, 14)] %>%
                         bind_rows() %>%
                         mutate(treatment = paste(treatment, "&#176;C")))
ggsave(here::here("figures", "cbass_30F3_33F4_36F4_39F2.png"),
       width = 18, height = 16, units = "cm", dpi = 600, type = "cairo")
ggsave(here::here("figures", "cbass_30F3_33F4_36F4_39F2.pdf"),
       width = 18, height = 16, units = "cm", device = cairo_pdf)

# 30F3 - 33F4 - 36F4 - 39F1
plot_across_treatments(data[c(3, 8, 12, 13)] %>%
                         bind_rows() %>%
                         mutate(treatment = paste(treatment, "&#176;C")))
ggsave(here::here("figures", "cbass_30F3_33F4_36F4_39F1.png"),
       width = 18, height = 16, units = "cm", dpi = 600, type = "cairo")
ggsave(here::here("figures", "cbass_30F3_33F4_36F4_39F1.pdf"),
       width = 18, height = 16, units = "cm", device = cairo_pdf)

# Plot day vs night ----

# Function to make multiplot for each treatment
plot_day_night <- function(data) {

  # x limits (11am to 8am for day, 16pm to 14pm for night)
  x_min <- c(11 * 60 * 60, 16 * 60 * 60)
  x_max <- c((8 * 60 * 60) + (24 * 60 * 60), (14 * 60 * 60) + (24 * 60 * 60))

  # Filter data
  data <- data %>%
      filter((phase == "Day" & between(day_sec, x_min[[1]], x_max[[1]])) |
               (phase == "Night" & between(day_sec, x_min[[2]], x_max[[2]]))
  )

  # Create data frame for x axis breaks
  x_breaks <- data %>%
    filter(minute == 0 & second == 0) %>% # only full hours
    filter(hour %in% c(0, 2 * (1:11))) %>%
    select(day_sec, hour) %>%
    unique()

  # Plot
  data %>%
    ggplot(aes(x = day_sec, y = value)) +
      geom_line(aes(colour = var), size = 0.5) +
      scale_x_continuous(breaks = x_breaks$day_sec, labels = x_breaks$hour) +
      facet_grid(rows = vars(var), cols = vars(phase), scales = "free", switch = "y") +
      scale_color_manual(values = c("gold", "grey30", "firebrick", "dark blue"), guide = "none") +
      labs(x = "Time (h)",
           y = "") +
      theme_bw() +
      theme(strip.text.y.left = element_markdown(color = "black"),
            strip.text.x = element_markdown(color = "black", face = "bold"),
            strip.background.y = element_blank(),
            strip.placement.y = "outside",
            panel.grid.minor = element_blank(),
            panel.grid.major = element_line(color = "grey95"),
            axis.text = element_text(size = 7))

}

# 39F1 - 39F5
plot_day_night(data[c(13, 17)] %>%
                 bind_rows() %>%
                 mutate(phase = if_else(fragment == "F5", "Night", "Day")))
ggsave(here::here("figures", "cbass_day_night_39F1_39F5.png"),
       width = 18, height = 18, units = "cm", dpi = 600, type = "cairo")
ggsave(here::here("figures", "cbass_day_night_39F1_39F5.pdf"),
       width = 18, height = 18, units = "cm", device = cairo_pdf)

# 39F2 - 39F5
plot_day_night(data[c(14, 17)] %>%
                 bind_rows() %>%
                 mutate(phase = if_else(fragment == "F5", "Night", "Day")))
ggsave(here::here("figures", "cbass_day_night_39F2_39F5.png"),
       width = 18, height = 18, units = "cm", dpi = 600, type = "cairo")
ggsave(here::here("figures", "cbass_day_night_39F2_39F5.pdf"),
       width = 18, height = 18, units = "cm", device = cairo_pdf)

# Plot H2O2 for 30, 33, and 36 (excluding 36-F1) ----

# Merge data
df <- lapply(c(1:8, 10, 11, 12), function(x)
  h2o2[[x]] %>%
    full_join(light[[x]] %>%
                select(light, hour, minute, second, day_sec, treatment, fragment)) %>%
    full_join(temp[[x]] %>%
                select(temperature, hour, minute, second, day_sec, treatment, fragment)) %>%
    arrange(day_sec)
) %>%
  bind_rows() %>%
  mutate(treatment = paste(treatment, "&#176;C"),
         # Remove an interference which remained after the cleaning procedure
         # CBASS 36 - F3 - 18:33:54 until 18:59:09
         h2o2_conc = ifelse(between(day_sec,
                                    (18 * 60 * 60) + (33 * 60) + 54,
                                    (18 * 60 * 60) + (59 * 60) + 9) &
                              fragment == "F3" &
                              treatment == "36 &#176;C",
                            NA,
                            h2o2_conc))

# Function to make multiplot combining H2O2, temperature and light
plot_combined <- function(data, max_temp) {

  # x limits (11am to 8am)
  x_min <- 11 * 60 * 60
  x_max <- (8 * 60 * 60) + (24 * 60 * 60)

  # Filter data
  data <- data %>% filter(between(day_sec, x_min, x_max))

  # Create data frame for x axis breaks
  x_breaks <- data %>%
    filter(minute == 0 & second == 0) %>%
    filter(hour %in% c(0, 2 * (1:11))) %>%
    select(day_sec, hour) %>%
    unique()

  # Get max values to adjust scales
  max_h2o2 <- max(data$h2o2_conc, na.rm = TRUE)
  max_light <- max(data$light, na.rm = TRUE)

  # data frame for annotating light values
  df_light <- tibble(x = c(15 * 60 * 60,
                           19 * 60 * 60 + 15 * 60,
                           24 * 60 * 60 + 2 * 60 * 60,
                           24 * 60 * 60 + 7 * 60 * 60 + 30 * 60),
                     value = c(138, 69, 0, 138),
                     y = value * max_h2o2 / max_light)

  # Plot
  data %>%
    ggplot(aes(x = day_sec)) +
    geom_line(aes(y = (temperature - 25) * max_h2o2 / max_temp * 3),
              color = "grey30") +
    geom_line(aes(y = h2o2_conc), color = "firebrick") +
    geom_line(aes(y = (light + 10) * max_h2o2 / max_light), # add 10 for annotations
              linetype = "dotted") +
    facet_grid(rows = vars(treatment), cols = vars(fragment)) +
    scale_x_continuous(breaks = x_breaks$day_sec, labels = x_breaks$hour) +
    scale_y_continuous(sec.axis = sec_axis(~(. / max_h2o2 * max_temp / 3) + 25, name = "Temperature [&#176;C]")) +
    labs(x = "Time (h)",
         y = "H<sub>2</sub>O<sub>2</sub> [&mu;M]") +
    theme_bw() +
    theme(strip.text.y = element_markdown(color = "black", face = "bold"),
          strip.text.x = element_text(color = "black", face = "bold"),
          #strip.placement.y = "outside",
          axis.title.y.left = element_markdown(),
          axis.title.y.right = element_markdown(),
          panel.grid = element_blank(),
          axis.text = element_text(size = 7)) +
    geom_text(aes(x = x, y = y, label = value), data = df_light, size = 3)
}

p <- plot_combined(df, max_temp = 36.5)
# Remove empty panel
g <- ggplotGrob(p)
# Remove grob
g$grobs[[4]] <- NULL
# Remove them from the layout
g$layout <- g$layout[!(g$layout$name == "panel-3-1"), ]
# Move axis closer to the panels
g$layout[g$layout$name == "axis-l-3", c("l", "r")] = c(6, 6)
g$layout[g$layout$name == "axis-b-1", c("t", "b")] = c(11, 11)
grid::grid.newpage()
grid::grid.draw(g)

ggsave(here::here("figures", "cbass_combined_plot.png"), g,
       width = 24, height = 16, unit = "cm", dpi = 600, type = "cairo")
ggsave(here::here("figures", "cbass_combined_plot.pdf"), g,
       width = 24, height = 16, unit = "cm", device = cairo_pdf)
