# Load packages ----

library(here)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggtext)
library(ggpubr)
library(grid)
library(drc)

# Load all datasets ----

# microsensor
files_microsensor <- list.files(here::here("data", "cleaned_data", "h2o2_no_interferences"))
microsensor <- lapply(files_microsensor, function(x)
  readr::read_csv(here::here("data", "cleaned_data", "h2o2_no_interferences", x)))
files_microsensor <- gsub("_h2o2_clean.csv", "", files_microsensor)
names(microsensor) <- files_microsensor
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
# pam, vba and cell count
files_pam_vba_cc <- list.files(here::here("data", "cleaned_data", "pam_vba_cellcount"))
pam_vba_cc <- lapply(files_pam_vba_cc, function(x)
  readr::read_csv(here::here("data", "cleaned_data", "pam_vba_cellcount", x)))
files_pam_vba_cc <- gsub(".csv", "", files_pam_vba_cc)
names(pam_vba_cc) <- files_pam_vba_cc

# Merge data ----

# microsensor, light and temperature data
data1 <- lapply(1:length(microsensor), function(x)
  microsensor[[x]] %>%
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

# pam, vba and cellcount
data2 <- pam_vba_cc$CBASS8_pam |>
  dplyr::select(temperature, sample, pam_mean) |>
  dplyr::left_join(pam_vba_cc$CBASS8_VBA |>
                     dplyr::select(temperature, sample, bleaching_mean)) |>
  dplyr::left_join(pam_vba_cc$CBASS8_cellcount |>
                     dplyr::select(temperature, sample, cell_nr_divided)) |>
  tidyr::pivot_longer(cols = 3:5,
                      values_to = "value",
                      names_to = "var") |>
  dplyr::mutate(fac_temp = as.factor(temperature),
                var = factor(var,
                             levels = c("pam_mean",
                                        "bleaching_mean",
                                        "cell_nr_divided"),
                             labels = c("Photosynthetic<br>efficiency [Fv/Fm]",
                                        "Coral<br>pigmentation [%]",
                                        "Symbiodinaceae<br>number (10<sup>6</sup>)")))

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
    dplyr::select(day_sec, hour) %>%
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
plot_by_treatment(data1[[1]])

# 30
plot_by_treatment(bind_rows(data1[1:4]))
ggsave(here::here("figures", "cbass30.png"),
       width = 18, height = 16, units = "cm", dpi = 600, type = "cairo")
ggsave(here::here("figures", "cbass30.pdf"),
       width = 18, height = 16, units = "cm", device = cairo_pdf)

# 33
plot_by_treatment(bind_rows(data1[5:8]))
ggsave(here::here("figures", "cbass33.png"),
       width = 18, height = 16, units = "cm", dpi = 600, type = "cairo")
ggsave(here::here("figures", "cbass33.pdf"),
       width = 18, height = 16, units = "cm", device = cairo_pdf)

# 36
plot_by_treatment(bind_rows(data1[9:12]))
ggsave(here::here("figures", "cbass36.png"),
       width = 18, height = 16, units = "cm", dpi = 600, type = "cairo")
ggsave(here::here("figures", "cbass36.pdf"),
       width = 18, height = 16, units = "cm", device = cairo_pdf)

# 39
plot_by_treatment(bind_rows(data1[13:16]))
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
    dplyr::select(day_sec, hour) %>%
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

# 30F3 - 33F4 - 36F4 - 39F1
fig3a <- plot_across_treatments(data1[c(3, 8, 12, 13)] %>%
                                  bind_rows() %>%
                                  mutate(treatment = paste(treatment, "&#176;C")))
ggsave(here::here("figures", "cbass_30F3_33F4_36F4_39F1.png"), fig3a,
       width = 18, height = 16, units = "cm", dpi = 600, type = "cairo")
ggsave(here::here("figures", "cbass_30F3_33F4_36F4_39F1.pdf"), fig3a,
       width = 18, height = 16, units = "cm", device = cairo_pdf)

# # Alternative plots
# # 30F3 - 33F4 - 36F1 - 39F1
# plot_across_treatments(data1[c(3, 8, 9, 13)] %>%
#                          bind_rows() %>%
#                          mutate(treatment = paste(treatment, "&#176;C")))
# ggsave(here::here("figures", "cbass_30F3_33F4_36F1_39F1.png"),
#        width = 18, height = 16, units = "cm", dpi = 600, type = "cairo")
# ggsave(here::here("figures", "cbass_30F3_33F4_36F1_39F1.pdf"),
#        width = 18, height = 16, units = "cm", device = cairo_pdf)
#
# # 30F3 - 33F4 - 36F4 - 39F2
# plot_across_treatments(data1[c(3, 8, 12, 14)] %>%
#                          bind_rows() %>%
#                          mutate(treatment = paste(treatment, "&#176;C")))
# ggsave(here::here("figures", "cbass_30F3_33F4_36F4_39F2.png"),
#        width = 18, height = 16, units = "cm", dpi = 600, type = "cairo")
# ggsave(here::here("figures", "cbass_30F3_33F4_36F4_39F2.pdf"),
#        width = 18, height = 16, units = "cm", device = cairo_pdf)

# Plot pam, bleaching and cell count
fig3bcd <- ggplot(data2, aes(x = fac_temp, y = value)) +
  geom_violin(fill = "grey90", ) +
  geom_boxplot(width = .2, outlier.shape = NA, coef = 0) +
  geom_point() +
  facet_grid(rows = vars(var),
             switch = "y",
             scales = "free_y") +
  xlab("Temperature [°C]") +
  theme_bw() +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 12),
        strip.text.y.left = ggtext::element_markdown(size = 9),
        strip.background = element_blank(),
        strip.placement = "outside",
        axis.title.y = element_blank(),
        panel.grid.minor = element_blank())

ggsave(here::here("figures", "pam_bleaching_cellcount.png"), fig3bcd,
       width = 18, height = 20, units = "cm", dpi = 600, type = "cairo")
ggsave(here::here("figures", "pam_bleaching_cellcount.pdf"), fig3bcd,
       width = 18, height = 20, units = "cm", device = cairo_pdf)

# Combine figures
ggpubr::ggarrange(fig3a, fig3bcd,
                  nrow = 2,
                  align = "h",
                  heights = c(0.6, 0.4),
                  labels = c("A", "B"),
                  vjust = 1)
ggsave(here::here("figures", "fig3.png"),
       width = 18, height = 24, units = "cm", dpi = 600, type = "cairo")
ggsave(here::here("figures", "fig3.pdf"),
       width = 18, height = 24, units = "cm", device = cairo_pdf)

# Test differences across temperatures ----

# Temperature as factor
pam_vba_cc <- lapply(pam_vba_cc, function(x)
  mutate(x, fac_temp = as.factor(temperature)))

# Table 1
#(1) Is 30°C significantly affecting the photosynthetic efficiency of P.damicornis?
# Analysis of variance (1-way) ANOVA
pam_control_anova <- aov(pam_mean ~ fac_temp, data = pam_vba_cc$CBASS1_MAREE_control_pam)
summary(pam_control_anova)
# p-value: 0.674
# Contrasts
TukeyHSD(pam_control_anova)
plot(pam_control_anova)

# Table 2:
#(1) How does temperature impact photosynthetic efficiency in the CBASS+ Microsensor assay?
# Analysis of variance (1-way) ANOVA
pam_cbass8_anova <- aov(pam_mean ~ fac_temp, data = pam_vba_cc$CBASS8_pam)
summary(pam_cbass8_anova)
# p-value: <0.001
# Contrasts
TukeyHSD(pam_cbass8_anova)
# p-value is significant for: 36-30, 39-30, 36-33, 39-33, 39-36°C
plot (pam_cbass8_anova)

# (2) How does temperature impact coral pigmentation in the CBASS+ Microsensor assay?
# Analysis of variance (1-way) ANOVA
VBA_cbass8_anova <- aov(bleaching_mean ~ fac_temp, data = pam_vba_cc$CBASS8_VBA)
summary(VBA_cbass8_anova)
# p-value: 0.00427
# Contrasts
TukeyHSD(VBA_cbass8_anova)
# p-value is significant for: 36-30, 39-30, 36-33, 39-33, 39-36°C
plot(pam_cbass8_anova)

# (3) How does temperature impact Symbioodiniaceae cell number in the CBASS+ Microsensor assay?
# Analysis of variance (1-way) ANOVA
CN_cbass8_anova <- aov(cell_nr ~ fac_temp, data = pam_vba_cc$CBASS8_cellcount)
summary(CN_cbass8_anova)
# p-value: 0.0214
# Contrasts
TukeyHSD(CN_cbass8_anova)
# p-value is significant for: 36-30, 39-30, 36-33, 39-33, 39-36°C
plot (pam_cbass8_anova)

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
    dplyr::select(day_sec, hour) %>%
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
plot_day_night(data1[c(13, 17)] %>%
                 bind_rows() %>%
                 mutate(phase = if_else(fragment == "F5", "Night", "Day")))
ggsave(here::here("figures", "cbass_day_night_39F1_39F5.png"),
       width = 18, height = 18, units = "cm", dpi = 600, type = "cairo")
ggsave(here::here("figures", "cbass_day_night_39F1_39F5.pdf"),
       width = 18, height = 18, units = "cm", device = cairo_pdf)

# # Alternative plot
# # 39F2 - 39F5
# plot_day_night(data1[c(14, 17)] %>%
#                  bind_rows() %>%
#                  mutate(phase = if_else(fragment == "F5", "Night", "Day")))
# ggsave(here::here("figures", "cbass_day_night_39F2_39F5.png"),
#        width = 18, height = 18, units = "cm", dpi = 600, type = "cairo")
# ggsave(here::here("figures", "cbass_day_night_39F2_39F5.pdf"),
#        width = 18, height = 18, units = "cm", device = cairo_pdf)

# Plot data for 30, 33, and 36 (excluding 36-F1) ----

# Merge data
df <- lapply(c(1:8, 10, 11, 12), function(x)
  microsensor[[x]] %>%
    full_join(light[[x]] %>%
                dplyr::select(light, hour, minute, second, day_sec, treatment, fragment)) %>%
    full_join(temp[[x]] %>%
                dplyr::select(temperature, hour, minute, second, day_sec, treatment, fragment)) %>%
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
    dplyr::select(day_sec, hour) %>%
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
    geom_line(aes(y = (light) * max_h2o2 / max_light),
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
    geom_text(aes(x = x, y = y - 3, label = value), data = df_light, size = 3) # subtract 3 for annotations
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

# Plot ED50 for photosynthetic efficiency ----

# CBASS pilot run to identify the baseline temperature
# ED50 Model
PAM_cbass1_DRC = drm(pam_mean ~ temperature,
                     data = pam_vba_cc$CBASS1_pam,
                     fct = LL.3(names = c('hill', 'max', 'ed50')))

summary(PAM_cbass1_DRC)
plot(PAM_cbass1_DRC)

# Extract ED50
ED50_PAM_cbass1 <- ED(PAM_cbass1_DRC, c(50))

# plot
temp_x <- seq(27, 39, length = 100)
dplyr::bind_cols(temp = temp_x,
                 as.data.frame(predict(PAM_cbass1_DRC,
                                       data.frame(Temp = temp_x),
                                       interval = "confidence"))) |>
  ggplot(aes(x = temp, y = Prediction)) +
  geom_line(col = "#2c7bb6") +
  geom_ribbon(aes(ymin = Lower, ymax = Upper),
              col = "#2c7bb6",
              fill = NA,
              linetype = 2) +
  geom_point(data = pam_vba_cc$CBASS1_pam, aes(x = temperature, y = pam_mean),
             col = "#2c7bb6", shape = 18, size = 3) +
  geom_vline(xintercept = c(ED50_PAM_cbass1[1, 1],
                            ED50_PAM_cbass1[1, 1] - 1.96 * ED50_PAM_cbass1[1, 2],
                            ED50_PAM_cbass1[1, 1] + 1.96 * ED50_PAM_cbass1[1, 2]),
             col = "#2c7bb6", linetype = c(1, 2, 2)) +
  annotate("richtext",
           x = ED50_PAM_cbass1[1, 1], y = 0.02,
           label = paste(round(ED50_PAM_cbass1[1, 1], digits = 2),
                         "&plusmn;",
                         round(ED50_PAM_cbass1[1, 2], digits = 2),
                         "°C"),
           col = "#2c7bb6", size = 4) +
  ylab("Photosynthetic efficiency [Fv/Fm]") +
  xlab("Temperature [°C]") +
  xlim(27, 39) +
  ylim(0, 0.68) +
  theme_bw() +
  theme(panel.grid = element_blank())

ggsave(here::here("figures", "ed50_pam_cbass1.png"),
       width = 12, height = 10, unit = "cm", dpi = 600, type = "cairo")
ggsave(here::here("figures", "ed50_pam_cbass1.pdf"),
       width = 12, height = 10, unit = "cm", device = cairo_pdf)

# CBASS with microsensors
# ED50 Model
PAM_cbass8_DRC = drm(pam_mean ~ temperature,
                     data = pam_vba_cc$CBASS8_pam,
                     fct = LL.3(names = c('hill', 'max', 'ed50')))

summary(PAM_cbass8_DRC)
plot(PAM_cbass8_DRC)

# Extract ED50
ED50_PAM_cbass8 <- ED(PAM_cbass8_DRC, c(50))

# plot
temp_x <- seq(30, 39, length = 100)
dplyr::bind_cols(temp = temp_x,
                 as.data.frame(predict(PAM_cbass8_DRC,
                                       data.frame(Temp = temp_x),
                                       interval = "confidence"))) |>
  ggplot(aes(x = temp, y = Prediction)) +
  geom_line(col = "#2c7bb6") +
  geom_ribbon(aes(ymin = Lower, ymax = Upper),
              col = "#2c7bb6",
              fill = NA,
              linetype = 2) +
  geom_point(data = pam_vba_cc$CBASS8_pam, aes(x = temperature, y = pam_mean),
             col = "#2c7bb6", shape = 18, size = 3) +
  geom_vline(xintercept = c(ED50_PAM_cbass8[1, 1],
                            ED50_PAM_cbass8[1, 1] - 1.96 * ED50_PAM_cbass8[1, 2],
                            ED50_PAM_cbass8[1, 1] + 1.96 * ED50_PAM_cbass8[1, 2]),
             col = "#2c7bb6", linetype = c(1, 2, 2)) +
  annotate("richtext",
           x = ED50_PAM_cbass8[1, 1], y = 0.02,
           label = paste(round(ED50_PAM_cbass8[1, 1], digits = 2),
                         "&plusmn;",
                         round(ED50_PAM_cbass8[1, 2], digits = 2),
                         "°C"),
           col = "#2c7bb6", size = 4) +
  ylab("Photosynthetic efficiency [Fv/Fm]") +
  xlab("Temperature [°C]") +
  xlim(30, 39.5) +
  ylim(0, 0.68) +
  theme_bw() +
  theme(panel.grid = element_blank())

ggsave(here::here("figures", "ed50_pam_cbass8.png"),
       width = 12, height = 10, unit = "cm", dpi = 600, type = "cairo")
ggsave(here::here("figures", "ed50_pam_cbass8.pdf"),
       width = 12, height = 10, unit = "cm", device = cairo_pdf)
