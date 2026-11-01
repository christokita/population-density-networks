###################################################
#
# Plotting results of supplemental contagion simulations
#
###################################################

require(dplyr)
require(tidyr)
require(ggplot2)
require(scales)
require(viridisLite)
require(patchwork)
require(brms)
source("_plot_themes/theme_ctokita.R")


##########################
# Define plot features
##########################
heat_map_pal <-  rocket(9)
plot_pal <- heat_map_pal[5]

plot_pal <- "#96939B"
contrast_pal <- "#FF6B35"

qual_pal <- mako(9)
low_pal <- qual_pal[7]
high_pal <- qual_pal[4]


##########################
# Load contagion simulation data
##########################
# Supplemental ontagion  data
complex_contagion_data <- read.csv('data_derived/sensitivity_analysis/contagion_threshold_sensitivity_results.csv') 



##########################
# PLOT: Percent of network "infected"
##########################
complex_final_fraction <- 
  complex_contagion_data %>% 
  group_by(threshold_distribution, population_density) %>% 
  summarise(
    mean = mean(final_infected_fraction),
    sd = sd(final_infected_fraction)
  ) %>% 
  mutate(
    lower = mean - sd/sqrt(nrow(.)),
    upper = mean + sd/sqrt(nrow(.))
  )

gg_percent_complex <- ggplot(complex_final_fraction, 
                             aes(x = population_density, y = mean, color = threshold_distribution, fill = threshold_distribution)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), color = NA, alpha = 0.2) +
  geom_line(linewidth = 0.6) +
  geom_point(stroke = 0, size = 2) +
  scale_x_log10(
    breaks = 10**seq(-4, 4, 2),
    expand = c(0, 0),
    labels = trans_format("log10", math_format(10^.x))
  ) +
  scale_y_continuous(
    expand = c(0, 0)
  ) +
  scale_color_manual(
    name = 'Threshold\ndistribution',
    values = c(plot_pal, contrast_pal),
    labels = c('Uniform', 'Min. 2 neighbors')
  ) +
  scale_fill_manual(
    name = 'Threshold\ndistribution',
    values = c(plot_pal, contrast_pal),
    labels = c('Uniform', 'Min. 2 neighbors')
  ) +
  coord_cartesian(clip = "off") +
  labs(
    x = "Population density",
    y = "% of network infected"
  ) +
  theme_ctokita(color_bar=FALSE) +
  theme(
    legend.position = 'right'
  )

gg_percent_complex
ggsave(
  gg_percent_complex,
  filename = 'output/sensitivity_analysis/contagion_complex_percent_infected.pdf',
  width = 75,
  height = 45,
  units = 'mm',
  dpi = 400
)


##########################
# PLOT: Time to majority infection
##########################
complex_time_majority <- 
  complex_contagion_data %>% 
  # Average within network to control for contagion replicates
  group_by(threshold_distribution, population_density, network_replicate) %>% 
  summarise(
    time_to_majority = mean(time_to_majority, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  # Now calculate across networks
  group_by(threshold_distribution, population_density) %>% 
  summarise(
    mean = mean(time_to_majority, na.rm = TRUE),
    sd = sd(time_to_majority, na.rm = TRUE),
    n = sum(!is.na(time_to_majority)),
    .groups = "drop"
  ) %>% 
  mutate(
    lower = mean - sd/sqrt(n),
    upper = mean + sd/sqrt(n)
  )

gg_time_complex <- ggplot(complex_time_majority, 
                          aes(x = population_density, y = mean, color = threshold_distribution, fill = threshold_distribution)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), color = NA, alpha = 0.2) +
  geom_line(linewidth = 0.6) +
  geom_point(stroke = 0, size = 2) +
  scale_x_log10(
    breaks = 10**seq(-4, 4, 2),
    expand = c(0, 0),
    labels = trans_format("log10", math_format(10^.x))
  ) +
  scale_y_continuous(
    expand = c(0, 0)
  ) +
  scale_color_manual(
    name = 'Threshold\ndistribution',
    values = c(plot_pal, contrast_pal),
    labels = c('Uniform', 'Min. 2 neighbors')
  ) +
  scale_fill_manual(
    name = 'Threshold\ndistribution',
    values = c(plot_pal, contrast_pal),
    labels = c('Uniform', 'Min. 2 neighbors')
  ) +
  coord_cartesian(clip = "off") +
  labs(
    x = "Population density",
    y = "Time to reach majority"
  ) +
  theme_ctokita(color_bar = FALSE) +
  theme(
    legend.position = 'right'
  )

gg_time_complex
ggsave(
  gg_time_complex,
  filename = 'output/sensitivity_analysis/contagion_complex_majority_time.pdf',
  width = 75,
  height = 45,
  units = 'mm',
  dpi = 400
)


##########################
# PLOT: Probability of majority spread
##########################
complex_reached_majority <- 
  complex_contagion_data %>% 
  mutate(
    reached_majority_spread = case_when(
      tolower(as.character(reached_majority_spread)) %in% c("true", "1", "1.0") ~ 1,
      tolower(as.character(reached_majority_spread)) %in% c("false", "0", "0.0") ~ 0,
      TRUE ~ NA_real_
    )
  ) %>% 
  # Average within network to control for contagion replicates
  group_by(threshold_distribution, population_density, network_replicate) %>% 
  summarise(
    reached_majority_spread = mean(reached_majority_spread, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  # Now calculate across networks
  group_by(threshold_distribution, population_density) %>% 
  summarise(
    mean = mean(reached_majority_spread, na.rm = TRUE),
    sd = sd(reached_majority_spread, na.rm = TRUE),
    n = sum(!is.na(reached_majority_spread)),
    .groups = "drop"
  ) %>% 
  mutate(
    lower = mean - sd/sqrt(n),
    upper = mean + sd/sqrt(n)
  )

gg_majority_complex <- ggplot(complex_reached_majority, 
                              aes(x = population_density, y = mean, color = threshold_distribution, fill = threshold_distribution)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), color = NA, alpha = 0.2) +
  geom_line(linewidth = 0.6) +
  geom_point(stroke = 0, size = 2) +
  scale_x_log10(
    breaks = 10**seq(-4, 4, 2),
    expand = c(0, 0),
    labels = trans_format("log10", math_format(10^.x))
  ) +
  scale_y_continuous(
    expand = c(0, 0)
  ) +
  scale_color_manual(
    name = 'Threshold\ndistribution',
    values = c(plot_pal, contrast_pal),
    labels = c('Uniform', 'Min. 2 neighbors')
  ) +
  scale_fill_manual(
    name = 'Threshold\ndistribution',
    values = c(plot_pal, contrast_pal),
    labels = c('Uniform', 'Min. 2 neighbors')
  ) +
  coord_cartesian(clip = "off") +
  labs(
    x = "Population density",
    y = "Reached majority of individuals"
  ) +
  theme_ctokita(color_bar = FALSE) +
  theme(
    legend.position = 'right'
  )

gg_majority_complex
ggsave(
  gg_majority_complex,
  filename = 'output/sensitivity_analysis/contagion_complex_majority_spread.pdf',
  width = 75,
  height = 45,
  units = 'mm',
  dpi = 400
)
