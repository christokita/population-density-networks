###################################################
#
# Plotting results of contagions on simulated networks
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

qual_pal <- mako(9)
low_pal <- qual_pal[7]
high_pal <- qual_pal[4]


##########################
# Load contagion simulation data
##########################
# Contagion summary data
complex_contagion_data <- read.csv('data_derived/contagion_complex_results.csv') %>% 
  mutate(reached_majority_spread = as.logical(reached_majority_spread))
simple_contagion_data <- read.csv('data_derived/contagion_simple_results.csv') %>% 
  mutate(reached_majority_spread = as.logical(reached_majority_spread))

# Example contagion timeseries data
simple_contagion_timeseries <- read.csv('data_derived/contagion_timeseries_simple.csv') %>% 
  mutate(
    simulation_id = paste(population_density, network_replicate, contagion_replicate, sep = "_"),
    network_id = paste(population_density, network_replicate, sep = "_")
  ) %>% 
  mutate(
    population_density = gsub("1e-04", "0.0001", population_density),
    population_density = gsub("10000", "10,000", population_density),
  )

complex_contagion_timeseries <- read.csv('data_derived/contagion_timeseries_complex.csv') %>% 
  mutate(
    simulation_id = paste(population_density, network_replicate, contagion_replicate, sep = "_"),
    network_id = paste(population_density, network_replicate, sep = "_")
  ) %>% 
  mutate(
    population_density = gsub("1e-04", "0.0001", population_density),
    population_density = gsub("10000", "10,000", population_density),
  )

##########################
# Assess within vs between network variation
# (Helpful for determining if we need more replicates of networks/contagions)
##########################
# Simple contagion
simple_variance_decomp <- 
  simple_contagion_data %>%
  # Format data
  select(-contagion_type, -k_cap_mean, -k_cap_sd, -contagion_replicate) %>% 
  select(-reached_majority_spread, -final_infected_fraction) %>% 
  gather("metric", "value", -population_density, -network_replicate) %>% 
  # Within network variation
  group_by(population_density, metric, network_replicate) %>%
  summarise(
    mean_outcome = mean(value, na.rm = TRUE),
    var_within = var(value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Between network variation
  group_by(population_density, metric) %>%
  summarise(
    within_network_var = mean(var_within, na.rm = TRUE),
    between_network_var = var(mean_outcome, na.rm = TRUE),
    total_var = within_network_var + between_network_var,
    frac_between = between_network_var / (within_network_var + between_network_var),
    frac_within = within_network_var / (within_network_var + between_network_var),
    .groups = "drop"
  ) %>% 
  arrange(metric, population_density)

# Complex contagion
complex_variance_decomp <- 
  complex_contagion_data %>%
  # Format data
  select(-contagion_type, -k_cap_mean, -k_cap_sd, -contagion_replicate) %>% 
  gather("metric", "value", -population_density, -network_replicate) %>% 
  # Within network variation
  group_by(population_density, metric, network_replicate) %>%
  summarise(
    mean_outcome = mean(value, na.rm = TRUE),
    var_within = var(value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Between network variation
  group_by(population_density, metric) %>%
  summarise(
    within_network_var = mean(var_within, na.rm = TRUE),
    between_network_var = var(mean_outcome, na.rm = TRUE),
    total_var = within_network_var + between_network_var,
    frac_between = between_network_var / (within_network_var + between_network_var),
    frac_within = within_network_var / (within_network_var + between_network_var),
    .groups = "drop"
  ) %>% 
  arrange(metric, population_density)


##########################
# PLOT: Percent of network "infected"
##########################
# Complex Contagion
complex_final_fraction <- 
  complex_contagion_data %>% 
  group_by(population_density) %>% 
  summarise(
    mean = mean(final_infected_fraction),
    sd = sd(final_infected_fraction)
  ) %>% 
  mutate(
    lower = mean - sd/sqrt(nrow(.)),
    upper = mean + sd/sqrt(nrow(.))
  )

gg_percent_complex <- ggplot(complex_final_fraction, aes(x = population_density, y = mean)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), color = NA, alpha = 0.25, fill = plot_pal) +
  geom_line(linewidth = 0.6, color = plot_pal) +
  geom_point(stroke = 0, size = 2, color = plot_pal) +
  scale_x_log10(
    breaks = 10**seq(-4, 4, 2),
    expand = c(0, 0),
    labels = trans_format("log10", math_format(10^.x))
  ) +
  scale_y_continuous(
    expand = c(0, 0)
  ) +
  coord_cartesian(clip = "off") +
  labs(
    x = "Population density",
    y = "% of network infected"
  ) +
  theme_ctokita()

gg_percent_complex
ggsave(
  gg_percent_complex,
  filename = 'output/contagion_complex_percent_infected.pdf',
  width = 45,
  height = 45,
  units = 'mm',
  dpi = 400
)


##########################
# PLOT: Time to majority infection
##########################
# Simple Contagion
simple_time_majority <- 
  simple_contagion_data %>% 
  # Average within network to control for contagion replicates
  group_by(population_density, network_replicate) %>% 
  summarise(
    time_to_majority = mean(time_to_majority, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  # Now calculate across networks
  group_by(population_density) %>% 
  summarise(
    mean = mean(time_to_majority, na.rm = TRUE),
    sd = sd(time_to_majority, na.rm = TRUE)
  ) %>% 
  mutate(
    lower = mean - sd,
    upper = mean + sd
  )

gg_time_simple <- ggplot(simple_time_majority, aes(x = population_density, y = mean)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), color = NA, alpha = 0.25, fill = plot_pal) +
  geom_line(linewidth = 0.6, color = plot_pal) +
  geom_point(stroke = 0, size = 2, color = plot_pal) +
  scale_x_log10(
    breaks = 10**seq(-4, 4, 2),
    expand = c(0, 0),
    labels = trans_format("log10", math_format(10^.x))
  ) +
  scale_y_continuous(
    expand = c(0, 0),
  ) +
  coord_cartesian(clip = "off") +
  labs(
    x = "Population density",
    y = "Time to reach majority"
  ) +
  theme_ctokita()

gg_time_simple
ggsave(
  gg_time_simple,
  filename = 'output/contagion_simple_majority_time.pdf',
  width = 45,
  height = 45,
  units = 'mm',
  dpi = 400
)


# Complex Contagion
complex_time_majority <- 
  complex_contagion_data %>% 
  # Average within network to control for contagion replicates
  group_by(population_density, network_replicate) %>% 
  summarise(
    time_to_majority = mean(time_to_majority, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  # Now calculate across networks
  group_by(population_density) %>% 
  summarise(
    mean = mean(time_to_majority, na.rm = TRUE),
    sd = sd(time_to_majority, na.rm = TRUE)
  ) %>% 
  mutate(
    lower = mean - sd,
    upper = mean + sd
  )

gg_time_complex <- ggplot(complex_time_majority, aes(x = population_density, y = mean)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), color = NA, alpha = 0.25, fill = plot_pal) +
  geom_line(linewidth = 0.6, color = plot_pal) +
  geom_point(stroke = 0, size = 2, color = plot_pal) +
  scale_x_log10(
    breaks = 10**seq(-4, 4, 2),
    expand = c(0, 0),
    labels = trans_format("log10", math_format(10^.x))
  ) +
  scale_y_continuous(
    expand = c(0, 0)
  ) +
  coord_cartesian(clip = "off") +
  labs(
    x = "Population density",
    y = "Time to reach majority"
  ) +
  theme_ctokita()

gg_time_complex
ggsave(
  gg_time_complex,
  filename = 'output/contagion_complex_majority_time.pdf',
  width = 45,
  height = 45,
  units = 'mm',
  dpi = 400
)


##########################
# PLOT: Probability of majority spread
##########################
# Simple Contagion
simple_reached_majority <- 
  simple_contagion_data %>% 
  # Average within network to control for contagion replicates
  group_by(population_density, network_replicate) %>% 
  summarise(
    reached_majority_spread = mean(reached_majority_spread),
    .groups = "drop"
  ) %>% 
  # Now calculate across networks
  group_by(population_density) %>% 
  summarise(
    mean = mean(reached_majority_spread, na.rm = TRUE),
    sd = sd(reached_majority_spread, na.rm = TRUE)
  ) %>% 
  mutate(
    lower = mean - sd,
    upper = mean + sd
  )

gg_majority_simple <- ggplot(simple_reached_majority, aes(x = population_density, y = mean)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), color = NA, alpha = 0.25, fill = plot_pal) +
  geom_line(linewidth = 0.6, color = plot_pal) +
  geom_point(stroke = 0, size = 2, color = plot_pal) +
  scale_x_log10(
    breaks = 10**seq(-4, 4, 2),
    expand = c(0, 0),
    labels = trans_format("log10", math_format(10^.x))
  ) +
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0, 1)
  ) +
  coord_cartesian(clip = "off") +
  labs(
    x = "Population density",
    y = "Reached majority of individuals"
  ) +
  theme_ctokita()

gg_majority_simple
ggsave(
  gg_majority_simple,
  filename = 'output/contagion_simple_majority_spread.pdf',
  width = 45,
  height = 45,
  units = 'mm',
  dpi = 400
)


# Complex Contagion
complex_reached_majority <- 
  complex_contagion_data %>% 
  # Average within network to control for contagion replicates
  group_by(population_density, network_replicate) %>% 
  summarise(
    reached_majority_spread = mean(reached_majority_spread),
    .groups = "drop"
  ) %>% 
  # Now calculate across networks
  group_by(population_density) %>% 
  summarise(
    mean = mean(reached_majority_spread, na.rm = TRUE),
    sd = sd(reached_majority_spread, na.rm = TRUE)
  ) %>% 
  mutate(
    lower = mean - sd,
    upper = mean + sd
  )

gg_majority_complex <- ggplot(complex_reached_majority, aes(x = population_density, y = mean)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), color = NA, alpha = 0.25, fill = plot_pal) +
  geom_line(linewidth = 0.6, color = plot_pal) +
  geom_point(stroke = 0, size = 2, color = plot_pal) +
  scale_x_log10(
    breaks = 10**seq(-4, 4, 2),
    expand = c(0, 0),
    labels = trans_format("log10", math_format(10^.x))
  ) +
  scale_y_continuous(
    expand = c(0, 0),
  ) +
  coord_cartesian(clip = "off") +
  labs(
    x = "Population density",
    y = "Reached majority of individuals"
  ) +
  theme_ctokita()

gg_majority_complex
ggsave(
  gg_majority_complex,
  filename = 'output/contagion_complex_majority_spread.pdf',
  width = 45,
  height = 45,
  units = 'mm',
  dpi = 400
)


##########################
# PLOT: Example timeseries of contagion spread
##########################
# Simple contagion
averaged_simple_timeseries <- simple_contagion_timeseries %>% 
  group_by(network_id, population_density, network_replicate, time) %>% 
  summarise(pct_infected = mean(pct_infected))

gg_timeseries_simple <- ggplot(averaged_simple_timeseries, aes(x = time, y = pct_infected, group = network_id, color = population_density)) +
  geom_line(linewidth = 0.3, alpha = 0.2) +
  scale_x_continuous(
    limits = c(0, 50),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    expand = c(0, 0)
  ) +
  scale_color_manual(
    name = "Population\ndensity",
    values = c(low_pal, high_pal)
  ) +
  labs(
    x = "Time step",
    y = "Population infected (%)"
  ) +
  theme_ctokita(color_bar = FALSE) +
  theme(
    legend.position = c(0.8, 0.25),
    aspect.ratio = 0.5
  )

gg_timeseries_simple
ggsave(
  gg_timeseries_simple,
  filename = 'output/contagion_simple_timeseries.pdf',
  width = 90,
  height = 45,
  units = 'mm',
  dpi = 400
)

# Complex contagion
averaged_complex_timeseries <- complex_contagion_timeseries %>% 
  group_by(network_id, population_density, network_replicate, time) %>% 
  summarise(pct_infected = mean(pct_infected))

gg_timeseries_complex <- ggplot(averaged_complex_timeseries, aes(x = time, y = pct_infected, group = network_id, color = population_density)) +
  geom_line(linewidth = 0.3, alpha = 0.2) +
  scale_x_continuous(
    limits = c(0, 50),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(0, 0.701),
    breaks = seq(0, 1, 0.1),
    expand = c(0, 0)
  ) +
  scale_color_manual(
    name = "Population\ndensity",
    values = c(low_pal, high_pal)
  ) +
  labs(
    x = "Time step",
    y = "Population infected (%)"
  ) +
  theme_ctokita(color_bar = FALSE) +
  theme(
    legend.position = c(0.8, 0.25),
    aspect.ratio = 0.5,
  )

gg_timeseries_complex
ggsave(
  gg_timeseries_complex,
  filename = 'output/contagion_complex_timeseries.pdf',
  width = 90,
  height = 45,
  units = 'mm',
  dpi = 400
)

