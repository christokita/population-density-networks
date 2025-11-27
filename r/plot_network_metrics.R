###################################################
#
# Plotting network metrics from simulations
#
###################################################

require(dplyr)
require(tidyr)
require(ggplot2)
require(scales)
require(viridisLite)
require(patchwork)
source("_plot_themes/theme_ctokita.R")


##########################
# Define plot features
##########################
heat_map_pal <- c('#fff7fb','#ece7f2','#d0d1e6','#a6bddb','#74a9cf','#3690c0','#0570b0','#045a8d','#023858')
heat_map_pal <-  rocket(9)
plot_pal <- heat_map_pal[5]

##########################
# Load data
##########################
# Load full parameter sweep data
sweep_data <- read.csv('data_derived/full_parameter_sweep_results.csv')

# Summarize by parameter combination
paramter_combination_results <- 
  sweep_data %>% 
  select(-replicate, -network_is_connected) %>% 
  group_by(k_cap_mean, population_density) %>% 
  summarise_all("mean")


##########################
# PLOT: Heat maps of network metrics
##########################
# Custom function to plot the metric of choice
plot_heat_map <- function(data, metric_name, metric_label, pal) {
  # Grab metric
  data$metric <- data[[metric_name]]
  
  # Create plot
  gg_heatmap <-
    ggplot(data, aes(x=population_density, y=k_cap_mean, fill=metric, color=metric)) +
    geom_tile() +
    scale_x_log10(
      breaks = 10**seq(-4, 4, 2),
      expand = c(0, 0),
      labels = trans_format("log10", math_format(10^.x))
    ) +
    scale_y_continuous(
      breaks = c(5, seq(25, 100, 25)),
      expand = c(0, 0)
    ) +
    scale_fill_gradientn(
      name = metric_label,
      colors = pal
    ) +
    scale_color_gradientn(
      name = metric_label,
      colors = pal
    ) +
    labs(
      x = "Population density",
      y = "Avg. social capacity"
    ) +
    theme_ctokita() +
    theme(
      axis.line = element_blank(),
      panel.border = element_rect(linewidth = 0.3),
    )
  return(gg_heatmap)
}

# Network density
gg_heatmap_density <- plot_heat_map(
  data = paramter_combination_results,
  metric_name = 'network_density',
  metric_label = 'Network\ndensity',
  pal = heat_map_pal
)
gg_heatmap_density
ggsave(
  gg_heatmap_density,
  filename = 'output/heatmap_network_density.pdf',
  width = 90, height = 45, units = 'mm',
  dpi = 400,
)

# Network diameter
gg_heatmap_diameter <- plot_heat_map(
  data = paramter_combination_results,
  metric_name = 'network_diameter',
  metric_label = 'Network\ndiameter',
  pal = heat_map_pal
)
gg_heatmap_diameter
ggsave(
  gg_heatmap_diameter,
  filename = 'output/heatmap_network_diameter.pdf',
  width = 90, height = 45, units = 'mm',
  dpi = 400,
)

# Shortest path
gg_heatmap_path <- plot_heat_map(
  data = paramter_combination_results,
  metric_name = 'network_avg_shortest_path',
  metric_label = 'Avg. shortest\npath',
  pal = heat_map_pal
)
gg_heatmap_path
ggsave(
  gg_heatmap_path,
  filename = 'output/heatmap_shortest_path.pdf',
  width = 90, height = 45, units = 'mm',
  dpi = 400,
)

# Clustering
gg_heatmap_clustering <- plot_heat_map(
  data = paramter_combination_results,
  metric_name = 'network_clustering_coef',
  metric_label = 'Clustering\ncoefficient',
  pal = heat_map_pal
)
gg_heatmap_clustering
ggsave(
  gg_heatmap_clustering,
  filename = 'output/heatmap_clustering.pdf',
  width = 90, height = 45, units = 'mm',
  dpi = 400,
)

# Modularity
gg_heatmap_modularity <- plot_heat_map(
  data = paramter_combination_results,
  metric_name = 'network_modularity',
  metric_label = 'Modularity',
  pal = heat_map_pal
)
gg_heatmap_modularity
ggsave(
  gg_heatmap_modularity,
  filename = 'output/heatmap_modularity.pdf',
  width = 90, height = 45, units = 'mm',
  dpi = 400,
)

# Assortativity
gg_heatmap_assortativity <- plot_heat_map(
  data = paramter_combination_results,
  metric_name = 'network_assortativity',
  metric_label = 'Assortativity',
  pal = heat_map_pal
)
gg_heatmap_assortativity
ggsave(
  gg_heatmap_assortativity,
  filename = 'output/heatmap_assortativity.pdf',
  width = 90, height = 45, units = 'mm',
  dpi = 400,
)


##########################
# PLOT: Grid heat map
##########################
# Make labeled, legend-free versions of each plot
gg_density_lab <- gg_heatmap_density +
  labs(title = "Network density", fill = NULL) +
  theme_ctokita() +
  theme(
    legend.position = "none",
    axis.line = element_blank(),
    panel.border = element_rect(linewidth = 0.3)
  )

gg_diameter_lab <- gg_heatmap_diameter +
  labs(title = "Network diameter", fill = NULL) +
  theme_ctokita() +
  theme(
    legend.position = "none",
    axis.line = element_blank(),
    panel.border = element_rect(linewidth = 0.3)
  )

gg_clustering_lab <- gg_heatmap_clustering +
  labs(title = "Clustering coefficient", fill = NULL) +
  theme_ctokita() +
  theme(
    legend.position = "none",
    axis.line = element_blank(),
    panel.border = element_rect(linewidth = 0.3)
  )

gg_modularity_lab <- gg_heatmap_modularity +
  labs(title = "Modularity", fill = NULL) +
  theme_ctokita() +
  theme(
    legend.position = "none",
    axis.line = element_blank(),
    panel.border = element_rect(linewidth = 0.3)
  )

gg_path_lab <- gg_heatmap_path +
  labs(title = "Avg. Shortest Path", fill = NULL) +
  theme_ctokita() +
  theme(
    legend.position = "none",
    axis.line = element_blank(),
    panel.border = element_rect(linewidth = 0.3)
  )

gg_assort_lab <- gg_heatmap_assortativity +
  labs(title = "Assortativity", fill = NULL) +
  theme_ctokita() +
  theme(
    legend.position = "none",
    axis.line = element_blank(),
    panel.border = element_rect(linewidth = 0.3)
  )

# Arrange into a separate grid plot (no color bars)
gg_heatmap_grid <-
  (gg_density_lab + gg_diameter_lab + gg_path_lab +
     gg_clustering_lab + gg_modularity_lab + gg_assort_lab) +
  plot_layout(ncol = 3)

gg_heatmap_grid
ggsave(
  gg_heatmap_grid,
  filename = 'output/full_parameter_sweep_plot.pdf',
  width = 180,
  height = 100,
  units = 'mm',
  dpi = 400
)


##########################
# PLOT: Population density sweep within selected social capacity
##########################
# Select simulations
simulation_data <- sweep_data %>% 
  filter(k_cap_mean == 50)

simulation_results <- 
  simulation_data %>% 
  select(k_cap_mean, population_density, network_density:network_assortativity, -network_is_connected) %>% 
  gather('metric', 'value', -k_cap_mean, -population_density) %>% 
  group_by(k_cap_mean, population_density, metric) %>% 
  summarise(
    mean = mean(value),
    sd = sd(value)
  ) %>% 
  arrange(metric, population_density)

# Custom function to plot the metric of choice
plot_density_sweep<- function(data, metric_name, metric_label, pal) {
  # Grab metric
  data <- 
    data %>% 
    filter(metric == metric_name) %>% 
    mutate(
      lower = mean - sd,
      upper = mean + sd
    )
  
  # Create plot
  gg_metric_plot <-
    ggplot(data, aes(x=population_density, y=mean)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), color = NA, alpha = 0.25, fill = pal) +
    geom_line(linewidth = 0.6, color = pal) +
    geom_point(stroke = 0, size = 2, color = pal) +
    scale_x_log10(
      breaks = 10**seq(-4, 4, 2),
      expand = c(0, 0),
      labels = trans_format("log10", math_format(10^.x))
    ) +
    scale_y_continuous(
      expand = c(0, 0)
    ) +
    labs(
      x = "Population density",
      y = metric_label
    ) +
    theme_ctokita() 
  return(gg_metric_plot)
}

# Individual metric plots
gg_density <- plot_density_sweep(
  data = simulation_results,
  metric_name = 'network_density',
  metric_label = 'Network density',
  pal = plot_pal
)

gg_diameter <- plot_density_sweep(
  data = simulation_results,
  metric_name = 'network_diameter',
  metric_label = 'Network diameter',
  pal = plot_pal
)

gg_shortest_path <- plot_density_sweep(
  data = simulation_results,
  metric_name = 'network_avg_shortest_path',
  metric_label = 'Avg. shortest path',
  pal = plot_pal
)

gg_clustering <- plot_density_sweep(
  data = simulation_results,
  metric_name = 'network_clustering_coef',
  metric_label = 'Clustering coefficient',
  pal = plot_pal
)

gg_modularity <- plot_density_sweep(
  data = simulation_results,
  metric_name = 'network_modularity',
  metric_label = 'Modularity',
  pal = plot_pal
)

gg_assortativity <- plot_density_sweep(
  data = simulation_results,
  metric_name = 'network_assortativity',
  metric_label = 'Assortativity',
  pal = plot_pal
)


##########################
# PLOT: Grid of density-sweep metrics
##########################
gg_density_sweep_grid <-
  (gg_density + gg_diameter + gg_shortest_path +
     gg_clustering + gg_modularity + gg_assortativity) +
  plot_layout(ncol = 3)

gg_density_sweep_grid
ggsave(
  gg_density_sweep_grid,
  filename = 'output/population_density_sweep.pdf',
  width    = 160,
  height   = 90,
  units    = 'mm',
  dpi      = 400
)
