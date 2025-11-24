###################################################
#
# Plotting network metrics from simulations
#
###################################################

require(dplyr)
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
# PLOT: Heat maps of relative network metrics (within social capacity)
##########################
# Custom function to plot the metric of choice
plot_relative_heat_map <- function(data, metric_name, metric_label, pal) {
  # Grab metric
  data$metric <- data[[metric_name]]
  data <- 
    data %>% 
    group_by(k_cap_mean) %>% 
    mutate(relative_metric = (metric - min(metric)) / (max(metric) - min(metric)))
  
  # Create plot
  gg_heatmap <-
    ggplot(data, aes(x=population_density, y=k_cap_mean, fill=relative_metric, color=relative_metric)) +
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
      name = paste0('Relative\n', metric_label),,
      colors = pal,
      # limits = c(0, 1),
      # breaks = c(0, 1)
    ) +
    scale_color_gradientn(
      name = paste0('Relative\n', metric_label),
      colors = pal,
      # limits = c(0, 1),
      # breaks = c(0, 1)
    ) +
    labs(
      x = "Population density",
      y = "Avg. social capacity"
    ) +
    theme_ctokita() +
    theme(
      axis.line = element_blank(),
      panel.border = element_rect(linewidth = 0.3),
      legend.title = element_text(margin = margin(b = 5)),
      legend.key.height = unit(2, 'mm')
    )
  return(gg_heatmap)
}

# Network density
gg_heatmap_density_relative <- plot_relative_heat_map(
  data = paramter_combination_results,
  metric_name = 'network_density',
  metric_label = 'network density',
  pal = heat_map_pal
)
gg_heatmap_density_relative
ggsave(
  gg_heatmap_density_relative,
  filename = 'output/heatmap_relative_network_density.pdf',
  width = 90, height = 45, units = 'mm',
  dpi = 400,
)

# Network diameter
gg_heatmap_diameter_relative <- plot_relative_heat_map(
  data = paramter_combination_results,
  metric_name = 'network_diameter',
  metric_label = 'network diameter',
  pal = heat_map_pal
)
gg_heatmap_diameter_relative
ggsave(
  gg_heatmap_diameter_relative,
  filename = 'output/heatmap_network_diameter_relative.pdf',
  width = 90, height = 45, units = 'mm',
  dpi = 400,
)

# Shortest path
gg_heatmap_path_relative <- plot_relative_heat_map(
  data = paramter_combination_results,
  metric_name = 'network_avg_shortest_path',
  metric_label = 'shortest path',
  pal = heat_map_pal
)
gg_heatmap_path_relative
ggsave(
  gg_heatmap_path_relative,
  filename = 'output/heatmap_shortest_path_relative.pdf',
  width = 90, height = 45, units = 'mm',
  dpi = 400,
)

# Clustering
gg_heatmap_clustering_relative <- plot_relative_heat_map(
  data = paramter_combination_results,
  metric_name = 'network_clustering_coef',
  metric_label = 'clustering',
  pal = heat_map_pal
)
gg_heatmap_clustering_relative
ggsave(
  gg_heatmap_clustering_relative,
  filename = 'output/heatmap_clustering_relative.pdf',
  width = 90, height = 45, units = 'mm',
  dpi = 400,
)

# Modularity
gg_heatmap_modularity_relative <- plot_relative_heat_map(
  data = paramter_combination_results,
  metric_name = 'network_modularity',
  metric_label = 'modularity',
  pal = heat_map_pal
)
gg_heatmap_modularity_relative
ggsave(
  gg_heatmap_modularity_relative,
  filename = 'output/heatmap_modularity_relative.pdf',
  width = 90, height = 45, units = 'mm',
  dpi = 400,
)

# Assortativity
gg_heatmap_assortativity_relative <- plot_relative_heat_map(
  data = paramter_combination_results,
  metric_name = 'network_assortativity',
  metric_label = 'assortativity',
  pal = heat_map_pal
)
gg_heatmap_assortativity_relative
ggsave(
  gg_heatmap_assortativity_relative,
  filename = 'output/heatmap_assortativity_relative.pdf',
  width = 90, height = 45, units = 'mm',
  dpi = 400,
)


##########################
# PLOT: Grid heat map of relative values
##########################
# Make labeled, legend-free versions of each plot
gg_density_lab_relative <- gg_heatmap_density_relative +
  labs(title = "Network density", fill = NULL) +
  theme_ctokita() +
  theme(
    legend.position = "none",
    axis.line = element_blank(),
    panel.border = element_rect(linewidth = 0.3)
  )

gg_diameter_lab_relative <- gg_heatmap_diameter_relative +
  labs(title = "Network diameter", fill = NULL) +
  theme_ctokita() +
  theme(
    legend.position = "none",
    axis.line = element_blank(),
    panel.border = element_rect(linewidth = 0.3)
  )

gg_clustering_lab_relative <- gg_heatmap_clustering_relative +
  labs(title = "Clustering coefficient", fill = NULL) +
  theme_ctokita() +
  theme(
    legend.position = "none",
    axis.line = element_blank(),
    panel.border = element_rect(linewidth = 0.3)
  )

gg_modularity_lab_relative <- gg_heatmap_modularity_relative +
  labs(title = "Modularity", fill = NULL) +
  theme_ctokita() +
  theme(
    legend.position = "none",
    axis.line = element_blank(),
    panel.border = element_rect(linewidth = 0.3)
  )

gg_path_lab_relative <- gg_heatmap_path_relative +
  labs(title = "Avg. Shortest Path", fill = NULL) +
  theme_ctokita() +
  theme(
    legend.position = "none",
    axis.line = element_blank(),
    panel.border = element_rect(linewidth = 0.3)
  )

gg_assort_lab_relative <- gg_heatmap_assortativity_relative +
  labs(title = "Assortativity", fill = NULL) +
  theme_ctokita() +
  theme(
    legend.position = "none",
    axis.line = element_blank(),
    panel.border = element_rect(linewidth = 0.3)
  )

# Arrange into a separate grid plot (no color bars)
gg_heatmap_grid_relative <-
  (gg_density_lab_relative + gg_diameter_lab_relative + gg_path_lab_relative +
     gg_clustering_lab_relative + gg_modularity_lab_relative + gg_assort_lab_relative) +
  plot_layout(ncol = 3)

gg_heatmap_grid_relative
ggsave(gg_heatmap_grid_relative, filename = 'output/full_parameter_sweep_plot_relative.pdf', width=180, height=100, units='mm')

