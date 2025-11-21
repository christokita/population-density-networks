###################################################
#
# Plotting network metrics from simulations
#
###################################################

require(dplyr)
require(ggplot2)
require(scales)
require(patchwork)


##########################
# Load data
##########################
# Load full parameter sweep data
sweep_data <- read.csv('output/full_parameter_sweep_results.csv')



##########################
# Define custom theme
##########################
heat_map_pal <- rev( c('#ffffcc','#c7e9b4','#7fcdbb','#41b6c4','#1d91c0','#225ea8','#0c2c84') )


theme_ctokita <- function() {
  theme_classic() +
  theme(
    axis.text = element_text(color='black', size = 9),
    axis.title = element_text(size = 11),
    plot.title = element_text(size = 11, face = 'bold', hjust = 0.5),
    axis.line = element_line(linewidth=0.3),
    aspect.ratio = 1
  )
}


##########################
# PLOT: Heat map of network density
##########################
gg_heatmap_density <-
  ggplot(sweep_data, aes(x=population_density, y=k_cap_mean, fill=network_density)) +
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
    name = "Network\ndensity",
    colors = heat_map_pal
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

gg_heatmap_density


##########################
# PLOT: Heat map of network diameter
##########################
gg_heatmap_diameter <-
  ggplot(sweep_data, aes(x=population_density, y=k_cap_mean, fill=network_diameter)) +
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
    name = "Network\ndiameter",
    colors = heat_map_pal
  ) +
  labs(
    x = "Population density",
    y = "Avg. social capacity"
  ) +
  theme_ctokita() +
  theme(
    axis.line = element_blank(),
    panel.border = element_rect(linewidth = 0.3)
  )

gg_heatmap_diameter


##########################
# PLOT: Heat map of clustering coefficient
##########################
gg_heatmap_clustering <-
  ggplot(sweep_data, aes(x=population_density, y=k_cap_mean, fill=network_clustering_coef)) +
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
    name = "Clustering\ncoefficient",
    colors = heat_map_pal
  ) +
  labs(
    x = "Population density",
    y = "Avg. social capacity"
  ) +
  theme_ctokita() +
  theme(
    axis.line = element_blank(),
    panel.border = element_rect(linewidth = 0.3)
  )

gg_heatmap_clustering


##########################
# PLOT: Heat map of modularity
##########################
gg_heatmap_modularity <-
  ggplot(sweep_data, aes(x=population_density, y=k_cap_mean, fill=network_modularity)) +
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
    name = "Modularity",
    colors = heat_map_pal
  ) +
  labs(
    x = "Population density",
    y = "Avg. social capacity"
  ) +
  theme_ctokita() +
  theme(
    axis.line = element_blank(),
    panel.border = element_rect(linewidth = 0.3)
  )

gg_heatmap_modularity


##########################
# PLOT: Heat map of avg shortest path
##########################
gg_heatmap_path <-
  ggplot(sweep_data, aes(x=population_density, y=k_cap_mean, fill=network_avg_shortest_path)) +
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
    name = "Avg.\nshortest\npath",
    colors = heat_map_pal
  ) +
  labs(
    x = "Population density",
    y = "Avg. social capacity"
  ) +
  theme_ctokita() +
  theme(
    axis.line = element_blank(),
    panel.border = element_rect(linewidth = 0.3)
  )

gg_heatmap_path


##########################
# PLOT: Heat map of assortativity
##########################
gg_heatmap_assortativity <-
  ggplot(sweep_data, aes(x=population_density, y=k_cap_mean, fill=network_assortativity)) +
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
    name = "Assortativity",
    colors = heat_map_pal
  ) +
  labs(
    x = "Population density",
    y = "Avg. social capacity"
  ) +
  theme_ctokita() +
  theme(
    axis.line = element_blank(),
    panel.border = element_rect(linewidth = 0.3)
  )

gg_heatmap_assortativity


##########################
# PLOT: Grid heat map
##########################
## 1. Make labeled, legend-free versions of each plot ----
gg_density_lab <- gg_heatmap_density +
  labs(title = "Network density", fill = NULL) +
  theme(
    legend.position = "none"
  )

gg_diameter_lab <- gg_heatmap_diameter +
  labs(title = "Network diameter", fill = NULL) +
  theme(
    legend.position = "none"
  )

gg_clustering_lab <- gg_heatmap_clustering +
  labs(title = "Clustering coefficient", fill = NULL) +
  theme(
    legend.position = "none"
  )

gg_modularity_lab <- gg_heatmap_modularity +
  labs(title = "Modularity", fill = NULL) +
  theme(
    legend.position = "none"
  )

gg_path_lab <- gg_heatmap_path +
  labs(title = "Avg. Shortest Path", fill = NULL) +
  theme(
    legend.position = "none"
  )

gg_assort_lab <- gg_heatmap_assortativity +
  labs(title = "Assortativity", fill = NULL) +
  theme(
    legend.position = "none"
  )

## 2. Arrange into a separate grid plot (no color bars) ----
gg_heatmap_grid <-
  (gg_density_lab + gg_diameter_lab +  gg_path_lab +
     gg_clustering_lab + gg_modularity_lab + gg_assort_lab) +
  plot_layout(ncol = 3)

gg_heatmap_grid
