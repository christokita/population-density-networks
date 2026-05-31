###################################################
#
# Plotting network metrics from simulations
#
###################################################

require(dplyr)
require(tidyr)
require(ggplot2)
require(igraph)
require(scales)
require(viridisLite)
require(patchwork)
source("_plot_themes/theme_ctokita.R")


##########################
# Define plot features
##########################
heat_map_pal <-  rocket(9)
plot_pal <- heat_map_pal[5]

qual_pal <- mako(9)
low_pal <- qual_pal[7]
high_pal <- qual_pal[4]

density_colors <- c("1e-04" = low_pal, "10000" = high_pal)


##########################
# PLOT: Proximity vs. popularity as predictors of connection
##########################
# Load raw network data (same approach as plot_example_network_metrics.R)
data_dir <- "data_derived/full_social_networks/"
edgelist_files <- list.files(data_dir, pattern = "^edgelist-")

# Filter to only the two densities of interest
edgelist_files <- edgelist_files[grepl("(density_0\\.0001)-|(density_10000\\.0)-", edgelist_files)]

# For each network, sample pairs and compute distance/degree vs. connection
n_replicates <- 50
n_sample <- 1.0 * (1000*999/2) #maximum connections in upper triangle
replicate_counts <- list()

pair_summary <- data.frame()
for (file in edgelist_files) {
  
  # Grab density and replicate
  density <- as.numeric(gsub(".*density_([0-9.e+-]+)-.*", "\\1", file, perl = TRUE))
  replicate <- as.numeric(gsub(".*replicate_([0-9]+)\\.csv", "\\1", file, perl = TRUE))
  
  # Skip if we already have enough replicates for this density
  density_key <- as.character(density)
  if (is.null(replicate_counts[[density_key]])) replicate_counts[[density_key]] <- 0
  if (replicate_counts[[density_key]] >= n_replicates) next
  replicate_counts[[density_key]] <- replicate_counts[[density_key]] + 1
  
  # Load edgelist and nodelist
  edgelist <- read.csv(paste0(data_dir, file), header = TRUE)
  nodelist_file <- gsub("^edgelist", "nodelist", file)
  nodelist <- read.csv(paste0(data_dir, nodelist_file), header = TRUE)
  
  # Build graph and get degrees
  g <- graph_from_data_frame(edgelist, directed = FALSE, vertices = nodelist)
  node_degrees <- degree(g)
  n <- nrow(nodelist)
  
  # Compute pairwise distance matrix
  pos <- as.matrix(nodelist[, c("x", "y")])
  dist_mat <- as.matrix(dist(pos))
  
  # Adjacency matrix
  adj_mat <- as.matrix(as_adjacency_matrix(g))
  
  # Sample a fixed number of random pairs per network
  upper_idx <- which(upper.tri(adj_mat), arr.ind = TRUE)
  sampled <- upper_idx[sample(nrow(upper_idx), n_sample, replace = FALSE), ]
  
  # Normalize distance to 0-1 within this network
  max_dist <- max(dist_mat)
  
  pairs_df <- data.frame(
    population_density = density,
    replicate = replicate,
    node_i = sampled[, 1],
    node_j = sampled[, 2],
    distance = dist_mat[sampled],
    relative_distance = dist_mat[sampled] / max_dist,
    degree_j = node_degrees[sampled[, 2]],
    connected = adj_mat[sampled]
  )
  
  pair_summary <- rbind(pair_summary, pairs_df)
  rm(g, edgelist, nodelist, pos, dist_mat, adj_mat, upper_idx, sampled, pairs_df)
}

# --- Panel A: Fraction connected by relative distance bin, per density ---
distance_binned <- pair_summary %>%
  mutate(distance_bin = cut(relative_distance, breaks = seq(0, 1, 0.05), include.lowest = TRUE)) %>%
  group_by(population_density, replicate, distance_bin) %>%
  summarise(
    bin_midpoint = mean(relative_distance),
    frac_connected = mean(connected),
    n_pairs = n(),
    .groups = 'drop'
  ) %>%
  group_by(population_density, distance_bin) %>%
  summarise(
    bin_midpoint = mean(bin_midpoint),
    mean_frac = mean(frac_connected),
    sd_frac = sd(frac_connected),
    .groups = 'drop'
  )

# --- Panel B: Fraction connected by target degree bin, per density ---
max_degree <- max(pair_summary$degree_j)
degree_breaks <- seq(0, ceiling(max_degree / 10) * 10, 10)

degree_binned <- pair_summary %>%
  mutate(degree_bin = cut(degree_j, breaks = degree_breaks, include.lowest = TRUE)) %>%
  group_by(population_density, replicate, degree_bin) %>%
  summarise(
    bin_midpoint = mean(degree_j),
    frac_connected = mean(connected),
    n_pairs = n(),
    .groups = 'drop'
  ) %>%
  group_by(population_density, degree_bin) %>%
  summarise(
    bin_midpoint = mean(bin_midpoint),
    mean_frac = mean(frac_connected),
    sd_frac = sd(frac_connected),
    .groups = 'drop'
  )

# --- Plotting ---


distance_binned$density_factor <- factor(distance_binned$population_density)
degree_binned$density_factor <- factor(degree_binned$population_density)

# Panel A: connection probability vs. relative distance
gg_distance <- ggplot(distance_binned, aes(x = bin_midpoint, y = mean_frac, color = density_factor, fill = density_factor)) +
  geom_point(data = ~filter(., density_factor == levels(density_factor)[1]), size = 1.5, stroke = 0, position = position_nudge(x = -0.01)) +
  geom_point(data = ~filter(., density_factor == levels(density_factor)[2]), size = 1.5, stroke = 0, position = position_nudge(x = 0.01)) +
  scale_color_manual(
    name = "Population\ndensity",
    values = density_colors,
    labels = c("0.0001", "10,000")
  ) +
  scale_fill_manual(
    name = "Population\ndensity", 
    values = density_colors,
    labels = c("0.0001", "10,000")
  ) +
  labs(
    x = "Relative distance between individuals",
    y = "Fraction connected"
  ) +
  theme_ctokita(color_bar = FALSE) +
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold")
  )

ggsave(
  gg_distance,
  filename = 'output/proximity_vs_density.pdf',
  width = 65, 
  height = 45, 
  units = 'mm',
  dpi = 400
)



# Combined fitgure with Panel B: connection probability vs. target degree
gg_degree <- ggplot(degree_binned, aes(x = bin_midpoint, y = mean_frac, color = density_factor, fill = density_factor)) +
  geom_point(data = ~filter(., density_factor == levels(density_factor)[1]), size = 1.5, stroke = 0, position = position_nudge(x = -1.5)) +
  geom_point(data = ~filter(., density_factor == levels(density_factor)[2]), size = 1.5, stroke = 0, position = position_nudge(x = 1.5)) +
  scale_color_manual(name = "Population density", values = density_colors) +
  scale_fill_manual(name = "Population density", values = density_colors) +
  labs(
    x = "Degree of target individual",
    y = "Fraction connected"
  ) +
  theme_ctokita() +
  theme(legend.position = "none")

gg_proximity_popularity <- (gg_distance+theme(legend.position = 'none')) + gg_degree +
  plot_layout(ncol = 2)

gg_proximity_popularity
ggsave(
  gg_proximity_popularity,
  filename = 'output/proximity_vs_popularity.pdf',
  width = 90, 
  height = 45, 
  units = 'mm',
  dpi = 400
)


##########################
# ANALYSIS: What predicts elite membership?
##########################
data_dir <- "data_derived/full_social_networks/"
edgelist_files <- list.files(data_dir, pattern = "^edgelist-")
edgelist_files <- edgelist_files[grepl("(density_0\\.0001)-|(density_10000\\.0)-", edgelist_files)]

n_replicates <- 50
replicate_counts <- list()

node_summary <- data.frame()
for (file in edgelist_files) {
  
  density <- as.numeric(gsub(".*density_([0-9.e+-]+)-.*", "\\1", file, perl = TRUE))
  replicate <- as.numeric(gsub(".*replicate_([0-9]+)\\.csv", "\\1", file, perl = TRUE))
  
  density_key <- as.character(density)
  if (is.null(replicate_counts[[density_key]])) replicate_counts[[density_key]] <- 0
  if (replicate_counts[[density_key]] >= n_replicates) next
  replicate_counts[[density_key]] <- replicate_counts[[density_key]] + 1
  
  edgelist <- read.csv(paste0(data_dir, file), header = TRUE)
  nodelist_file <- gsub("^edgelist", "nodelist", file)
  nodelist <- read.csv(paste0(data_dir, nodelist_file), header = TRUE)
  
  # Compute degree
  g <- graph_from_data_frame(edgelist, directed = FALSE, vertices = nodelist)
  node_degrees <- degree(g)
  
  # Compute spatial centrality: distance from center of the space
  center_x <- mean(range(nodelist$x))
  center_y <- mean(range(nodelist$y))
  dist_from_center <- sqrt((nodelist$x - center_x)^2 + (nodelist$y - center_y)^2)
  # Normalize to 0-1
  dist_from_center <- dist_from_center / max(dist_from_center)
  
  node_df <- data.frame(
    population_density = density,
    replicate = replicate,
    node = nodelist$id,
    degree = node_degrees,
    social_capacity = nodelist$k_limit,
    dist_from_center = dist_from_center
  )
  
  node_summary <- rbind(node_summary, node_df)
  rm(g, edgelist, nodelist, node_df)
}

# --- Correlation analysis per network ---
cor_summary <- node_summary %>%
  group_by(population_density, replicate) %>%
  summarise(
    cor_capacity = cor(degree, social_capacity),
    cor_centrality = cor(degree, -dist_from_center),  # negative so "more central" = higher
    .groups = 'drop'
  )

# Average across replicates
cor_avg <- cor_summary %>%
  group_by(population_density) %>%
  summarise(
    mean_cor_capacity = mean(cor_capacity),
    sd_cor_capacity = sd(cor_capacity),
    mean_cor_centrality = mean(cor_centrality),
    sd_cor_centrality = sd(cor_centrality),
    .groups = 'drop'
  )

print(cor_avg)


# --- Plotting: degree vs. social capacity and degree vs. spatial centrality ---
# Sample a subset for scatter plots (too many points otherwise)
plot_data <- node_summary %>%
  group_by(population_density) %>%
  slice_sample(n = 2000) %>%
  ungroup()

plot_data$density_factor <- factor(plot_data$population_density)

density_colors <- c("1e-04" = low_pal, "10000" = high_pal)

# Panel A: degree vs. social capacity
gg_capacity <- ggplot(plot_data, aes(x = social_capacity, y = degree, color = density_factor)) +
  geom_point(size = 1.5, alpha = 0.1, stroke = 0) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.6) +
  scale_y_continuous(
    breaks = seq(0, 200, 25),
    limits = c(0, 155),
    expand = c(0, 0)
  ) +
  scale_color_manual(
    name = "Population\ndensity",
    values = density_colors,
    labels = c("0.0001", "10,000")
  ) +
  labs(
    x = "Social capacity",
    y = "Degree"
  ) +
  theme_ctokita(color_bar = FALSE) +
  theme(legend.position = "none")

# Panel B: degree vs. distance from center
gg_centrality <- ggplot(plot_data, aes(x = dist_from_center, y = degree, color = density_factor)) +
  geom_point(size = 1.5, alpha = 0.1, stroke = 0) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.6) +
  scale_y_continuous(
    breaks = seq(0, 200, 25),
    limits = c(0, 135),
    expand = c(0, 0)
  ) +
  scale_color_manual(
    name = "Population\ndensity",
    values = density_colors,
    labels = c("0.0001", "10,000")
  ) +
  labs(
    x = "Distance from center (normalized)",
    y = "Degree"
  ) +
  theme_ctokita(color_bar = FALSE) +
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold")
  )

gg_elite <- gg_capacity + gg_centrality + plot_layout(ncol = 2)

gg_elite
ggsave(
  gg_elite,
  filename = 'output/elite_membership_predictors.pdf',
  width = 105, height = 45, units = 'mm',
  dpi = 400
)
