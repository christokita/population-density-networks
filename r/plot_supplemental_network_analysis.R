###################################################
#
# Plotting supplemental network metrics from simulations
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
# PLOT: Network metrics under different interaction radius
##########################
# Select social capacity from main set
regular_radius <- read.csv('data_derived/full_parameter_sweep_results.csv') %>% 
  filter(k_cap_mean == 50)


# Read in simulations of other interaction radii
smaller_radius <- read.csv('data_derived/sensitivity_analysis/select_density_results_radius0.1.csv')
larger_radius <- read.csv('data_derived/sensitivity_analysis/select_density_results_radius10.csv')

radius_simulation_results <- 
  regular_radius %>% 
  rbind(smaller_radius) %>% 
  rbind(larger_radius) %>% 
  select(radius, k_cap_mean, population_density, network_density:network_assortativity, -network_is_connected) %>% 
  gather('metric', 'value', -radius, -k_cap_mean, -population_density) %>% 
  group_by(radius, k_cap_mean, population_density, metric) %>% 
  summarise(
    mean = mean(value),
    sd = sd(value)
  ) %>% 
  arrange(metric, population_density) %>% 
  # Normalize density
  mutate(
    population_density_just_delta = population_density,
    population_density = population_density*radius**2
  )


# Custom function to plot the metric of choice
plot_density_sweep_radius <- function(data, metric_name, metric_label, pal) {
  # Grab metric
  data <- 
    data %>% 
    filter(metric == metric_name) %>% 
    mutate(
      lower = mean - sd,
      upper = mean + sd,
      radius = as.character(radius)
    )
  
  # Create plot
  gg_metric_plot <-
    ggplot(data, aes(x=population_density, y=mean, color = radius, fill = radius, group = radius)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), color = NA, alpha = 0.2) +
    geom_line(linewidth = 0.6) +
    geom_point(stroke = 0, size = 2) +
    scale_x_log10(
      breaks = 10**seq(-6, 6, 2),
      expand = c(0, 0),
      labels = trans_format("log10", math_format(10^.x))
    ) +
    scale_y_continuous(
      expand = c(0, 0)
    ) +
    scale_color_manual(
      name = "Interaction\nradius",
      values = pal
    ) +
    scale_fill_manual(
      name = "Interaction\nradius",
      values = pal
    ) +
    coord_cartesian(clip = "off") +
    labs(
      x = expression("Population density, " * delta * italic(r)^2),
      y = metric_label
    ) +
    theme_ctokita() +
    guides(
      fill = guide_legend(),
      color = guide_legend()
    )
  return(gg_metric_plot)
}


# Individual metric plots
radius_pal <- c("#F6AA1C", plot_pal, "#220901")

gg_density_radius <- plot_density_sweep_radius(
  data = radius_simulation_results,
  metric_name = 'network_density',
  metric_label = 'Network density',
  pal = radius_pal
)

gg_diameter_radius <- plot_density_sweep_radius(
  data = radius_simulation_results,
  metric_name = 'network_diameter',
  metric_label = 'Network diameter',
  pal = radius_pal
)

gg_shortest_path_radius <- plot_density_sweep_radius(
  data = radius_simulation_results,
  metric_name = 'network_avg_shortest_path',
  metric_label = 'Avg. shortest path',
  pal = radius_pal
)

gg_clustering_radius <- plot_density_sweep_radius(
  data = radius_simulation_results,
  metric_name = 'network_clustering_coef',
  metric_label = 'Clustering coefficient',
  pal = radius_pal
)

gg_modularity_radius <- plot_density_sweep_radius(
  data = radius_simulation_results,
  metric_name = 'network_modularity',
  metric_label = 'Modularity',
  pal = radius_pal
)

gg_assortativity_radius <- plot_density_sweep_radius(
  data = radius_simulation_results,
  metric_name = 'network_assortativity',
  metric_label = 'Assortativity',
  pal = radius_pal
)

# Plot full grid
gg_density_radius_grid <-
  (
    gg_density_radius + gg_diameter_radius +
      gg_shortest_path_radius + gg_clustering_radius +
      gg_modularity_radius + gg_assortativity_radius
  ) +
  plot_layout(ncol = 2, guides = 'collect')

gg_density_radius_grid
ggsave(
  gg_density_radius_grid,
  filename = 'output/suppl_network_analysis/population_density_radius_sweep.pdf',
  width    = 115,
  height   = 135,
  units    = 'mm',
  dpi      = 400
)


# Plot inset of just delta for one metric
gg_inset <-
  # Filter to just modularity
  radius_simulation_results %>% 
  filter(metric == 'network_clustering_coef') %>% 
  mutate(
    lower = mean - sd,
    upper = mean + sd,
    radius = as.character(radius)
  ) %>% 
  # Plot
  ggplot(., aes(x=population_density_just_delta, y=mean, color=radius)) +
  geom_line(linewidth = 0.4) +
  geom_point(stroke = 0, size = 1.5) +
  scale_x_log10(
    breaks = 10**seq(-4, 4, 8
    ),
    expand = c(0, 0),
    labels = trans_format("log10", math_format(10^.x))
  ) +
  scale_y_continuous(
    expand = c(0, 0),
    breaks = seq(0, 1.0, 0.02),
  ) +
  scale_color_manual(
    name = "Interaction\nradius",
    values = radius_pal
  ) +
  scale_fill_manual(
    name = "Interaction\nradius",
    values = radius_pal
  ) +
  coord_cartesian(clip = "off") +
  labs(
    x = expression(delta),
  ) +
  theme_ctokita() +
  theme(
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    legend.position = 'None'
  )

gg_inset
ggsave(
  gg_inset,
  filename = 'output/suppl_network_analysis/population_density_radius_clustering_inset.pdf',
  width    = 22,
  height   = 22,
  units    = 'mm',
  dpi      = 400
)


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
  filename = 'output/suppl_network_analysis/proximity_vs_density.pdf',
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
  filename = 'output/suppl_network_analysis/proximity_vs_popularity.pdf',
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
  filename = 'output/suppl_network_analysis/elite_membership_predictors.pdf',
  width = 105, height = 45, units = 'mm',
  dpi = 400
)


##########################
# PLOT: Rich Club Coefficient
##########################
# Data files
data_dir <- "data_derived/full_social_networks/"
edgelist_files <- list.files(data_dir, pattern = "^edgelist-")

# Function to compute the (unnormalized) rich-club coefficient across degree thresholds
rich_club_coef <- function(g, k_values) {
  deg <- degree(g)
  edge_ends <- ends(g, E(g), names = FALSE)
  edge_min_degree <- pmin(deg[edge_ends[, 1]], deg[edge_ends[, 2]]) #edge is in club at k if both ends have degree > k
  # Counts with degree >= d (index d + 1), padded with 0 so k = max degree works
  max_d <- max(deg)
  n_ge <- c(rev(cumsum(rev(tabulate(deg + 1, nbins = max_d + 1)))), 0)
  e_ge <- c(rev(cumsum(rev(tabulate(edge_min_degree + 1, nbins = max_d + 1)))), 0)
  n_rich <- n_ge[k_values + 2] #degree > k is degree >= k + 1
  e_rich <- e_ge[k_values + 2]
  phi <- ifelse(n_rich > 1, 2 * e_rich / (n_rich * (n_rich - 1)), NA)
  return(phi)
}


# Run calculation
n_replicates <- 50
n_null <- 25 #number of rewired null networks per observed network
swaps_per_edge <- 10 #rewiring attempts = swaps_per_edge * number of edges
min_club_size <- 10 #drop thresholds where the club is too small to be meaningful
elite_frac <- 0.1 #club size used for the summary stat (top 10% by degree)
replicate_counts <- list()
set.seed(323)

pb <- txtProgressBar(min = 0, max = length(edgelist_files), style = 3)
rich_club_summary <- data.frame()
for (file_idx in seq_along(edgelist_files)) {
  file <- edgelist_files[file_idx]
  
  density <- as.numeric(gsub(".*density_([0-9.e+-]+)-.*", "\\1", file, perl = TRUE))
  replicate <- as.numeric(gsub(".*replicate_([0-9]+)\\.csv", "\\1", file, perl = TRUE))
  
  density_key <- as.character(density)
  if (is.null(replicate_counts[[density_key]])) replicate_counts[[density_key]] <- 0
  if (replicate_counts[[density_key]] >= n_replicates) next
  replicate_counts[[density_key]] <- replicate_counts[[density_key]] + 1
  
  edgelist <- read.csv(paste0(data_dir, file), header = TRUE)
  nodelist_file <- gsub("^edgelist", "nodelist", file)
  nodelist <- read.csv(paste0(data_dir, nodelist_file), header = TRUE)
  
  # Build graph (rich-club formula and swaps assume a simple graph)
  g <- graph_from_data_frame(edgelist, directed = FALSE, vertices = nodelist)
  stopifnot(is_simple(g))
  node_degrees <- degree(g)
  
  # Observed rich-club coefficient at every degree threshold
  k_values <- 0:max(node_degrees)
  n_rich <- sapply(k_values, function(k) sum(node_degrees > k))
  phi_obs <- rich_club_coef(g, k_values)
  
  # Degree-preserving null via double-edge swaps
  phi_null <- matrix(NA, nrow = n_null, ncol = length(k_values))
  for (i in 1:n_null) {
    g_null <- rewire(g, with = keeping_degseq(loops = FALSE, niter = swaps_per_edge * ecount(g)))
    phi_null[i, ] <- rich_club_coef(g_null, k_values)
  }
  null_mean <- colMeans(phi_null)
  null_sd <- apply(phi_null, 2, sd)
  null_q95 <- apply(phi_null, 2, quantile, probs = 0.95, na.rm = TRUE)
  
  rc_df <- data.frame(
    population_density = density,
    replicate = replicate,
    k = k_values,
    n_rich = n_rich,
    frac_rich = n_rich / vcount(g),
    phi_obs = phi_obs,
    phi_null = null_mean,
    rho = phi_obs / null_mean,
    z_score = (phi_obs - null_mean) / null_sd,
    exceeds_null = phi_obs > null_q95
  ) %>%
    filter(n_rich >= min_club_size)
  
  rich_club_summary <- rbind(rich_club_summary, rc_df)
  rm(g, g_null, edgelist, nodelist, phi_null, rc_df)
  setTxtProgressBar(pb, file_idx)
}
close(pb)

# --- Average normalized coefficient across replicates at each degree threshold ---
rich_club_avg <- rich_club_summary %>%
  group_by(population_density, k) %>%
  filter(n() >= 0.5 * n_replicates) %>% #only keep thresholds reached in most replicates
  summarise(
    n_networks = n(),
    # Club size (computed before frac_rich is overwritten below)
    se_frac_rich = sd(frac_rich) / sqrt(n_networks),
    frac_rich = mean(frac_rich),
    # Normalized rich-club coefficient
    n_rho = sum(!is.na(rho)),
    mean_rho = mean(rho, na.rm = TRUE),
    sd_rho = sd(rho, na.rm = TRUE),
    se_rho = sd_rho / sqrt(n_rho),
    rho_lower = mean_rho - qt(0.975, n_rho - 1) * se_rho,
    rho_upper = mean_rho + qt(0.975, n_rho - 1) * se_rho,
    frac_exceeds_null = mean(exceeds_null, na.rm = TRUE),
    .groups = "drop"
  )


# --- Plotting ---
rich_club_avg$density_factor <- factor(rich_club_avg$population_density)

# Panel A: normalized rich-club coefficient vs. fraction of population in club (for two choice densities)
gg_rich_club_frac <- 
  # Filter to exemplar densities
  rich_club_avg %>% 
  filter(population_density %in% c(1e-4, 1e4)) %>% 
  # Plot
  ggplot(., aes(x = frac_rich, y = mean_rho, color = density_factor, fill = density_factor)) +
  geom_hline(yintercept = 1, linetype = "dashed", linewidth = 0.3, color = "grey50") +
  geom_vline(xintercept = elite_frac, linetype = "dotted", linewidth = 0.3, color = "grey50") +
  geom_ribbon(aes(ymin = rho_lower, ymax = rho_upper), color = NA, alpha = 0.25) +
  geom_point(size = 1.5, stroke = 0) +
  scale_x_log10() +
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
    x = "Fraction of population in club",
    y = expression("Normalized rich-club coeff.")
  ) +
  theme_ctokita(color_bar = FALSE) +
  theme(
    legend.position = c(0.7, 0.8),
    legend.title = element_text(face = "bold")
  )

# Panel B: Sweep across density for rich-club coeff at top 10% of K
elite_rich_club <- rich_club_summary %>%
  filter(!is.na(rho)) %>%
  group_by(population_density, replicate) %>%
  summarise(
    rho_elite = approx(x = log10(frac_rich), y = rho, xout = log10(elite_frac), ties = mean)$y,
    .groups = "drop"
  ) %>%
  group_by(population_density) %>%
  summarise(
    n_reps = sum(!is.na(rho_elite)),
    mean_rho = mean(rho_elite, na.rm = TRUE),
    se_rho = sd(rho_elite, na.rm = TRUE) / sqrt(n_reps),
    rho_lower = mean_rho - qt(0.975, n_reps - 1) * se_rho,
    rho_upper = mean_rho + qt(0.975, n_reps - 1) * se_rho,
    .groups = "drop"
  )

gg_rich_club_sweep <- 
  ggplot(elite_rich_club, aes(x = population_density, y = mean_rho)) +
  geom_hline(yintercept = 1, linetype = "dashed", linewidth = 0.3, color = "grey50") +
  geom_ribbon(aes(ymin = rho_lower, ymax = rho_upper), color = NA, alpha = 0.25, fill = plot_pal) +
  geom_line(linewidth = 0.6, color = plot_pal) +
  geom_point(stroke = 0, size = 1.5, color = plot_pal) +
  scale_x_log10(
    breaks = 10**seq(-4, 4, 2),
    expand = c(0, 0),
    labels = trans_format("log10", math_format(10^.x))
  ) +
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0.95, 1.25)
  ) +
  coord_cartesian(clip = "off") +
  labs(
    x = expression("Population density, " * delta * italic(r)^2),
    y = paste0("Normalized rich-club coeff. \namong top ", elite_frac * 100, "% by degree")
  ) +
  theme_ctokita() +
  theme(axis.title.y = element_text(lineheight = 0.9))


gg_rich_club <- gg_rich_club_frac + gg_rich_club_sweep + plot_layout(ncol = 2)

gg_rich_club
ggsave(
  gg_rich_club,
  filename = 'output/suppl_network_analysis/rich_club_coefficient.pdf',
  width = 110, height = 47.5, units = 'mm',
  dpi = 400
)
