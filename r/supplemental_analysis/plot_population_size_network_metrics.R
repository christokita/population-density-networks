###################################################
#
# Plotting density-driven network metrics relative to population size (N)
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
n_pal <- rev(rocket(6)[2:5])
cap_pal <- rev(rocket(9)[2:7])
edge_pal <- rocket(6)[c(4, 3, 2)]

qual_pal <- mako(9)
predictor_pal <- c("size" = qual_pal[4], "capacity" = qual_pal[7])

density_label <- expression("Population density, " * delta * italic(r)^2)
population_size_label <- "Population\nsize, N"
capacity_label <- "Social\ncapacity, μc"
density_per_n_label <- "Population density relative\nto population size, δr²/N"
density_per_capacity_label <- expression("Population density, " * delta * italic(r)^2 * "/" * mu[c])

# Every metric recorded, plus mean degree derived from network density
metric_labels <- c(
  network_clustering_coef        = "Clustering coefficient",
  network_modularity             = "Modularity",
  network_avg_shortest_path      = "Avg. shortest path",
  network_diameter               = "Network diameter",
  network_density                = "Network density",
  mean_degree                    = "Mean degree",
  network_assortativity          = "Assortativity",
  network_num_components         = "Number of components",
  network_largest_component_size = "Largest component",
  network_is_connected           = "Fraction connected"
)

# Components, connectivity and largest-component size sit at 1 everywhere
live_metrics <- c(
  'network_clustering_coef', 'network_modularity', 'network_avg_shortest_path',
  'network_assortativity', 'mean_degree', 'network_density', 'network_diameter'
)

# A local metric, a global one, and modularity, which responds to both
focus_metrics <- c('network_clustering_coef', 'network_modularity', 'network_avg_shortest_path')

# Completion is the edge least disturbed by the drift in baseline clustering
completion_target <- 0.1

# Below mu_c ~ 20 modularity is inflated by low mean degree. Set to 5 for the whole sweep.
capacity_min <- 20


##########################
# Load simulation sweeps
##########################
# Filter on radius so a future rbind can never mix radii within one N
load_population_sweep <- function(radius_keep = 1, k_cap_keep = 50) {
  n_1000 <- read.csv('data_derived/full_parameter_sweep_results.csv') %>% 
    filter(k_cap_mean == k_cap_keep)
  n_500 <- read.csv('data_derived/sensitivity_analysis/select_density_results_N500.csv')
  n_2000 <- read.csv('data_derived/sensitivity_analysis/select_density_results_N2000.csv')
  n_5000 <- read.csv('data_derived/sensitivity_analysis/select_density_results_N5000.csv')
  
  population_sweep <- 
    n_1000 %>% 
    rbind(n_500, n_2000, n_5000) %>% 
    filter(radius == radius_keep) %>% 
    mutate(
      network_is_connected = as.numeric(network_is_connected == "True"),
      mean_degree = network_density * (n - 1),
      # Space enters the model only through delta * r^2
      population_density = population_density * radius**2
    )
  return(population_sweep)
}

# The N = 1,000 sweep across social capacity, used for the mu_c slopes
load_capacity_sweep <- function(radius_keep = 1) {
  capacity_sweep <- 
    read.csv('data_derived/full_parameter_sweep_results.csv') %>% 
    filter(radius == radius_keep) %>% 
    mutate(
      network_is_connected = as.numeric(network_is_connected == "True"),
      mean_degree = network_density * (n - 1),
      population_density = population_density * radius**2
    )
  return(capacity_sweep)
}

# Replicate counts, radii and density grid, per N
summarise_sweep_inventory <- function(data) {
  inventory <- 
    data %>% 
    group_by(n) %>% 
    summarise(
      radii = paste(sort(unique(radius)), collapse = ", "),
      k_cap = paste(sort(unique(k_cap_mean)), collapse = ", "),
      n_densities = n_distinct(population_density),
      density_min = min(population_density),
      density_max = max(population_density),
      replicates = paste(sort(unique(table(population_density))), collapse = ", "),
      .groups = "drop"
    )
  return(inventory)
}


##########################
# Normalize a metric within each group
##########################
# resample = TRUE draws replicates with replacement, for the bootstrap below
summarise_metric <- function(data, metric_name, group_var = "n", resample = FALSE) {
  metric_summary <- 
    data %>% 
    rename(value = all_of(metric_name), group = all_of(group_var)) %>% 
    group_by(group, population_density) %>% 
    summarise(
      mean = if (resample) mean(value[sample.int(n(), n(), replace = TRUE)]) else mean(value),
      sd = sd(value),
      replicates = n(),
      .groups = "drop"
    ) %>% 
    mutate(sem = sd / sqrt(replicates))
  return(metric_summary)
}

# Curves run from 1 at the low-density plateau to 0 at the high-density plateau
normalize_curve <- function(metric_summary) {
  normalized <- 
    metric_summary %>% 
    group_by(group) %>% 
    arrange(population_density, .by_group = TRUE) %>% 
    mutate(
      plateau_low  = mean(head(mean, 3)),
      plateau_high = mean(tail(mean, 3)),
      span         = plateau_low - plateau_high,
      norm_mean    = (mean - plateau_high) / span,
      norm_lower   = (mean - sem - plateau_high) / span,
      norm_upper   = (mean + sem - plateau_high) / span
    ) %>% 
    ungroup()
  return(normalized)
}

normalize_metric <- function(data, metric_name, group_var = "n") {
  normalized <- 
    summarise_metric(data, metric_name, group_var) %>% 
    normalize_curve() %>% 
    mutate(metric = metric_name)
  return(normalized)
}

# Is there a crossover to find, and has the sweep actually flattened at both ends?
check_plateaus <- function(data, metrics = names(metric_labels)) {
  plateau_checks <- 
    lapply(metrics, function(metric_name) {
      normalize_metric(data, metric_name) %>% 
        group_by(metric, group) %>% 
        arrange(population_density, .by_group = TRUE) %>% 
        summarise(
          plateau_low  = first(plateau_low),
          plateau_high = first(plateau_high),
          # Dynamic range against replicate noise: below ~3 there is no usable signal
          range_over_noise = abs(first(span)) / mean(sd),
          # Spread of the three points defining each plateau, as a fraction of the
          # whole curve. Small means the sweep really has flattened out.
          low_flatness  = (max(head(mean, 3)) - min(head(mean, 3))) / abs(first(span)),
          high_flatness = (max(tail(mean, 3)) - min(tail(mean, 3))) / abs(first(span)),
          .groups = "drop"
        )
    }) %>% 
    bind_rows() %>% 
    rename(n = group)
  return(plateau_checks)
}


##########################
# Locate the crossover edges
##########################
# First crossing walking up in density. Sorting by normalized value instead can
# report an edge from the wrong side of a non-monotonic curve.
first_crossing <- function(density, norm_value, target) {
  crosses <- (head(norm_value, -1) - target) * (tail(norm_value, -1) - target) <= 0 &
    head(norm_value, -1) != tail(norm_value, -1)
  i <- which(crosses)
  if (length(i) == 0) {
    return(NA_real_)
  }
  i <- i[1]
  log_density <- log10(density[i]) +
    (target - norm_value[i]) / (norm_value[i + 1] - norm_value[i]) *
    (log10(density[i + 1]) - log10(density[i]))
  return(log_density)
}

edges_from_curve <- function(normalized, targets) {
  edges <- 
    normalized %>% 
    group_by(group) %>% 
    arrange(population_density, .by_group = TRUE) %>% 
    group_modify(~ tibble(
      target = targets,
      log_density = sapply(targets, function(target) {
        first_crossing(.x$population_density, .x$norm_mean, target)
      })
    )) %>% 
    ungroup()
  return(edges)
}

find_edges <- function(data, metric_name, targets = c(0.9, 0.5, 0.1), group_var = "n") {
  edges <- 
    normalize_metric(data, metric_name, group_var) %>% 
    edges_from_curve(targets) %>% 
    mutate(metric = metric_name)
  names(edges)[names(edges) == "group"] <- group_var
  return(edges)
}

# Resample runs within each group, rebuild the curve, relocate the edges
bootstrap_edges <- function(data, metric_name, targets = c(0.9, 0.5, 0.1),
                            group_var = "n", B = 500, seed = 42) {
  set.seed(seed)
  boot_draws <- 
    lapply(1:B, function(b) {
      summarise_metric(data, metric_name, group_var, resample = TRUE) %>% 
        normalize_curve() %>% 
        edges_from_curve(targets) %>% 
        mutate(metric = metric_name, boot = b)
    }) %>% 
    bind_rows()
  names(boot_draws)[names(boot_draws) == "group"] <- group_var
  return(boot_draws)
}

# Slope of an edge against its predictor on log-log axes
fit_edge_slopes <- function(edges, predictor = "n") {
  slopes <- 
    edges %>% 
    rename(predictor_value = all_of(predictor)) %>% 
    filter(!is.na(log_density)) %>% 
    group_by(metric, target) %>% 
    summarise(
      slope = coef(lm(log_density ~ log10(predictor_value)))[2],
      r2 = summary(lm(log_density ~ log10(predictor_value)))$r.squared,
      .groups = "drop"
    )
  return(slopes)
}

# Bootstrap confidence interval on that slope
bootstrap_slopes <- function(boot_draws, predictor = "n") {
  slope_ci <- 
    boot_draws %>% 
    rename(predictor_value = all_of(predictor)) %>% 
    group_by(metric, target, boot) %>% 
    summarise(
      slope = if (sum(!is.na(log_density)) > 2) {
        coef(lm(log_density ~ log10(predictor_value)))[2]
      } else {
        NA_real_
      },
      .groups = "drop"
    ) %>% 
    group_by(metric, target) %>% 
    summarise(
      slope_lower = quantile(slope, 0.025, na.rm = TRUE),
      slope_upper = quantile(slope, 0.975, na.rm = TRUE),
      .groups = "drop"
    )
  return(slope_ci)
}

# Spread of each edge across N, in decades. Smaller is a better collapse.
compare_collapse <- function(edges) {
  collapse <- 
    edges %>% 
    group_by(metric, target) %>% 
    summarise(
      spread_density = max(log_density) - min(log_density),
      spread_density_per_n = max(log_density - log10(n)) - min(log_density - log10(n)),
      better_axis = ifelse(spread_density < spread_density_per_n, "delta r^2", "delta r^2 / N"),
      .groups = "drop"
    )
  return(collapse)
}

# Width of the crossover, onset to completion, and how it changes with N
crossover_width <- function(edges) {
  widths <- 
    edges %>% 
    filter(target %in% c(0.9, 0.1)) %>% 
    select(metric, n, target, log_density) %>% 
    pivot_wider(names_from = target, values_from = log_density, names_prefix = "edge_") %>% 
    mutate(width = edge_0.1 - edge_0.9)
  return(widths)
}


##########################
# Plotting function
##########################
# Custom function to plot the metric of choice, one line per population size
plot_normalized_sweep <- function(data, metric_name, metric_label, x_axis = "density", pal = n_pal) {
  # Grab metric and set the x-axis under test
  data <- 
    data %>% 
    filter(metric == metric_name) %>% 
    mutate(
      n_numeric = as.numeric(as.character(group)),
      scaled_density = switch(
        x_axis,
        density       = population_density,
        density_per_n = population_density / n_numeric
      ),
      n = gsub("000", ",000", as.character(group)),
      n = factor(n, levels = c("500", "1,000", "2,000", "5,000"))
    )
  
  x_label <- switch(
    x_axis,
    density       = density_label,
    density_per_n = density_per_n_label
  )
  
  # Create plot
  gg_metric_plot <-
    ggplot(data, aes(x = scaled_density, y = norm_mean, color = n, fill = n, group = n)) +
    geom_ribbon(aes(ymin = norm_lower, ymax = norm_upper), color = NA, alpha = 0.2) +
    geom_line(linewidth = 0.6) +
    geom_point(stroke = 0, size = 2) +
    scale_x_log10(
      breaks = 10**seq(-10, 6, 2),
      expand = c(0, 0),
      labels = trans_format("log10", math_format(10^.x))
    ) +
    scale_y_continuous(
      expand = c(0, 0)
    ) +
    scale_color_manual(
      name = population_size_label,
      values = pal
    ) +
    scale_fill_manual(
      name = population_size_label,
      values = pal
    ) +
    coord_cartesian(clip = "off") +
    labs(
      x = x_label,
      y = metric_label
    ) +
    theme_ctokita() +
    theme(axis.title.x = element_text(lineheight = 0.9)) +
    guides(
      fill = guide_legend(),
      color = guide_legend()
    )
  return(gg_metric_plot)
}


##########################
# Load data and locate the crossover edges
##########################
population_sweep <- load_population_sweep()
capacity_sweep <- load_capacity_sweep()

# Data checks: radii, replicate counts, and whether both plateaus are reached
sweep_inventory <- summarise_sweep_inventory(population_sweep)
plateau_checks <- check_plateaus(population_sweep)

# Crossover edges for every metric, and which x-axis collapses them across N
all_edges <- 
  lapply(live_metrics, function(metric_name) find_edges(population_sweep, metric_name)) %>% 
  bind_rows()

axis_collapse <- compare_collapse(all_edges)
n_slopes <- fit_edge_slopes(all_edges)

# Crossover width, and how far it widens per decade of N
widths <- crossover_width(all_edges %>% filter(metric %in% focus_metrics))
width_scaling <- 
  widths %>% 
  group_by(metric) %>% 
  summarise(
    width_per_decade_of_n = coef(lm(width ~ log10(n)))[2],
    r2 = summary(lm(width ~ log10(n)))$r.squared,
    .groups = "drop"
  )


##########################
# Completion-edge scaling against N and against social capacity
##########################
# Two one-dimensional cuts: N slopes use mu_c = 50, mu_c slopes use N = 1,000.
# There is no N x mu_c grid, so this does not test an interaction.
size_boot <- 
  lapply(focus_metrics, function(metric_name) {
    bootstrap_edges(population_sweep, metric_name, group_var = "n", B = 500)
  }) %>% 
  bind_rows()

capacity_boot <- 
  lapply(focus_metrics, function(metric_name) {
    bootstrap_edges(
      capacity_sweep %>% filter(k_cap_mean >= capacity_min),
      metric_name, group_var = "k_cap_mean", B = 200
    )
  }) %>% 
  bind_rows()

capacity_edges <- 
  lapply(focus_metrics, function(metric_name) {
    find_edges(capacity_sweep, metric_name, group_var = "k_cap_mean")
  }) %>% 
  bind_rows()

completion_slopes <- 
  bind_rows(
    fit_edge_slopes(all_edges %>% filter(metric %in% focus_metrics), predictor = "n") %>% 
      left_join(bootstrap_slopes(size_boot, predictor = "n"), by = c("metric", "target")) %>% 
      mutate(predictor = "size"),
    fit_edge_slopes(capacity_edges %>% filter(k_cap_mean >= capacity_min), predictor = "k_cap_mean") %>% 
      left_join(bootstrap_slopes(capacity_boot, predictor = "k_cap_mean"), by = c("metric", "target")) %>% 
      mutate(predictor = "capacity")
  ) %>% 
  filter(target == completion_target)

# mu_c dependence levels off above ~20, so keep both ranges to hand
capacity_range_check <- 
  bind_rows(
    fit_edge_slopes(capacity_edges, predictor = "k_cap_mean") %>% 
      mutate(range = "mu_c 5-100"),
    fit_edge_slopes(capacity_edges %>% filter(k_cap_mean >= capacity_min), predictor = "k_cap_mean") %>% 
      mutate(range = paste("mu_c", capacity_min, "-100"))
  ) %>% 
  filter(target == completion_target)


##########################
# PLOT: Proposed main text figure (new Fig. 6)
##########################
normalized_curves <- 
  lapply(live_metrics, function(metric_name) normalize_metric(population_sweep, metric_name)) %>% 
  bind_rows()

# A: clustering collapses on delta r^2
gg_clustering_collapse <- plot_normalized_sweep(
  data = normalized_curves,
  metric_name = 'network_clustering_coef',
  metric_label = "Normalized clustering",
  x_axis = "density"
)

# B: modularity fans out on the same axis
gg_modularity_fan <- plot_normalized_sweep(
  data = normalized_curves,
  metric_name = 'network_modularity',
  metric_label = "Normalized modularity",
  x_axis = "density"
)

# C: modularity lines up once density is measured per capita
gg_modularity_collapse <- plot_normalized_sweep(
  data = normalized_curves,
  metric_name = 'network_modularity',
  metric_label = "Normalized modularity",
  x_axis = "density_per_n"
)

# Plot full grid
gg_population_size_figure <-
  (
    gg_clustering_collapse + gg_modularity_fan + gg_modularity_collapse
  ) +
  plot_layout(ncol = 3, guides = 'collect')

gg_population_size_figure

ggsave(
  gg_population_size_figure,
  filename = 'output/population_size_network_metrics.pdf',
  width    = 155,
  height   = 50,  #extra height to account for double x axis line in third panel
  units    = 'mm',
  dpi      = 400
)


##########################
# PLOT: Supplement, social capacity as the second knob
##########################
# The capacity_min trim is there for modularity, so clustering can use the whole sweep
capacity_curves <- 
  lapply(focus_metrics, function(metric_name) {
    normalize_metric(capacity_sweep, metric_name, group_var = "k_cap_mean")
  }) %>% 
  bind_rows()

clustering_by_capacity <- 
  capacity_curves %>% 
  filter(metric == 'network_clustering_coef') %>% 
  mutate(capacity = as.numeric(as.character(group)))

# A: clustering fans out across social capacity on raw density
gg_capacity_fan <-
  ggplot(clustering_by_capacity, aes(x = population_density, y = norm_mean, color = capacity, fill = capacity, group = capacity)) +
  geom_ribbon(aes(ymin = norm_lower, ymax = norm_upper), color = NA, alpha = 0.2) +
  geom_line(linewidth = 0.6) +
  geom_point(stroke = 0, size = 2) +
  scale_x_log10(
    breaks = 10**seq(-10, 6, 2),
    expand = c(0, 0),
    labels = trans_format("log10", math_format(10^.x))
  ) +
  scale_y_continuous(
    expand = c(0, 0)
  ) +
  scale_color_gradientn(
    name = capacity_label,
    colors = cap_pal
  ) +
  scale_fill_gradientn(
    name = capacity_label,
    colors = cap_pal
  ) +
  coord_cartesian(clip = "off") +
  labs(
    x = density_label,
    y = "Normalized clustering"
  ) +
  theme_ctokita()

gg_capacity_fan
ggsave(
  gg_capacity_fan,
  filename = 'output/suppl_network_analysis/social_capacity_clustering_coeff.pdf',
  width    = 70,
  height   = 45,
  units    = 'mm',
  dpi      = 400
)

# B: and lines up once density is measured per unit of social capacity
gg_capacity_collapse <-
  ggplot(clustering_by_capacity, aes(x = population_density / capacity, y = norm_mean, color = capacity, fill = capacity, group = capacity)) +
  geom_ribbon(aes(ymin = norm_lower, ymax = norm_upper), color = NA, alpha = 0.2) +
  geom_line(linewidth = 0.6) +
  geom_point(stroke = 0, size = 2) +
  scale_x_log10(
    breaks = 10**seq(-10, 6, 2),
    expand = c(0, 0),
    labels = trans_format("log10", math_format(10^.x))
  ) +
  scale_y_continuous(
    expand = c(0, 0)
  ) +
  scale_color_gradientn(
    name = capacity_label,
    colors = cap_pal
  ) +
  scale_fill_gradientn(
    name = capacity_label,
    colors = cap_pal
  ) +
  coord_cartesian(clip = "off") +
  labs(
    x = "Population density relative\nto social capacity, δr²/ mu_c",
    y = "Normalized clustering"
  ) +
  theme_ctokita()

gg_capacity_collapse
ggsave(
  gg_capacity_collapse,
  filename = 'output/suppl_network_analysis/norm_social_capacity_clustering_coeff.pdf',
  width    = 70,
  height   = 46.5,
  units    = 'mm',
  dpi      = 400
)

# C: the same comparison as a slope. Avg. shortest path fits neither predictor, so leave it out
# Slopes are plotted in log units but labelled as multipliers, so 1x is no shift
shift_metrics <- c('network_clustering_coef', 'network_modularity')

completion_shift <- 
  completion_slopes %>% 
  filter(metric %in% shift_metrics) %>% 
  mutate(
    predictor = factor(predictor, levels = c("size", "capacity")),
    metric_label = factor(metric_labels[metric], levels = rev(metric_labels[shift_metrics]))
  )

gg_crossover_shift <-
  ggplot(completion_shift, aes(x = slope, y = metric_label, color = predictor)) +
  geom_vline(xintercept = 0, linetype = "solid", linewidth = 0.3, color = "grey50") +
  geom_linerange(
    aes(xmin = 0, xmax = slope),
    orientation = "y",
    linewidth = 0.6,
    position = position_dodge(width = 0.6)
  ) +
  geom_point(stroke = 0, size = 2.5, position = position_dodge(width = 0.6)) +
  scale_x_continuous(
    breaks = log10(c(0.1, 0.2, 0.5, 1, 2, 5, 10)),
    labels = c("0.1×", "0.2×", "0.5×", "1×", "2×", "5×", "10×"),
    limits = c(-1, 1)
  ) +
  scale_color_manual(
    name = NULL,
    values = predictor_pal,
    labels = c(
      size = expression("Population size, " * italic(N)),
      capacity = expression("Social capacity, " * mu[c])
    )
  ) +
  coord_cartesian(clip = "off") +
  labs(
    x = "Shift in population density needed\nto complete network restructuring\n(per 10-fold increase)",
    y = NULL
  ) +
  theme_ctokita() +
  theme(axis.title.x = element_text(lineheight = 0.9)) +
  guides(
    color = guide_legend()
  )

gg_crossover_shift
ggsave(
  gg_crossover_shift,
  filename = 'output/suppl_network_analysis/network_crossover_shift.pdf',
  width    = 90,
  height   = 48.5,
  units    = 'mm',
  dpi      = 400
)


##########################
# PLOT: Supplement, crossover width against population size
##########################
width_by_metric <- 
  widths %>% 
  mutate(metric_label = factor(metric_labels[metric], levels = metric_labels[focus_metrics]))

gg_width <-
  ggplot(width_by_metric, aes(x = n, y = width, color = metric_label, group = metric_label)) +
  geom_line(linewidth = 0.6) +
  geom_point(stroke = 0, size = 2) +
  scale_x_log10(
    breaks = c(500, 1000, 2000, 5000),
    labels = c("500", "1,000", "2,000", "5,000")
  ) +
  scale_y_continuous(
    limits = c(0, NA)
  ) +
  scale_color_manual(
    name = NULL,
    values = edge_pal
  ) +
  coord_cartesian(clip = "off") +
  labs(
    x = expression("Population size, " * italic(N)),
    y = "Crossover width (decades)"
  ) +
  theme_ctokita() +
  guides(
    color = guide_legend()
  )

gg_width

# ggsave(
#   gg_width,
#   filename = 'output/suppl_network_analysis/population_size_crossover_width.pdf',
#   width    = 75,
#   height   = 45,
#   units    = 'mm',
#   dpi      = 400
# )


##########################
# PLOT: Supplement, every metric on both candidate axes
##########################
gg_all_metrics_density <- 
  wrap_plots(lapply(live_metrics, function(metric_name) {
    plot_normalized_sweep(
      data = normalized_curves,
      metric_name = metric_name,
      metric_label = metric_labels[[metric_name]],
      x_axis = "density"
    )
  })) +
  plot_layout(ncol = 3, guides = 'collect')

gg_all_metrics_per_n <- 
  wrap_plots(lapply(live_metrics, function(metric_name) {
    plot_normalized_sweep(
      data = normalized_curves,
      metric_name = metric_name,
      metric_label = metric_labels[[metric_name]],
      x_axis = "density_per_n"
    )
  })) +
  plot_layout(ncol = 3, guides = 'collect')

gg_all_metrics_density
gg_all_metrics_per_n

# ggsave(
#   gg_all_metrics_density,
#   filename = 'output/suppl_network_analysis/population_size_all_metrics.pdf',
#   width    = 180,
#   height   = 125,
#   units    = 'mm',
#   dpi      = 400
# )
# ggsave(
#   gg_all_metrics_per_n,
#   filename = 'output/suppl_network_analysis/population_size_all_metrics_per_n.pdf',
#   width    = 180,
#   height   = 125,
#   units    = 'mm',
#   dpi      = 400
# )
