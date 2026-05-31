###################################################
#
# Plotting supplemental metrics for example networks
#
###################################################

require(dplyr)
require(ggplot2)
require(scales)
require(viridisLite)
source("_plot_themes/theme_ctokita.R")


##########################
# Define plot features
##########################
qual_pal <- mako(9)
low_pal <- qual_pal[7]
high_pal <- qual_pal[4]

# Low density network color pal
lowdens_pal <- mako(9)[2:8]
highdens_pal <- mako(8)[2:7]


##########################
# Load data
##########################
# Get files
data_dir <- "data_derived/full_social_networks/"
edgelist_files <- list.files(data_dir, pattern = "^edgelist-")

# Loop through example networks
degree_data <- data.frame()
for (file in edgelist_files) {

  # Grab density and replicate
  density <- as.numeric( gsub(".*density_([0-9.]+)-.*", "\\1", file, perl = TRUE) )
  replicate <- as.numeric( gsub(".*replicate_([0-9]+)\\.csv", "\\1", file, perl = TRUE) )

  # Load edgelist and nodelist
  edgelist <- read.csv(paste0(data_dir, file), header = TRUE)
  nodelist_file <- gsub("^edgelist", "nodelist", file)
  nodelist <- read.csv(paste0(data_dir, nodelist_file), header = TRUE)

  # Calculate degree from edgelist (times each node appears as source or target)
  edge_counts <- table(c(edgelist$source, edgelist$target))
  degrees <- as.integer(edge_counts[match(nodelist$id, names(edge_counts))])
  degrees[is.na(degrees)] <- 0L

  degrees_df <- data.frame(
    population_density = density,
    replicate = replicate,
    node = nodelist$id,
    location_x = nodelist$x,
    location_y = nodelist$y,
    degree = degrees
  )

  # Add to dataframe
  degree_data <- rbind(degree_data, degrees_df)
  rm(edgelist, nodelist, edge_counts, degrees, degrees_df)
}

# Make degree character for plotting
degree_data$population_density_label <- degree_data$population_density
degree_data$population_density_label <- gsub("1e-04", "0.0001", degree_data$population_density_label)
degree_data$population_density_label <- gsub("10000", "10,000", degree_data$population_density_label)



##########################
# Plot degree distribution
##########################
gg_degree_dist <- ggplot(data = degree_data %>% filter(population_density_label %in% c("0.0001", "10,000")), aes(x = degree, fill = population_density_label, color = population_density_label)) +
  geom_histogram(
    aes(y = after_stat(count / sum(count))),
    breaks = seq(0, 200, 2),
    alpha = 0.25,
    position = "identity",
    colour = NA,
    width = 2
  ) +
  # Step outline using IDENTICAL bin settings
  stat_bin(
    aes(y = after_stat(count / sum(count))),
    geom = "step",
    breaks = seq(0, 200, 2),
    alpha = 0.8,
    position = position_nudge(x=-1)
  ) +
  scale_y_continuous(
    limits = c(0, 0.03),
    expand = c(0 ,0)
  ) +
  scale_x_continuous(
    limits = c(0, 150),
    breaks = seq(0, 200, 25),
    expand = c(0, 0),
  ) +
  labs(
    x = "Degree",
    y = "Frequency",
  ) +
  scale_fill_manual(
    name = "Population density",
    values = c(low_pal, high_pal)
  ) +
  scale_color_manual(
    name = "Population density",
    values = c(low_pal, high_pal)
  ) +
  theme_ctokita(color_bar = FALSE) +
  theme(
    aspect.ratio = NULL,
    legend.position = c(0.9, 0.9)
  )

gg_degree_dist
ggsave(
  gg_degree_dist,
  filename = 'output/example_degree_distribution.pdf',
  width = 90, height = 45, units = 'mm',
  dpi = 400,
)

