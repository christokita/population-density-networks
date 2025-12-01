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
heat_map_pal <-  rocket(9)
plot_pal <- heat_map_pal[5]


##########################
# Load data
##########################
# Get files
data_dir <- "data_derived/full_social_networks/"
data_files <- list.files(data_dir)

# Loop through example networks 
degree_data <- data.frame()
for (file in data_files) {
  
  # Grab density
  density <- as.numeric( gsub(".*density_([\\.0-9]+)-.*", "\\1", file, perl = TRUE) )
  
  # Load
  network <- read.csv(paste0(data_dir, file), header = TRUE, row.names = 1)
  names(network) <- seq(1:length(network)) - 1
  
  # Get degree
  degrees <- colSums(network)
  degrees_df <- data.frame(
    population_density = density,
    node = names(degrees),
    degree = degrees
  )
  
  # Add to dataframe
  degree_data <- rbind(degree_data, degrees_df)
  rm(degrees, degrees_df)
}

# Make degree character for plotting
degree_data$population_density <- gsub("1e-04", "0.0001", degree_data$population_density)
degree_data$population_density <- gsub("10000", "10,000", degree_data$population_density)



##########################
# Plot degree distribution
##########################
gg_degree_dist <- ggplot(data = degree_data, aes(x = degree, fill = population_density, color = population_density)) +
  geom_histogram(
    aes(y = after_stat(count / sum(count))),
    breaks = seq(0, 200, 2),
    alpha = 0.4,
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
    limits = c(0, 200),
    expand = c(0, 0),
  ) +
  labs(
    x = "Degree",
    y = "Frequency",
  ) +
  scale_fill_manual(
    name = "Population density",
    values = c("gray40", plot_pal)
  ) +
  scale_color_manual(
    name = "Population density",
    values = c("gray40", plot_pal)
  ) +
  theme_ctokita()[1] +
  theme(
    aspect.ratio = 0.33,
    legend.position = c(0.9, 0.9)
  )

gg_degree_dist
ggsave(
  gg_degree_dist,
  filename = 'output/example_degree_distribution.pdf',
  width = 135, height = 45, units = 'mm',
  dpi = 400,
)

