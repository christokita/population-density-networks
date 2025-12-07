##############################
#
# Custom theme for use in plots generated using ggplot2
#
##############################

library(ggplot2)

####################
# My preferred theme
####################
theme_ctokita <- function(base_font_size = 6, base_font_color = "black", base_font_family = "Helvetica", color_bar = TRUE) {
  return_list <- list(
    theme_classic() +
    theme(
      # Axis settings
      axis.text       = element_text(size = base_font_size, color = base_font_color, family = base_font_family),
      axis.title      = element_text(size = base_font_size+1, color = base_font_color, family = base_font_family),
      axis.ticks      = element_line(linewidth = 0.3, color = base_font_color),
      axis.line       = element_line(linewidth = 0.3),
      
      # Title settings
      plot.title = element_text(size = base_font_size+2, color = base_font_color, family = base_font_family, face = 'bold', hjust = 0.5, margin = margin(b = 2)),
      
      # Legend settings
      legend.title      = element_text(size = base_font_size+1, family = base_font_family, margin = margin(b = 2)),
      legend.text       = element_text(size = base_font_size, color = base_font_color, family = base_font_family),
      legend.background = element_blank(),
      legend.text.align = 0,
      legend.key.size  = unit(3, "mm"),
      legend.key.width = unit(2, "mm"),
      legend.margin = margin(t = 0, r = -10, b = 0, l = 0),
      
      # Panel/strip settings
      strip.text      = element_text(size = base_font_size+1, color = base_font_color, family = base_font_family),
      strip.background = element_blank(),
      
      # General plot settings
      panel.background = element_blank(),
      aspect.ratio    = 1
    ),
    guides(
      fill = guide_colorbar(
        frame.colour   = "black",
        frame.linewidth = 0.3,
        ticks = TRUE,
        ticks.colour = "black",
        ticks.linewidth = 0.3
      ),
      colour = guide_colorbar(
        frame.colour   = "black",
        frame.linewidth = 0.3,
        ticks = TRUE,
        ticks.colour = "black",
        ticks.linewidth = 0.3
      )
    )
  )
  
  # Return theme depending on condition of plot
  if (color_bar == FALSE) {
    return_list[1]
  } else {
    return_list
  }
}

