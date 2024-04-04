# Soil properties by depth ggplot example

library(cowplot)
library(dplyr)
library(ggplot2)

set.seed(1)

# Data with multiple reps per site/depth. Site and depth should be factors, and 
# depth factor levels should be ordered greatest to least
df = data.frame(
  site = rep(c("A", "B", "C"), each = 112) %>% factor,
  depth = rep(c("0-10", "10-20", "20-30", "30-40", 
                "40-50", "50-60", "60-70", "70-80"), times = 42) %>% 
    factor(., levels = c("70-80", "60-70", "50-60", "40-50", 
                         "30-40", "20-30", "10-20", "0-10")),
  org_matter = c(rnorm(112, mean = 1), rnorm(112, mean = 2), rnorm(112, mean = 3)), 
  sand_pct = c(rnorm(112, mean = 1), rnorm(112, mean = 2), rnorm(112, mean = 3))
)

# Summarize data to one mean and se per site/depth
df_sum = df %>% 
  group_by(site, depth) %>% 
  summarize(
    across(c(org_matter, sand_pct), mean, .names = "mean_{.col}"),
    across(c(org_matter, sand_pct), ~sd(.x)/sqrt(n()), .names = "se_{.col}"),
    .groups = "drop"
  )

# I usually start with my labels and theme, then add data layers. I only put
# aesthetics in the main ggplot call that I want to apply to all data layers.
# E.g. here, all layers (points, lines, errorbars) will be grouped by site, but
# since we want all black error bars, I'll save the 'color' aesthetic for the 
# point and line layers specifically.
# We are also flipping coordinates here (switching x and y) with coord_flip.
# This just makes it more intuitive with error bars (which we usually set ymin
# and ymax for, but in this case we really want xmin and xmax). Coord_flip
# lets you use all the code you're more used to, then just flips the axes 
# for you.
om_fig = ggplot(data = df_sum, 
                  aes(x = depth, y = mean_org_matter, group = site)) + 
  theme_bw() + 
  theme(panel.grid = element_blank()) +
  labs(x = "Depth (cm)", y = expression(Organic~Matter~(g~kg^{-1}))) + 
  coord_flip() ; om_fig

# Add errorbars first so they are below points and lines
om_fig = om_fig + geom_errorbar(
  aes(ymin = mean_org_matter - se_org_matter, 
      ymax = mean_org_matter + se_org_matter), 
  width = 0.2, lwd = 0.4) ; om_fig

# Add lines
om_fig = om_fig + geom_line(aes(color = site), linetype = 2) ; om_fig

# Add points
om_fig = om_fig + geom_point(aes(color = site), size = 3) ; om_fig

# Set color scales and name of the legend
om_fig = om_fig + 
  scale_color_manual(values = c("blue", "green", "red"), name = "Site") ; om_fig

# I am not actually sure how to get the legend to have 2 dots with a connecting
# line like in the figure you sent. But I can put it horizontally on top 
om_fig = om_fig + 
  guides(color = guide_legend(nrow = 1, ncol = 3, title.position = "left")) +
  theme(legend.position = "top"); om_fig

# Repeat for sand, but I'm just going to do one chunk of code rather
# than step by step. You could write this into a function if you're doing many
# variables
sand_fig = ggplot(data = df_sum, 
                  aes(x = depth, y = mean_sand_pct, group = site)) + 
  theme_bw() + 
  theme(panel.grid = element_blank(), legend.position = "top") +
  labs(x = "Depth (cm)", y = "Sand (%)") +
  coord_flip() + 
  geom_errorbar(aes(ymin = mean_sand_pct - se_sand_pct, 
                    ymax = mean_sand_pct + se_sand_pct), width = 0.2, lwd = 0.4) +
  geom_line(aes(color = site), linetype = 2) + 
  geom_point(aes(color = site), size = 3) +
  scale_color_manual(values = c("blue", "green", "red"), name = "Site") +
  guides(color = guide_legend(nrow = 1, ncol = 3, title.position = "left"))
sand_fig

# Put figures together with labels using cowplot::plot_grid (this is my 
# preferred package for this, but there are others available if you have 
# specific needs)
plot_grid(om_fig, sand_fig, nrow = 1, ncol = 2, align = "h", 
          labels = c("(A)", "(B)"))
  # notice two legends - need to fix this

# Extract legend from one of the plots (get_legend from cowplot)
legend = get_legend(om_fig)

# Remove legend from each plot
om_fig = om_fig + theme(legend.position = "none")
sand_fig = sand_fig + theme(legend.position = "none")

# Plot grid again
fig = plot_grid(om_fig, sand_fig, nrow = 1, ncol = 2, align = "h", 
                labels = c("(A)", "(B)"))
fig
# Now add legend, with relative height 10x smaller than the figure
fig_w_legend = plot_grid(legend, fig, nrow = 2, ncol = 1, rel_heights = c(1, 10))
fig_w_legend
