# Example script for making a set of nutrient-by-depth figures typically used in
# farmer reports

# Start by making data with multiple sites and 4 nutrients to be plotted. This
# step should be replaced with data import from an excel or CSV file, and data
# manipulations if needed.
#
# The structure of the data must be:
#   - One row per site/core/depth (with ID columns for each)
#   - One column per soil nutrient or property
#   - The `depth` column must be depth ranges and it should be standardized
#     across the dataset. I.e. If you are using 6 inch depth intervals up to 42
#     inches, and you have one core that could only be sampled to 40 inches, the
#     36-40 inch depth for that core should be labeled "36-42" (NOT "36-40") so
#     that it can be properly grouped with other cores' depth ranges on the
#     figure.
#   - Assure that nutrients/properties and depths are in their final reporting
#     units (e.g. change cm to inches and ppm to lb/ac for farmer reports).
data <- {tibble::tribble(
    ~site, ~core_id, ~depth_in, ~pH,  ~OM, ~P_lbA, ~K_lbA,
      "1",      "1",     "0-6", 6.5, 3.27,    162,    738,
      "1",      "1",    "6-12", 5.3, 2.67,     30,    184,
      "1",      "1",   "12-18", 5.7, 1.75,     12,    128,
      "1",      "1",   "18-24", 6.4, 1.14,      8,    130,
      "1",      "1",   "24-30", 7.2, 1.27,      6,    176,
      "1",      "1",   "30-36",   8, 0.75,      2,    140,
      "1",      "1",   "36-42", 8.3, 0.57,      4,    140,
      "1",      "2",     "0-6",   6, 4.77,     64,    286,
      "1",      "2",    "6-12", 5.7, 3.86,     26,    148,
      "1",      "2",   "12-18", 6.1, 2.98,     16,    202,
      "1",      "2",   "18-24", 6.2,  2.2,      8,    186,
      "1",      "2",   "24-30", 6.7, 1.67,      6,    198,
      "1",      "2",   "30-36", 7.1, 1.38,      8,    150,
      "1",      "2",   "36-42", 7.1, 1.17,     10,    166,
      "1",      "3",     "0-6", 6.8, 3.82,     46,    406,
      "1",      "3",    "6-12", 6.1, 3.06,     12,    160,
      "1",      "3",   "12-18", 7.9, 1.29,      4,    150,
      "1",      "3",   "18-24", 8.1, 0.52,      4,    116,
      "1",      "3",   "24-30", 8.2, 0.42,      2,    118,
      "1",      "3",   "30-36", 8.1, 0.57,      2,    138,
      "3",      "4",     "0-6", 6.6, 2.52,     52,    536,
      "3",      "4",    "6-12", 5.1, 2.35,     30,    150,
      "3",      "4",   "12-18", 6.2, 2.13,     14,    150,
      "3",      "4",   "18-24", 6.1, 1.62,     24,    182,
      "3",      "4",   "24-30", 6.6, 1.17,     36,    148,
      "3",      "4",   "30-36", 6.5, 0.95,     32,    152,
      "3",      "4",   "36-42", 6.8,  0.8,     34,    154,
      "3",      "9",     "0-6", 5.5, 1.73,     26,    318,
      "3",      "9",    "6-12",   6, 1.45,     12,    148,
      "3",      "9",   "12-18", 6.3, 0.78,     12,     78,
      "3",      "9",   "18-24", 6.8, 0.46,     20,     80,
      "3",      "9",   "24-30", 6.5, 0.42,     28,     68,
      "3",      "9",   "30-36", 6.7, 0.44,     32,     68,
      "3",      "6",     "0-6", 6.7, 2.89,     34,    484,
      "3",      "6",    "6-12", 5.5, 2.31,     24,    152,
      "3",      "6",   "12-18", 6.1,  1.4,     12,    124,
      "3",      "6",   "18-24", 6.2, 0.84,     14,    102,
      "3",      "6",   "24-30", 6.5, 0.63,     20,    102,
      "3",      "6",   "30-36", 6.6, 0.58,     24,     88,
      "3",      "6",   "36-42",   7,  0.6,     28,     96,
      "3",      "1",     "0-6", 5.3, 1.49,     32,    326,
      "3",      "1",    "6-12", 5.6, 0.71,     24,     98,
      "3",      "1",   "12-18", 5.6, 0.51,     34,    100,
      "3",      "1",   "18-24", 5.8, 0.51,     30,    106,
      "3",      "1",   "24-30", 5.7, 0.39,     28,    100,
      "3",      "1",   "30-36",   6, 0.34,     36,     94,
      "3",      "1",   "36-42", 6.1, 0.35,     40,    104,
      "3",      "3",     "0-6", 4.8, 1.91,     58,    414,
      "3",      "3",    "6-12", 4.6, 1.94,     32,    298,
      "3",      "3",   "12-18", 5.6, 2.63,     28,    196,
      "3",      "3",   "18-24", 6.1, 1.97,     16,    160,
      "3",      "3",   "24-30", 5.9,  1.4,     18,    136,
      "3",      "3",   "30-36", 6.4, 0.81,     16,     94,
      "3",      "3",   "36-42", 6.4, 0.33,     28,     58,
      "3",      "5",     "0-6", 5.9, 2.86,     48,    556,
      "3",      "5",    "6-12", 5.3, 2.58,     26,    234,
      "3",      "5",   "12-18", 6.1, 2.33,     12,    114,
      "3",      "5",   "18-24", 6.2, 2.02,      8,     98,
      "3",      "5",   "24-30", 6.5, 1.06,     20,    114,
      "3",      "5",   "30-36", 6.5, 0.33,     40,     64,
      "3",      "2",     "0-6", 6.4, 2.49,     52,    454,
      "3",      "2",    "6-12", 5.3, 2.11,     52,    132,
      "3",      "2",   "12-18",   6, 1.58,     20,     72,
      "3",      "2",   "18-24",   6,    1,     12,     56,
      "3",      "2",   "24-30", 6.3, 0.39,     20,     42,
      "3",      "2",   "30-36", 6.1, 0.25,     24,     48,
      "3",      "7",     "0-6", 5.8, 2.99,     62,    392,
      "3",      "7",    "6-12", 5.8, 2.46,     36,    210,
      "3",      "7",   "12-18", 6.2, 1.86,     10,    134,
      "3",      "7",   "18-24", 6.3, 1.38,     20,    116,
      "3",      "7",   "24-30", 6.5, 1.01,     44,    110,
      "3",      "7",   "30-36", 6.9, 0.92,     34,    110,
      "3",      "8",     "0-6",   5, 1.57,     28,    292,
      "3",      "8",    "6-12", 5.6, 1.51,      8,    126,
      "3",      "8",   "12-18", 5.9, 0.96,      8,     96,
      "3",      "8",   "18-24",   6, 0.57,     16,     90,
      "3",      "8",   "24-30", 6.1, 0.63,     20,     84,
      "3",      "8",   "30-36", 6.3, 0.69,     16,     96,
      "3",      "8",   "36-42", 6.8, 0.87,     30,    132
)}
head(data)

# Change depth range column to factor and order the levels starting with the
# **DEEPEST DEPTH**. If depth levels are already in order in the data, you can
# use unique() and rev() as below. If not, you'll need to specify the levels
# manually using something like `levels <- c("12-18", "6-12", "0-6")`.
levels <- unique(data$depth_in)
levels # notice, shallowest depth is first, need to reverse
levels <- rev(levels)
levels # this order is correct with deepest first
data$depth_in <- factor(data$depth_in, levels = rev(unique(data$depth_in)))

# Get the nutrient by depth function
github_url <- "https://github.com/redseasoils/example_scripts/blob/main/functions/nutrient_by_depth_plot.R"
devtools::source_url(paste0(github_url, "?raw=TRUE"))
?nutrient_by_depth_plot # view function documentation

# Prepare arguments
#
# data variables to plot
nutrients <- c("pH", "OM", "P_lbA", "K_lbA")
# axis labels for each nutrient
nutrient_labs <- c("pH", "OM", "P (lb/ac)", "K (lb/ac)")
# data variables to exclude from stock calculations
exclude_stocks <- c("pH", "OM")
# make a color palette (one color per nutrient)
colors <- RColorBrewer::brewer.pal(4, "Set2")

# Create the plots
plots <- nutrient_by_depth_plot(
  data = data, nutrients = nutrients, site_var = site, core_id_var = core_id,
  calculate_stocks = TRUE, depth_range_var = depth_in, topsoil_boundary = "0-6",
  exclude_stocks = exclude_stocks, colors = colors,
  nutrient_labs = nutrient_labs, do_plot_grid = TRUE
)
plots[[1]]
plots[[2]]
