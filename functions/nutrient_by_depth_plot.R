require(docstring)
nutrient_by_depth_plot = function(
    data,
    nutrients,
    site_var = NULL,
    core_id_var = NULL,
    calculate_stocks = TRUE,
    depth_range_var = depth_in,
    topsoil_boundary = "0-6",
    exclude_stocks = NULL,
    colors = NULL,
    nutrient_labs,
    do_plot_grid = TRUE
){

  #' Create nutrient by depth plots
  #'
  #' Build vertical line plots of soil nutrients and properties at multiple depth
  #' intervals. This function is designed to handle a data frame with one row per
  #' site/core/depth. It will perform the summarization of the data and return a
  #' list of plots with one plot per site and one panel on each plot dedicated to
  #' each \code{nutrient}.
  #'
  #' @param data A data frame with one row per site/core/depth.
  #' @param nutrients A vector of column names in \code{data} which represent
  #'   nutrients or properties to be plotted.
  #' @param site_var Name of column in \code{data} containing site IDs. If
  #'   \code{NULL} (the default), data will be summarized by
  #'   \code{depth_range_var} only and output will contain only one plot.
  #' @param core_id_var Name of column in \code{data} containing core IDs.
  #'   Required.
  #' @param depth_range_var Name of column in \code{data} containing depth ranges
  #'   to be used as y axis variable in plots. Note that this variable should be
  #'   changed to an ordered factor prior to using this function, and the order of
  #'   \emph{factor levels should start at the deepest depth range}.
  #'   Required.
  #' @param calculate_stocks Logical. Should stocks of each nutrient/property be
  #'   calculated for topsoil and subsoil (the boundary of which is specified in
  #'   \code{topsoil_boundary}), and displayed on each figure panel? Defaults to
  #'   \code{TRUE}.
  #' @param topsoil_boundary Character. A value of \code{depth_range_var} in
  #'   \code{data} below which values will be totaled for "subsoil stock"
  #'   calculations. Values at this depth and depths above will be totaled for
  #'   "topsoil stock" calculations.
  #' @param exclude_stocks Names of columns in \code{data} which should be
  #'   excluded from stock calculations. Ignored if \code{calculate_stocks} is
  #'   \code{FALSE}.
  #' @param nutrient_labs Optional. Vector of labels to be used on x axis for
  #'   \code{nutrients}. If not specified, x axis labels will be \code{nutrients}.
  #' @param colors Optional vector of colors to be used for each soil nutrient or
  #'   properties points on the figure. Length should match or exceed that of
  #'   \code{nutrients}. If not specified, \code{\link[pals]{alphabet2}} will be
  #'   used.
  #' @param do_plot_grid Logical. Should all nutrient plots for a site be combined
  #'   using \code{\link[cowplot]{plot_grid}}? Defaults to TRUE.
  #'
  #' @return A list of plots, nested if \code{do_plot_grid = FALSE}
  #' @export

  suppressWarnings({
    nutrients <- names(dplyr::select(data, {{ nutrients }}))
    site_var_char <- names(dplyr::select(data, {{ site_var }}))
    core_id_var_char <- names(dplyr::select(data, {{ core_id_var }}))
    depth_range_var_char <- names(dplyr::select(data, {{ depth_range_var }}))
    exclude_stocks <- names(dplyr::select(data, {{ exclude_stocks }}))
  })

  # if(length(unique(dat[[site_var_char]]))>1) stop("Data should be for one farm only.")
  # if(length(unique(dat[[core_id_var_char]]))<2) stop("Data should contain more than one core.")
  if (any(!nutrients %in% names(data))) stop("nutrients must match names of column in data")

  # Summarize data to one observation per nutrient
  data_sum <- data
  if (length(site_var_char) > 0) {
    data_sum <- data_sum %>%
      group_by({{ site_var }}) %>%
      group_split()
  } else {
    data_sum <- list(data_sum)
  }
  data_sum <- data %>%
    group_by({{ site_var }}) %>%
    group_modify(
      \(y, ...) {y %>%
          group_by({{ depth_range_var }}) %>%
          summarize(across(all_of(nutrients), list(
            mean = ~ mean(.x, na.rm = T),
            se = ~ sd(.x, na.rm = T) / sqrt(sum(!is.na(.x)))
          )), .groups = "drop")}
    )

  # # Define title of each x axis as the name of the nutrient, add units to the end
  # title_x <- str_replace_all(nutrient, "_", " ") %>%
  #   str_replace_all(c("lbA" = "(lb/ac)", "mg.kg" = "mg/kg"))
  # title_x <- ifelse(str_detect(title_x, "^Organic"),
  #                  paste0(title_x, " (%)"), title_x)
  # nutrient_num = which(nutrient_colnames==nutrient)

  # Set unique colors for each nutrient
  # nutrient_colors <- RColorBrewer::brewer.pal(12, "Set3")
  # nutrient_colors[2] <- "#FF60B1"
  # nutrient_color <- nutrient_colors[nutrient_num]
  if (is.null(colors) & length(nutrients) > 26) {
    stop("nutrients is too long for default color palette (length 26). specify colors directly.")
  } else if (is.null(colors)) {
    colors <- pals::alphabet()[1:length(nutrients)]
  }

  # Find boundary between topsoil and subsoil
  if (calculate_stocks) {
    stocks_nms <- nutrients[!nutrients %in% exclude_stocks]
    ts_b_lvl <- which(levels(dat[[depth_range_var_char]]) == topsoil_boundary)
    # ts_b_line = ts_b_lvl - 0.5
    ts_b_lvls = levels(dat[[depth_range_var_char]])[
      ts_b_lvl:length(levels(dat[[depth_range_var_char]]))]
    # Calculate topsoil and subsoil stocks
    stocks = data %>%
      mutate(is_topsoil = ifelse({{ depth_range_var }} %in% ts_b_lvls,
                                 TRUE, FALSE)) %>%
      group_by({{ site_var }}, {{ core_id_var }}, is_topsoil) %>%
      summarize(across(all_of(stocks_nms), ~ sum(.x, na.rm = T)),
                .groups = "drop") %>%
      group_by({{ site_var }}, is_topsoil) %>%
      summarize(across(all_of(stocks_nms), ~ mean(.x, na.rm = T)),
                .groups = "drop")
    # Create labels of mean stocks in topsoil and subsoil
    stocks <- stocks %>%
      rowwise() %>% mutate(
        across(all_of(stocks_nms), list(
          tslab = ~ if_else(
            is_topsoil,
            paste0("Topsoil stock: ", sprintf("%.1f", round(.x, 1)), " lb/ac"),
            paste0("Subsoil stock: ", sprintf("%.1f", round(.x, 1)), " lb/ac"),
          )
        ))
      ) %>% ungroup()
  }

  plotlist <- list()
  sites <- unique(data[[site_var_char]])
  for (i in 1:length(sites)) {
    cur_site <- sites[i]
    cur_site_data <- data_sum %>% filter({{ site_var }} == cur_site)
    cur_site_stocks <- stocks %>% filter({{ site_var }} == cur_site)
    cur_site_plots <- list()
    for (m in 1:length(nutrients)) {
      cur_nutrient <- nutrients[m]
      cur_nutrient_data <- cur_site_data %>%
        select({{ site_var }}, {{ depth_range_var }}, contains(cur_nutrient)) %>%
        rename_with(~ ifelse(str_detect(.x, "_mean$"), "mean", .x)) %>%
        rename_with(~ ifelse(str_detect(.x, "_se$"), "se", .x)) %>%
        droplevels() %>%
        arrange({{ depth_range_var }})
      cur_nutrient_stocks <- cur_site_stocks %>%
        select({{ site_var }}, is_topsoil, matches(paste0(cur_nutrient, "_tslab"))) %>%
        rename_with(~ ifelse(str_detect(.x, "_tslab$"), "tslab", .x))

      # Calculate placement of labels on y axis
      y_min <- arrange(cur_nutrient_data, {{ depth_range_var }})
      y_min = ifelse(is.na(y_min$se),
                     y_min$mean,
                     (y_min$mean - y_min$se))
      y_min = min(y_min, na.rm = T)

      # Calculate placement of labels on x axis
      x_min = which(levels(cur_nutrient_data[[depth_range_var_char]]) ==
                      topsoil_boundary)
      x_min = x_min + 0.25
      x_max = 0.75

      # Create plot
      plt = ggplot(data = cur_nutrient_data) +
        # Lines on bottom - dotted - colored by nutrient
        geom_path(aes(y = {{ depth_range_var }}, x = mean, group = 1),
                  linetype = 2, color = colors[m]) +
        # Error bars above lines - solid - black
        geom_errorbarh(aes(y = {{ depth_range_var }},
                           xmin = mean - se,
                           xmax = mean + se),
                       height = 0.2, color = "black") +
        # Points on top - black outline - fill colored by nutrient
        geom_point(aes(y = {{ depth_range_var }}, x = mean), shape = 21, size = 3,
                   fill = colors[m], color = "black") +
        # coord_flip() +
        theme_bw() +
        theme(
          legend.position = "bottom",
          panel.grid = element_blank(),
          panel.border = element_rect(color = "black", fill = "transparent",
                                      linewidth = 1),
          axis.text.x = element_text(angle = 90),
          plot.margin = margin(5,5,5,5,"pt"),
          plot.background = element_rect(fill = "transparent",
                                         color = "transparent")
        ) +
        labs(x = ifelse(missing(nutrient_labs), cur_nutrient, nutrient_labs[m]),
             y = "Depth (in)")

      if (calculate_stocks && !cur_nutrient %in% exclude_stocks) {
        y_plc = layer_scales(plt)$x$range$range
        ts_b_lvl <- which(levels(cur_nutrient_data[[depth_range_var_char]]) == topsoil_boundary)
        ts_b_line = ts_b_lvl - 0.5
        plt = plt +
          # Horizontal dotted line separating topsoil and subsoil
          geom_hline(aes(yintercept = ts_b_line), linetype = 2) +
          # Label topsoil stock text
          geom_text(data = cur_nutrient_stocks, fontface = 'italic',
                    aes(label = tslab, y = ifelse(is_topsoil, x_min, x_max),
                        x = -Inf),
                    hjust = -0.01, size = 3)
      }
      cur_site_plots[[m]] <- plt
      names(cur_site_plots)[m] <- cur_nutrient
    }
    plotlist[[i]] <- cur_site_plots
    names(plotlist)[i] <- cur_site
  }

  if (do_plot_grid) {
    plotlist_len <- map(plotlist, length)
    plotlist_len <- map(plotlist_len, ~c(1, .x/2+1))
    plotlist <- map2(plotlist, plotlist_len, ~ map(seq_along(.x), \(z) {
      if (!z %in% .y) {.x[[z]] + theme(axis.title.y = element_blank())} else {.x[[z]]}
    }))
    plotlist <- map(plotlist, ~ cowplot::plot_grid(plotlist = .x, nrow = 2))
  }

  return(plotlist)
}

