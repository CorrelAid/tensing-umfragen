library(ggplot2)
library(dplyr)
library(ggtext)

ts_no_data_plot <- function(message = "Keine Daten verfügbar") {
  ggplot2::ggplot(
    data = data.frame(x = 0.5, y = 0.5, label = message),
    mapping = ggplot2::aes(x = x, y = y, label = label)
  ) +
    ggplot2::geom_text(fontface = "bold", size = 5, colour = "#555555") +
    ggplot2::xlim(0, 1) +
    ggplot2::ylim(0, 1) +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::theme_void()
}

ts_no_data_girafe <- function(message = "Keine Daten verfügbar") {
  ggiraph::girafe(
    ggobj = ts_no_data_plot(message),
    options = list(
      ggiraph::opts_toolbar(saveaspng = TRUE)
    )
  )
}

#' some functions for plots that need to be created multiple times

aussage_bar_chart <- function(data_long, var_filter, var_value) {
  plot_data <- data_long %>%
    dplyr::filter(.data[[var_filter]] == var_value, !is.na(value)) %>%
    dplyr::group_by(aussage, q_label, value, ch_label) %>%
    dplyr::summarize(n = n(),percent = n / length(unique(data_long$tn_id)))

  title <- unique(plot_data$q_label)

  p <- ggplot(plot_data, aes(x = value, fill = value, y = percent)) +
    ggiraph::geom_col_interactive(
      aes(
        tooltip = paste0(ch_label, " (", value, ")\n", scales::percent(percent, accuracy = 0.1)),
        data_id = paste(aussage, value, sep = "-")
      )
    ) +
    scale_fill_manual(
      name = "",
      values = setNames(rev(pal_yes_no_6), 1:6),
      breaks = c(1, 6),
      labels = c(
        "1 = trifft überhaupt nicht zu",
        "6 = trifft voll und ganz zu"
      )
    ) +
    scale_y_continuous(labels = scales::label_percent(), limits = c(0, 1)) +
    labs(y = "% Aktive", title = str_wrap(title, 80), x = "Bewertung") +
    theme(
      plot.title = element_textbox_simple(
        margin = grid::unit(c(5, 0, 20, 0), "pt"),
        halign = 0.5,
        family = TS_FONT_FAMILY
      ),
      legend.position = "bottom"
    )

  ggiraph::girafe(
    ggobj = p,
    options = list(
      ggiraph::opts_toolbar(saveaspng = TRUE)
    )
  )
}

# rev_x so far always true
eig_bar_chart <- function(plot_data, rev_x = TRUE) {
  title <- unique(plot_data$eig_label)

  # reverses the x-axis levels
  plot_data$choice_label <- factor(
    plot_data$choice_label,
    levels = rev(levels(plot_data$choice_label))
  )
  
  lvls <- levels(plot_data$choice_label)
  end_levels <- lvls[c(1, length(lvls))]

  # add zero-percent rows for missing end levels so they still get colours in the legend
  missing_end_levels <- setdiff(end_levels, as.character(plot_data$choice_label))
  if (length(missing_end_levels) > 0) {
    extra_rows <- tibble::tibble(
      percent = 0,
      choice_label = factor(missing_end_levels, levels = lvls)
    )
    # make sure class matches (ordered vs factor)
    if (is.ordered(plot_data$choice_label)) {
      extra_rows$choice_label <- ordered(extra_rows$choice_label, levels = lvls)
    }
    plot_data <- dplyr::bind_rows(plot_data, extra_rows)
  }

  colors <- rev(pal_yes_no_6) |> set_names(levels(plot_data$choice_label))

  # swap the (1) and (6) labels for the legend
  legend_labels <- stringr::str_replace(
    end_levels,
    "(?<=\\()([16])(?=\\)$)",
    function(x) ifelse(x == "1", "6", "1")
  )

  p <- ggplot(
    plot_data,
    aes(y = percent, fill = choice_label, x = choice_label)
  ) +
    geom_col() +
    # ggiraph::geom_col_interactive(
    #   aes(
    #     tooltip = paste0(
    #       choice_label, "\n",
    #       scales::percent(percent, accuracy = 0.1)
    #     ),
    #     data_id = choice_label
    #   )
    # ) +
    scale_fill_manual(
      name   = "",
      values = colors,
      breaks = end_levels,
      labels = legend_labels,
      drop = FALSE
    ) +
    scale_x_discrete(
      drop = FALSE,
      # labels = function(x) sub(".*\\((\\d+)\\)$", "\\1", x)
      labels = function(x) as.character(seq_along(x)) # sets x-axis labels to 1-6
    ) +
    scale_y_continuous(labels = scales::label_percent(), limits = c(0, 1)) +
    labs(
      x = "Bewertung",
      title = title,
      fill = NULL,
      y = "% Aktive"
    ) +
    # guides(fill = "none") +
    theme(
      plot.subtitle = element_textbox_simple(),
      plot.title = element_textbox_simple(
        margin = unit(c(0, 0, 10, 0), "pt"),
        halign = 0.5,
        family = TS_FONT_FAMILY
      ),
      # axis.text.x = element_text(size = 10.5)
      legend.position = "bottom"
    )
  ggiraph::girafe(
    ggobj = p,
    options = list(
      ggiraph::opts_toolbar(saveaspng = TRUE)
    )
  )
}


# Trends.qmd page functions (for now)
# ---- helpers used by all year_* charts ----
.ts_assert_cols <- function(data, cols) {
  miss <- setdiff(cols, names(data))
  if (length(miss)) stop("Missing columns: ", paste(miss, collapse = ", "))
}

.ts_factor_year <- function(x) factor(x, levels = sort(unique(x)))

.ts_palette_named <- function(levels, palette = NULL) {
  base <- if (is.null(palette)) pal_8 else unname(palette)
  L <- length(base)
  idx <- ((seq_along(levels) - 1) %% L) + 1
  vals <- base[idx]
  names(vals) <- levels
  vals
}

.ts_theme <- function() {
      base <- if (exists("theme_ts", inherits = TRUE)) theme_ts else theme_minimal()
      base + ggplot2::theme(
        legend.position   = "bottom",
        legend.text       = ggplot2::element_text(size = 9),
        legend.key.width  = grid::unit(10, "pt"),
        legend.key.height = grid::unit(10, "pt"),
        legend.spacing.x  = grid::unit(6, "pt"),
        legend.box.margin = ggplot2::margin(t = 4, r = 0, b = 0, l = 0),
        panel.ontop = FALSE,
        panel.grid.minor.y = ggplot2::element_blank(),
        panel.grid.major.y = ggplot2::element_blank()
      )
    }

# bar chart variants: "single", "stacked", "grouped"
.year_bar_chart_core <- function(data,
                                 year_col,
                                 value_col,
                                 fill_col = NULL,
                                 variant = c("single", "stacked", "grouped"),
                                 x_label = "Jahr",
                                 y_label = "Anzahl",
                                 title = NULL,
                                 tooltip_col = NULL,
                                 data_id_col = NULL,
                                 palette = NULL,
                                 legend = TRUE,
                                 dodge_width = 0.6,
                                 bar_width = 0.5,
                                 stack_reverse = FALSE) {
  variant <- match.arg(variant)

  need <- c(year_col, value_col)
  if (!is.null(fill_col)) need <- c(need, fill_col)
  .ts_assert_cols(data, need)

  df <- data |>
    dplyr::mutate(
      year_factor = .ts_factor_year(.data[[year_col]]),
      fill_factor = if (is.null(fill_col)) NULL else {
        if (is.factor(.data[[fill_col]])) .data[[fill_col]]
        else factor(.data[[fill_col]])
      },
      tooltip_value = if (is.null(tooltip_col)) {
        scales::comma(.data[[value_col]])
      } else {
        as.character(.data[[tooltip_col]])
      },
      data_id_value = if (is.null(data_id_col)) {
        if (is.null(fill_col)) as.character(year_factor)
        else paste(year_factor, fill_factor, sep = "-")
      } else {
        as.character(.data[[data_id_col]])
      }
    )

  if (!is.null(fill_col)) {
    df <- tidyr::complete(
      df, year_factor, fill_factor,
      fill = stats::setNames(list(0), value_col)
    )
  } else {
    df <- tidyr::complete(
      df, year_factor,
      fill = stats::setNames(list(0), value_col)
    )
  }

  if (variant == "stacked") {
    df <- df %>%
      dplyr::group_by(year_factor) %>%
      dplyr::mutate(
        .ord = dplyr::min_rank(.data[[value_col]]) +
               1e-9 * as.integer(fill_factor)
      ) %>%
      dplyr::ungroup()
  }

  if (variant == "single") { #single bar
    fill_levels <- levels(df$year_factor)
    fill_map <- .ts_palette_named(fill_levels, palette)
    fill_aes <- ggplot2::aes(fill = year_factor)
    fill_scale <- ggplot2::scale_fill_manual(
      values = fill_map,
      guide = "none"
    )
    geom <- ggiraph::geom_col_interactive(width = 0.6)
  } else if (variant == "stacked") { # stacked bars
    fill_levels <- levels(df$fill_factor)
    fill_map <- .ts_palette_named(fill_levels, palette)
    fill_aes <- ggplot2::aes(fill = fill_factor, group = fill_factor)
    fill_scale <- ggplot2::scale_fill_manual(
      values = fill_map,
      guide = if (legend) ggplot2::guide_legend(title = NULL, nrow = 4, byrow = TRUE) else "none"
    )
    geom <- ggiraph::geom_col_interactive(
      mapping  = ggplot2::aes(order = if (stack_reverse) .ord else - .ord),
      width    = 0.6,
      position = ggplot2::position_stack(reverse = stack_reverse)
    )

  } else { # grouped bars
    fill_levels <- levels(df$fill_factor)
    fill_map <- .ts_palette_named(fill_levels, palette)
    fill_aes <- ggplot2::aes(fill = fill_factor)
    fill_scale <- ggplot2::scale_fill_manual(
      values = fill_map,
      guide = if (legend) ggplot2::guide_legend(title = NULL) else "none"
    )
    geom <- ggiraph::geom_col_interactive(
      position = ggplot2::position_dodge(width = dodge_width),
      width = bar_width
    )
  }

  p <- ggplot2::ggplot(
    df,
    ggplot2::aes(
      x = year_factor,
      y = .data[[value_col]],
      tooltip = tooltip_value,
      data_id = data_id_value
    )
  ) +
    geom +
    fill_aes +
    fill_scale +
    ggplot2::scale_y_continuous(
      labels = scales::comma,
      expand = ggplot2::expansion(mult = c(0, 0.08))
    ) +
    ggplot2::labs(x = x_label, y = y_label, title = title) +
    .ts_theme() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(margin = ggplot2::margin(t = 4)),
    )

  ggiraph::girafe(
    ggobj = p,
    options = list(
      ggiraph::opts_toolbar(saveaspng = TRUE),
      ggiraph::opts_sizing(rescale = TRUE)
    )
  )
}

# ---- year bar chart variant wrappers ----

year_bar_chart <- function(data,
                           year_col = "year",
                           value_col = "value",
                           x_label = "Jahr",
                           y_label = "Anzahl",
                           title = NULL,
                           tooltip_col = NULL,
                           data_id_col = NULL,
                           palette = NULL) {
  .year_bar_chart_core(
    data = data,
    year_col = year_col,
    value_col = value_col,
    fill_col = NULL,
    variant = "single",
    x_label = x_label,
    y_label = y_label,
    title = title,
    tooltip_col = tooltip_col,
    data_id_col = data_id_col,
    palette = palette
  )
}

year_bar_chart_stacked <- function(data,
                                   year_col = "year",
                                   value_col = "value",
                                   stack_col,
                                   x_label = "Jahr",
                                   y_label = "Anzahl",
                                   title = NULL,
                                   tooltip_col = NULL,
                                   data_id_col = NULL,
                                   palette = NULL,
                                   legend = TRUE,
                                   stack_reverse = FALSE) {
  .year_bar_chart_core(
    data = data,
    year_col = year_col,
    value_col = value_col,
    fill_col = stack_col,
    variant = "stacked",
    x_label = x_label,
    y_label = y_label,
    title = title,
    tooltip_col = tooltip_col,
    data_id_col = data_id_col,
    palette = palette,
    legend = legend,
    stack_reverse = stack_reverse
  )
}

year_bar_chart_grouped <- function(data,
                                   year_col = "year",
                                   value_col = "value",
                                   group_col,
                                   x_label = "Jahr",
                                   y_label = "Anzahl",
                                   title = NULL,
                                   tooltip_col = NULL,
                                   data_id_col = NULL,
                                   palette = NULL,
                                   legend = TRUE,
                                   dodge_width = 0.6,
                                   bar_width = 0.5) {
  .year_bar_chart_core(
    data = data,
    year_col = year_col,
    value_col = value_col,
    fill_col = group_col,
    variant = "grouped",
    x_label = x_label,
    y_label = y_label,
    title = title,
    tooltip_col = tooltip_col,
    data_id_col = data_id_col,
    palette = palette,
    legend = legend,
    dodge_width = dodge_width,
    bar_width = bar_width
  )
}
