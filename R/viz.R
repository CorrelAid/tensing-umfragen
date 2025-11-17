library(ggplot2)
library(dplyr)
library(ggtext)

#' some functions for plots that need to be created multiple times

aussage_bar_chart <- function(data_long, var_filter, var_value) {
  plot_data <- data_long %>%
    dplyr::filter(.data[[var_filter]] == var_value) |>
    dplyr::group_by(aussage, q_label, value, ch_label) %>%
    dplyr::summarize(n = n(), percent = n / length(unique(data_long$tn_id)))

  title <- unique(plot_data$q_label)

  p <- ggplot(plot_data, aes(x = value, fill = value, y = percent)) +
    geom_col() +
    scale_fill_manual(
      name   = "",
      values = setNames(COLS_6, 1:6),
      breaks = c(1, 6),
      labels = c(
        "1 = trifft überhaupt nicht zu",
        "6 = trifft voll und ganz zu"
      )
    ) +
    #scale_x_discrete(labels = meta_aussagen_choices$label)+
    scale_y_continuous(labels = scales::label_percent(), limits = c(0, 1)) +
    labs(
      y = "% Aktive",
      title = str_wrap(title, 80),
      # fill = NULL,
      x = "Bewertung",
      # subtitle = "Bewertung von trifft überhaupt nicht zu (1) zu trifft voll und ganz zu (6)"
    ) +
    # guides(fill = "none") +
    theme(
      plot.title = element_textbox_simple(margin = unit(c(0, 0, 20, 0), "pt")),
      legend.position = "bottom"
    )

  ggiraph::girafe(ggobj = p)
}


bedarfe_bar_chart <- function(data_long, og_choices, og_cfg, bedarf) {
  order_df <- og_choices %>%
    filter(
      list_name %in% og_cfg$QS_UNTERSTUETZUNGSBEDARFE$select_from_list_name
    ) %>%
    mutate(order = c(1:2, 4:5, 3)) %>%
    select(order, bedarf_value = label) %>%
    arrange(order)

  plot_data <- data_long %>%
    left_join(order_df, by = "bedarf_value") %>%
    dplyr::filter(bedarf == .env$bedarf) |>
    dplyr::group_by(bedarf, bedarf_value, order, .drop = FALSE) %>%
    dplyr::summarize(n = n(), percent = n / length(unique(data_long$og_id)))

  title <- unique(plot_data$bedarf)

  colors <- COLS_6[1:nrow(order_df)]
  names(colors) <- order_df$bedarf_value

  p <- ggplot(
    plot_data,
    aes(x = fct_reorder(bedarf_value, order), fill = bedarf_value, y = percent)
  ) +
    geom_col() +
    #scale_x_discrete(labels = meta_aussagen_choices$label)+
    scale_y_continuous(labels = scales::label_percent(), limits = c(0, 1)) +
    labs(
      y = "% Aktive",
      title = str_wrap(title, 80),
      fill = NULL,
      x = "Bewertung",
      subtitle = "Bewertung nein bis ja"
    ) +
    guides(fill = "none") +
    theme(
      plot.subtitle = element_textbox_simple(),
      plot.title = element_textbox_simple(margin = unit(c(0, 0, 10, 0), "pt"))
    )

  ggiraph::girafe(ggobj = p)
}

# rev_x so far always true
eig_bar_chart <- function(plot_data, rev_x = TRUE) {
  title <- unique(plot_data$eig_label)

  # we want to reverse the y position so that the best option is on top
  # we do want to have the most green
  colors <- rev(COLS_6) |> set_names(levels(plot_data$choice_label))

  lvls <- levels(plot_data$choice_label)
  end_levels <- lvls[c(1, length(lvls))]

  p <- ggplot(
    plot_data,
    aes(y = percent, fill = choice_label, x = choice_label)
  ) +
    geom_col() +
    scale_fill_manual(
      name   = "",
      values = colors,
      breaks = end_levels,
      labels = end_levels,
      drop = FALSE
    ) +
    scale_x_discrete(
      drop = FALSE,
      labels = function(x) sub(".*\\((\\d+)\\)$", "\\1", x)
    ) +
    scale_y_continuous(labels = scales::label_percent(), limits = c(0, 1)) +
    labs(
      x = "% Aktive",
      title = title,
      fill = NULL,
      y = NULL
    ) +
    # guides(fill = "none") +
    theme(
      plot.subtitle = element_textbox_simple(),
      plot.title = element_textbox_simple(margin = unit(c(0, 0, 10, 0), "pt")),
      # axis.text.x = element_text(size = 10.5)
      legend.position = "bottom"
    )
  ggiraph::girafe(ggobj = p)
}


# Trends.qmd page functions (for now)
# ---- helpers used by all year_* charts ----
.ts_assert_cols <- function(data, cols) {
  miss <- setdiff(cols, names(data))
  if (length(miss)) stop("Missing columns: ", paste(miss, collapse = ", "))
}

.ts_factor_year <- function(x) factor(x, levels = sort(unique(x)))

.ts_palette_named <- function(levels, palette = NULL) {
  base <- if (is.null(palette)) COLS_6 else unname(palette)   # drop names
  L <- length(base)
  idx <- ((seq_along(levels) - 1) %% L) + 1                   # 1..L cycling
  vals <- rev(base)[idx]                                      # reverse palette
  names(vals) <- levels                                       # keep level names
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

  # keep empty groups visible
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
