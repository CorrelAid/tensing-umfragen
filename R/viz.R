#' some functions for plots that need to be created multiple times

aussage_bar_chart <- function(data_long, var_filter, var_value) {
  plot_data <- data_long %>%
    dplyr::filter(.data[[var_filter]] == var_value) |>
    dplyr::group_by(aussage, q_label, value, ch_label) %>%
    dplyr::summarize(n = n(), percent = n / length(unique(data_long$tn_id)))

  title <- unique(plot_data$q_label)

  p <- ggplot(plot_data, aes(x = value, fill = value, y = percent)) +
    geom_col() +
    scale_fill_manual(values = COLS_6) +
    #scale_x_discrete(labels = meta_aussagen_choices$label)+
    scale_y_continuous(labels = scales::label_percent(), limits = c(0, 1)) +
    labs(
      y = "% Aktive",
      title = str_wrap(title, 80),
      fill = NULL,
      x = "Bewertung",
      subtitle = "Bewertung von trifft überhaupt nicht zu (1) zu trifft voll und ganz zu (6)"
    ) +
    guides(fill = "none") +
    theme(
      plot.title = element_textbox_simple(margin = unit(c(0, 0, 20, 0), "pt"))
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

  p <- ggplot(
    plot_data,
    aes(y = fct_rev(choice_label), fill = choice_label, x = percent)
  ) +
    geom_col() +
    scale_fill_manual(values = colors) +
    scale_y_discrete(drop = FALSE) +
    scale_x_continuous(labels = scales::label_percent(), limits = c(0, 1)) +
    labs(
      x = "% Aktive",
      title = title,
      fill = NULL,
      y = NULL
    ) +
    guides(fill = "none") +
    theme(
      plot.subtitle = element_textbox_simple(),
      plot.title = element_textbox_simple(margin = unit(c(0, 0, 10, 0), "pt"))
    )
  ggiraph::girafe(ggobj = p)
}
