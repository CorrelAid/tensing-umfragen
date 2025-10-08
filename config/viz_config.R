#' colors and palettes
#' very messy, needs to be cleaned up
library(ggplot2)
library(ggtext)

TS_GREEN <- "#b5c948"
RED <- "#D81B60"
cols_4 <- c(TS_GREEN, "#3E3D3C", "#1E88E5", RED)
PAL_AMPEL <- c(TS_GREEN, "#FFFF00", RED)

#hcl.pals(type = "diverging")
#scales::show_col(hcl.colors(6, "Purple-Green"))
cols_6 <- hcl.colors(6, "Purple-Green")
COLS_6_orig <- c(
  '#8c510a',
  '#d8b365',
  '#f6e8c3',
  '#c7eae5',
  '#5ab4ac',
  '#01665e'
)
COLS_6 <- c('#7a4a0c', '#cfb77c', '#f4e9d2', '#c0e3dc', '#4ca89f', '#155c52')

PAL5_DIV_orig <- c('#a6611a', '#dfc27d', '#f5f5f5', '#80cdc1', '#018571')
PAL5_DIV <- c('#874d16', '#d6bb88', '#f0f0f0', '#75c4b9', '#116b5c')
PAL5_UEB <- c(
  `Nein` = '#a6611a',
  `Eher nein` = '#dfc27d',
  `Bin mir unsicher` = '#f5f5f5',
  `Eher ja` = '#80cdc1',
  `Ja` = '#018571'
)
theme_ts <- ggplot2::theme_minimal(
  base_size = 14,
  base_family = "Atkinson Hyperlegible"
) +
  ggplot2::theme(
    legend.position = "top",
    plot.title = element_text(
      face = "bold",
      size = 16,
      colour = "black"
    ),
    plot.title.position = "plot"
  )


PLACEHOLDER_PLOT <-
  ggplot(tibble::tibble(x = 1:3, y = 4:6), aes(x = x, y = y)) +
  geom_blank() +
  annotate(
    "text",
    x = 2,
    y = 5,
    label = "PLATZHALTER PLOT",
    size = 16
  ) +
  labs(title = "Platzhalter")
