#' colors and palettes
library(ggplot2)
library(ggtext)

TS_FONT_FAMILY <- "Atkinson Hyperlegible"

TS_GREEN <- "#b5c948"
KEINE_ANGABE_GRAY <- "#676565"

theme_ts <- ggplot2::theme_minimal(
  base_size = 14,
  base_family = TS_FONT_FAMILY
) +
  ggplot2::theme(
    legend.position = "top",
    plot.title = element_text(
      face = "bold",
      size = 16,
      colour = "black",
      hjust = 0.5,
      family = TS_FONT_FAMILY
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


# New accessible palettes, incorporating TenSing Green

pal_yes_no <- c(
  "#b5c948",
  "#1F2A4F"
)

pal_yes_no_3 <- c(
  "#B5C948",
  "#6A7A4C",
  "#1F2A4F"
)

pal_yes_no_5 <- c(
  "#B5C948",
  "#88994A",
  "#6A7A4C",
  "#4C5A4D",
  "#1F2A4F"
)

pal_yes_no_6 <- c(
  "#B5C948",
  "#97A949",
  "#79894B",
  "#5B6A4C",
  "#3D4A4E",
  "#1F2A4F"
)

pal_8 <- c(
  "#b5c948",
  "#7EA85A",
  "#4F8D63",
  "#3A6F78",
  "#3E5F97",
  "#2F4B70",
  "#6C4C7A",
  "#B18FBF"
)

pal_trends_10 <- c(
  "#b5c948",
  "#7F9C3A",
  "#4F7A45",
  "#3A6459",
  "#345A72",
  "#2F4E78",
  "#3D3E63",
  "#5A496F",
  "#7B5C7D",
  "#A38D3A"
)

pal_likert_11 <- c(
  "#B5C948",
  "#A6B949",
  "#97A949",
  "#88994A",
  "#79894B",
  "#6A7A4C",
  "#5B6A4C",
  "#4C5A4D",
  "#3D4A4E",
  "#2E3A4E",
  "#1F2A4F"
)

# Mapping for info columns

INFO_COLS <- c(
  "ja"                      = pal_yes_no_5[1],
  "Ja"                      = pal_yes_no_5[1],
  "zum Teil"                = pal_yes_no_5[2],
  "nein"                    = pal_yes_no_5[5],
  "Nein"                    = pal_yes_no_5[5],
  "keine Angabe"            = KEINE_ANGABE_GRAY,
  "Genau wie bisher auch"   = pal_yes_no_5[1],
  "Auf anderem Weg, und zwar:" = pal_yes_no_5[5]
)

