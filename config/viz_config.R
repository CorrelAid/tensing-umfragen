library(ggplot2)

TS_GREEN <- "#b5c948"
RED <- "#D81B60"
cols_4 <- c(TS_GREEN, "#3E3D3C", "#1E88E5", RED)
PAL_AMPEL <- c(TS_GREEN, "#FFFF00", RED)

#hcl.pals(type = "diverging")
#scales::show_col(hcl.colors(6, "Purple-Green"))
cols_6 <- hcl.colors(6, "Purple-Green")

theme_ts <- ggplot2::theme_minimal(
  base_size = 14,
  base_family = "Atkinson Hyperlegible"
) +
  ggplot2::theme(
    legend.position = "top",
    plot.title = element_text(face = "bold", size = 16, colour = "black"),
    plot.title.position = "plot"
  )

