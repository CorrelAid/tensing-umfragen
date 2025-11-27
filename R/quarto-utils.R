# TODO: not best practice to have this here.. should be passed to functions
# tn_cfg <- readr::read_rds(here::here("config/tn_cfg.rds"))
# og_cfg <- readr::read_rds(here::here("config/tn_cfg.rds"))

#' Format a Question Label for Markdown Output
#' @param Q element from the config object (prefix Q_)  -> a row from the survey metadata
#' @param inline Logical; if \code{TRUE}, returns a markdown-formatted inline object (\code{I()}), otherwise plain text.
#' @return A character string (or inline object) with the question label formatted in italics and quotation marks for markdown rendering.
#' @details Wraps the label of the question in markdown italics and quotation marks for styled text display.
#' @seealso \code{\link[base]{sprintf}}, \code{\link[base]{I}}
#' @export
fmt_q <- function(Q, inline = TRUE) {
  s <- sprintf('"*%s*"', Q$label)
  if (inline) {
    return(I(s))
  }
  return(s)
}

#' Format a Source Reference to a Questionnaire Item
#' @param Q element from the config object (prefix Q_)  -> a row from the survey metadata
#' @param type A character string specifying the questionnaire type: \code{"tn"} for Teilnehmer*innen or \code{"og"} for Ortsgruppen.
#' @param inline Logical; if \code{TRUE}, returns a markdown-formatted inline object (\code{I()}), otherwise plain text.
#' @return A character string (or inline object) referencing a question in the specified questionnaire with a markdown link.
#' @details Combines a formatted question label (via \code{fmt_q()}) with a link to the corresponding questionnaire URL defined in \code{tn_cfg} or \code{og_cfg}.
#' @seealso \code{fmt_q}, \code{\link[base]{sprintf}}, \code{\link[base]{I}}
#' @export
fmt_source <- function(Q, type, url, inline = TRUE) {
  if (type == "tn") {
    s <- sprintf(
      "%s im [%s Fragebogen](%s)",
      fmt_q(Q),
      "Teilnehmer*innen",
      url
    )
  } else if (type == "og") {
    s <- sprintf(
      "%s im [%s Fragebogen](%s)",
      fmt_q(Q),
      "Ortsgruppen",
      url
    )
  } else {
    stop(paste("Invalid value for argument type. allowed:", "tn, og"))
  }

  if (inline) {
    return(I(s))
  }
  return(s)
}

#' Format Markdown Link to a survey
#' @param type A character string specifying the questionnaire type: \code{"tn"} for Teilnehmer*innen or \code{"og"} for Ortsgruppen.
#' @param inline Logical; if \code{TRUE}, returns a markdown-formatted link as an inline object (\code{I()}), otherwise as plain text.
#' @return A character string (or inline object) containing a formatted markdown link to the specified questionnaire.
#' @details Generates a markdown link to either the participant or local group questionnaire URL based on configuration objects \code{tn_cfg} and \code{og_cfg}.
#' @seealso \code{\link[base]{sprintf}}, \code{\link[base]{I}}
#' @export
fmt_fragebogen <- function(type, inline = TRUE, url) {
  # TODO add link to Fragebogen
  if (type == "tn") {
    s <- sprintf("[%s Fragebogen](%s)", "Teilnehmer*innen", url)
  } else if (type == "og") {
    s <- sprintf("[%s Fragebogen](%s)", "Ortsgruppen", url)
  } else {
    stop(paste("Invalid value for argument type. allowed:", "tn, og"))
  }

  if (inline) {
    return(I(s))
  }
  return(s)
}

get_pie_chart_data <- function(data, col) {
  data %>%
    count({{ col }}) %>%
    mutate(
      csum = rev(cumsum(rev(n))),
      perc = round(n * 100 / nrow(data), 1),
      pos = n / 2 + lead(csum, 1),
      pos = if_else(is.na(pos), n / 2, pos)
    )
}

#' Format Character Vector as Markdown Bullet Point List
#' @param char_vec A character vector to format.
#' @return A single character string with each element of the vector as a markdown bullet point.
#' @details Removes \code{NA} values and concatenates elements into a newline-separated bullet list using markdown syntax.
#' @seealso \code{\link[base]{paste}}
#' @export
fmt_vec_to_bullet_point <- function(char_vec) {
  char_vec <- char_vec[!is.na(char_vec)]
  paste("- ", char_vec, collapse = "\n")
}

# Creates a Year Comparison Tabset in Quarto
# Takes current and previous year data along with a rendering function and titles as input.
render_year_tabset <- function(curr_data, prev_data, render_fn) {
  stopifnot(is.function(render_fn))
  cat("::: {.panel-tabset}\n\n")
  cat("## ", curr_data$year, "\n\n", sep = "")
  render_fn(curr_data)
  cat("\n\n")
  cat("## ", prev_data$year, "\n\n", sep = "")
  render_fn(prev_data)
  cat("\n\n:::\n")
  invisible(NULL)
}

# Ensure htmlwidget dependencies get registered even within results='asis' chunks
render_widget_output <- function(widget) {
  deps <- htmltools::findDependencies(widget)
  if (length(deps) > 0 &&
      isTRUE(getOption("knitr.in.progress")) &&
      requireNamespace("knitr", quietly = TRUE)) {
    knitr::knit_meta_add(deps)
  }
  print(htmltools::tagList(widget))
}

# Add download buttons to all static plots rendered via knitr's plot hook
.ts_setup_plot_download_hook <- local({
  initialized <- FALSE
  function() {
    if (initialized || !isTRUE(getOption("knitr.in.progress"))) {
      return(invisible(NULL))
    }

    default_plot_hook <- knitr::knit_hooks$get("plot")

    knitr::knit_hooks$set(plot = function(x, options) {
      # keep default behavior for dump.qmd or if there is no plot output
      if (identical(basename(knitr::current_input()), "dump.qmd") ||
          length(x) == 0) {
        return(default_plot_hook(x, options))
      }

      default_html <- default_plot_hook(x, options)

      if (is.null(default_html)) {
        return(default_html)
      }

      href <- x[1]
      download_label <- if (!is.null(options$fig_download_label)) {
        options$fig_download_label
      } else {
        "PNG speichern"
      }

      htmltools::doRenderTags(
        htmltools::div(
          class = "ts-plot-download-wrapper",
          htmltools::HTML(default_html),
          htmltools::a(
            class = "ts-plot-download-btn",
            href = href,
            download = basename(href),
            title = download_label,
            `aria-label` = download_label,
            htmltools::tags$span("⬇︎")
          )
        )
      )
    })

    initialized <<- TRUE
    invisible(NULL)
  }
})

callout_datenquellen <- function(cfg, og_q = NULL, tn_q = NULL) {
  # helper: resolve character names to real question objects
  resolve <- function(x, cfg_part) {
    if (is.character(x)) {
      cfg_part[[x]]
    } else {
      x
    }
  }

  tn_block <- ""
  og_block <- ""

  if (!is.null(tn_q) && length(tn_q) > 0) {
    tn_resolved <- lapply(tn_q, resolve, cfg_part = cfg$tn_cfg)
    tn_links <- purrr::map_chr(tn_resolved, ~ fmt_q(.x))
    tn_src <- fmt_fragebogen("tn", url = cfg$tn_cfg$URL)

    tn_block <- paste0(
      "**Relevante Fragen (TN)** im ", tn_src, ":\n\n",
      "- ", paste(tn_links, collapse = "\n- "), "\n\n"
    )
  }

  if (!is.null(og_q) && length(og_q) > 0) {
    og_resolved <- lapply(og_q, resolve, cfg_part = cfg$og_cfg)
    og_links <- purrr::map_chr(og_resolved, ~ fmt_q(.x))
    og_src <- fmt_fragebogen("og", url = cfg$og_cfg$URL)

    og_block <- paste0(
      "**Relevante Fragen (OG)** im ", og_src, ":\n\n",
      "- ", paste(og_links, collapse = "\n- "), "\n\n"
    )
  }

  paste0(
    "\n\n",
    "::: {.callout-note collapse=\"true\"}\n",
    "## Datenquelle\n\n",
    tn_block,
    og_block,
    ":::",
    "\n\n"
  )
}
