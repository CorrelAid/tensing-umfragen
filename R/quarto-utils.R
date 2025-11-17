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