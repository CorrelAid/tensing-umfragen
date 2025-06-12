tn_cfg <- readr::read_rds(here::here("config/tn_cfg.rds"))
og_cfg <- readr::read_rds(here::here("config/tn_cfg.rds"))

fmt_q <- function(Q, inline = TRUE) {
  s <- sprintf('"*%s*"', Q$label)
  if (inline) {
    return(I(s))
  }
  return(s)
}

fmt_source <- function(Q, type, inline = TRUE) {
    if (type == "tn") {
        s <- sprintf('%s im [%s Fragebogen](%s)', fmt_q(Q), "Teilnehmer*innen", tn_cfg$URL)

    } else if (type == "og") {
        s <- sprintf('%s im [%s Fragebogen](%s)', fmt_q(Q), "Ortsgruppen", og_cfg$URL)
    } else {
      stop(paste("Invalid value for argument type. allowed:", "tn, og"))
    }
  
  if (inline) {
      return(I(s))
  }
  return(s)
}

fmt_fragebogen <- function(type, inline = TRUE) {
  # TODO add link to Fragebogen
  if (type == "tn") {
    s <- sprintf('[%s Fragebogen](%s)', "Teilnehmer*innen", tn_cfg$URL)
    
  } else if (type == "og") {
    s <- sprintf('[%s Fragebogen](%s)', "Ortsgruppen", og_cfg$URL)
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
    mutate(csum = rev(cumsum(rev(n))),
           perc = round(n * 100 / nrow(data), 1),
           pos = n / 2 + lead(csum, 1),
           pos = if_else(is.na(pos), n / 2, pos)) 
  
}

fmt_vec_to_bullet_point <- function(char_vec) {
  char_vec <- char_vec[!is.na(char_vec)]
  paste("- ", char_vec, collapse = "\n")
}
