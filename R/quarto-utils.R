tn_cfg <- readr::read_rds("config/tn_cfg.rds")
og_cfg <- readr::read_rds("config/tn_cfg.rds")

fmt_source <- function(Q, type, inline = TRUE) {
    if (type == "tn") {
        s <- sprintf('"%s" im [%s Fragebogen](%s)', Q$label, "Teilnehmer*innen", tn_cfg$URL)

    } else if (type == "og") {
        s <- sprintf('"%s" im [%s Fragebogen](%s)', Q$label, "Ortsgruppen", og_cfg$URL)
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
