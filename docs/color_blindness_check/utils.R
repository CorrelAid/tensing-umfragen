get_colorblindness_check_url <- function(pal) {
    # Coloring for Colorblindness works with query parameters, so we can construct the links 
    # https://davidmathlogic.com/colorblind/#%color1hex-%color2hex-...
    # e.g. https://davidmathlogic.com/colorblind/#%23B5C948-%231F2A4F-%236A7A4C
    col_query_params <- paste(stringr::str_remove_all(toupper(pal), "#"), collapse = "-%23")
    paste0("https://davidmathlogic.com/colorblind/#%23", col_query_params)
}

take_screenshots <- function(cbc_dir, pal_url, pal_name) {
    session <- ChromoteSession$new()
    session$go_to(pal_url)
    session$screenshot(here::here(cbc_dir, "screenshots", paste0(pal_name, "_main.png")), selector = c("h2.text-center", "div.flex-row:nth-child(6)"), show = FALSE)
    session$screenshot(here::here(cbc_dir, "screenshots", paste0(pal_name, "_add.png")), selector = "div.flex:nth-child(11)", show = FALSE)
    session$close()
}

embed_screenshots <- function(cbc_dir, pal_name) {

    cat(sprintf("![](%s)\n", 
            c(here::here(cbc_dir, "screenshots", paste0(pal_name, "_main.png")), 
                here::here(cbc_dir, "screenshots", paste0(pal_name, "_add.png")))), sep = "\n")
}