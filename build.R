# build.R
rm(list = ls())
options(readr.show_col_types = FALSE)
source("R/load_libs.R")

# --- cleanup ---------------------------------------------------------------
clean_dir("preview")
clean_dir("_site")

# --- pipeline ---------------------------------------------------------------
source("pipeline/run_pipeline.R")

# --- render ---------------------------------------------------------------
message("=== Rendering websites ===")
years <- get_all_years()
for (y in years) {
    message("=== Rendering edition for ", y, " ===")
    Sys.setenv(RENDER_YEAR = as.character(y))
    out_dir <- file.path("_site", as.character(y))

    if (dir.exists(out_dir)) {
        unlink(out_dir, recursive = TRUE, force = TRUE)
    }
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

    cmd <- sprintf("quarto render . --profile build --output-dir %s", shQuote(out_dir))
    status <- system(cmd)
    if (status != 0) stop("Rendering failed for year ", y)

    nested_dirs <- c(file.path(out_dir, "_site"), file.path(out_dir, "preview"))
    for (d in nested_dirs) {
        if (dir.exists(d)) {
            unlink(d, recursive = TRUE, force = TRUE)
        }
    }
}

message("=== All years rendered successfully. ===")
Sys.unsetenv("RENDER_YEAR")

# --- root redirect via HTML template ---------------------------------------
latest <- max(years)

template_file <- "redirect_template.html"
out_file <- file.path("_site", "index.html")

template <- readLines(template_file)
redirect_html <- gsub("__TARGET__", latest, template, fixed = TRUE)

writeLines(redirect_html, out_file)
