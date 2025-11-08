r_files <- list.files("R", full.names = TRUE, pattern = "\\.R$", recursive = TRUE)
invisible(lapply(sort(r_files), source))
