files <- sort(list.files("R", pattern = "\\.R$", full.names = TRUE))

for (f in files) {
  cat("\n====", basename(f), "====\n")
  txt <- readLines(f, warn = FALSE)
  
  # match lines where you define functions
  defs <- grep("^[a-zA-Z0-9_.]+\\s*<-\\s*function", txt, value = TRUE)
  
  # extract just the function names
  names <- sub("\\s*<-.*", "", defs)
  
  print(trimws(names))
}
