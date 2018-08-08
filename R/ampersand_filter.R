ampersand_filter <- function() {
  std_input <- file("stdin")
  on.exit(close.connection(std_input))

  ast <- readLines(std_input, warn = FALSE)

  intext_regex <- "\"citationMode\":\\{\"t\":\"AuthorInText\".*?\\}\\]\\]\\}"

  intext_citations <- unique(unlist(regmatches(ast, gregexpr(intext_regex, ast, useBytes = TRUE))))
  corrected_citations <- gsub("&", "and", intext_citations, useBytes = TRUE)

  for(i in seq_along(corrected_citations)) {
    ast <- gsub(intext_citations[i], corrected_citations[i], ast, fixed = TRUE, useBytes = TRUE)
  }

  write(ast, stdout())
}
