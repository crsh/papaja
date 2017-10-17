ampersand_filter <- function() {
  std_input <- file("stdin")

  ast <- readLines(std_input, warn = FALSE)

  # write(ast, "~/ast_test.txt")

  intext_regex <- "\"citationMode\":\\{\"t\":\"AuthorInText\".*?\\}\\]\\]\\}"

  intext_citations <- unlist(stringr::str_extract_all(ast, intext_regex))
  corrected_citations <- stringr::str_replace(intext_citations, "&", "and")

  intext_locations <- stringr::str_locate_all(ast, intext_regex)[[1]]

  for(i in rev(seq_along(corrected_citations))) {
    stringr::str_sub(ast, intext_locations[i, "start"], intext_locations[i, "end"]) <- corrected_citations[i]
  }

  # write(ast, "~/ast_test2.txt")

  write(ast, stdout())
  closeAllConnections()

  # replace_ampersand <- function(x) {
  #   if(
  #     is.list(x) &&
  #     !is.null(x$t) &&
  #     x$t == "Cite" &&
  #     x$c[[1]][[1]]$citationMode$t == "AuthorInText"
  #   ) {
  #     list_structure <- as.relistable(x$c[[2]])
  #     corrected_citation <- gsub("&", "and", unlist(x$c[[2]]))
  #     x$c[[2]] <- relist(corrected_citation, skeleton = list_structure)
  #   }
  #
  #   x
  # }
  #
  # ast <- jsonlite::fromJSON(std_input, simplifyVector = FALSE)
  #
  # # write(jsonlite::toJSON(ast, auto_unbox = TRUE), "~/json_ast_test.txt")
  # ast$blocks <- lapply(
  #   ast$blocks
  #   , function(x) {
  #     x$c <- lapply(x$c, replace_ampersand)
  #     x
  #   }
  # )
  # # write(jsonlite::toJSON(ast, auto_unbox = TRUE), "~/json_ast_test2.txt")
  #
  # write(jsonlite::toJSON(ast, auto_unbox = TRUE), stdout())
  # closeAllConnections()
}
