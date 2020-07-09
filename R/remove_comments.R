#' Remove comments from R Markdown
#'
#' Removes markdown comments from an R Markdown file
#'
#' @param x Character. Path to R Markdown file
#' @param file Character. Name of the new R Markdown file without comments.
#'
#' @export

remove_comments <- function(x, file) {
  x <- readLines(x, encoding = "UTF-8")
  x <- x[!grepl("^<!--((?!-->).)+-->$", x, useBytes = TRUE, perl = TRUE)]
  x <- x[!grepl("^<@~\\{#.+\\}$", x, useBytes = TRUE)]
  x <- x[!grepl("^~@>$", x, useBytes = TRUE)]

  x <- gsub("<!--((?!-->).+)-->", "", x, useBytes = TRUE, perl = TRUE)

  writeLines(x, file, useBytes = TRUE)
}
