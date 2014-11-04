apa.table <- function(x, caption = NULL, note = NULL, placement = "tbp", landscape = FALSE, ...) {
  require("knitr")
  if(landscape) table_env <- "sidewaystable" else table_env <- "table"
  cat("\\begin{", table_env, "}[", placement, "]\n\\centering\n\\begin{threeparttable}\n\\caption{", caption, "}", sep = "")

  if(is.list(x) && !is.data.frame(x)) {
    lapply(seq_along(x), function(i) {
        cat("\n\\begin{tabular}{@{}c@{}}\n")
        if(i == 1) cat("\\toprule\n")
        cat(names(x)[i], "\\\\\n")
        kable(x[[i]], format = "latex", booktabs = TRUE, ...)
        cat("\\\\\n\\end{tabular}")
      }
    )
  } else kable(x, format = "latex", booktabs = TRUE, ...)

  cat("\n")
  if(!is.null(note)) cat("\\tablenotes{\\textit{Note.}", note, "}\n")
  cat("\\end{threeparttable}\n\\end{", table_env, "}", sep = "")
}
