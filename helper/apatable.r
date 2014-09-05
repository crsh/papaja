apa.table <- function(x, caption = NULL, label = NULL, placement = "tbp", ...) {
  require("knitr")
  if(!is.null(label)) label <- paste0("\\label{", label, "}")
  cat("\\begin{table}[", placement, "]\n  \\centering\n  \\captionbox{", caption, label, "}{", sep = "")
  kable(x, format = "latex", booktabs = TRUE, ...)
  cat("\n  } \n\\end{table}")
}
