apa.table <- function(x, caption = NULL, placement = "tbp", ...) {
  require("knitr")
  cat("\\begin{table}[", placement, "]\n  \\centering\n  \\captionbox{", caption, "}{", sep = "")
  kable(x, format = "latex", booktabs = TRUE, ...)
  cat("\n  } \n\\end{table}")
}
