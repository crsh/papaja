apa_print.metafor <- function(
  x
  , est_name = NULL
  , ci = 0.95
  , in_paren = FALSE
  , ...
){

  #papaja:::validate(x, check_class = "rma.uni")

  est_name <- "b"

  apa_res <- papaja:::apa_print_container()

  apa_res$estimate <- paste0("$b = ", x$b[1], "95\\% CI = $[", x$ci.lb, "$, $", x$ci.ub, "]$")

  apa_res$statistic <- paste0("$z = ", x$zval, ", p = ", x$pval)

  apa_res$full_result <- paste(apa_res$estimate, apa_res$statistic)

  apa_res
}





