apa_print.summary.lm <- function(x, op = "(", cp = ")", ci = ci, standardized = standardized) {
  validate(ci, check_class = "matrix")
  sapply(ci, validate, check_class = "numeric")

  coefs <- x$coefficients
  if(is.matrix(ci)) {
    coefs <- cbind(coefs, ci = ci)
  } else stop("Please supply estimates of confidence intervals.")

  p_pos <- grep("Pr|p-value", colnames(coefs))
  if(standardized) stat_name <- "\\beta" else stat_name <- "b"

  apa_print <- apply(coefs, 1, function(y) {
    p <- printp(y[p_pos])
    if(!grepl("<|>", p)) eq <- "= " else eq <- ""
    paste0("$", stat_name, " = ", printnum(y["Estimate"], gt1 = !standardized)
           , paste0("$ $[", paste(printnum(y[tail(names(y), 2)], gt1 = !standardized), collapse = ", "), "]")
           , "$, $t", op, x$fstatistic["dendf"], cp, " = ",  printnum(y["t value"]), "$, $p ", eq, p, "$"
    )
  })
  p <- printp(pf(x$fstatistic[1], x$fstatistic[2], x$fstatistic[3], lower.tail = FALSE))
  if(!grepl("<|>", p)) eq <- "= " else eq <- ""
  f <- paste0("$F", op, x$fstatistic["numdf"], ",", x$fstatistic["dendf"], cp, " = ", printnum(x$fstatistic["value"])
              , "$, $p ", eq, p, "$"
  )
  r2 <- paste0("$R^2 = ", printnum(x$r.squared, gt1 = FALSE), "$")
  apa_print <- c(apa_print, `F-test` = f, `R2` = r2)
  apa_print
}
