#' Format statistics (APA 6th edition)
#'
#' Formats output objects from various statistical methods to create formated chraracter strings to report
#' the results in accordance with APA manuscript guidelines.
#'
#' @param x Output object. See details.
#' @param stat_name Character. Name of the reported statistic; if \code{NULL} the name specified in the
#'    supplied object is used.
#' @param n Numeric. Size of the sample. Needed when reporting $\chi^2$, otherwise this parameter is
#'    ignored.
#' @param standardized Logical. If \code{x} is an object of class \code{summary.lm} indicates if
#'    coefficients are standardized or unstandardized.
#' @param ci Numeric. Matrix containing lower and upper boundaries of confidence intervals for estimated
#'    coefficients in columns. Ignored if \code{x} is not of class \code{summary.lm}.
#' @param in_paren Logical. Indicates if the formated string be reported inside parentheses. See details.
#' @details Currently the following output objects are supported:
#'
#'    \itemize{
#'      \item \code{htest}
#'      \item \code{summary.lm}
#'      \item \code{anova}
#'    }
#'
#'    If \code{in_paren} is \code{TRUE} parentheses in the formated string, such as those surrounding degrees
#'    of freedom, are replaced with brackets.
#' @examples
#' t_stat <- t.test(iris$Sepal.Length, iris$Sepal.Width)
#' apa.stat(t_stat)
#'
#' cor_stat <- cor.test(iris$Sepal.Length, iris$Sepal.Width)
#' apa.stat(cor_stat)
#'
#' iris_lm <- lm(Petal.Length ~ Sepal.Length + Sepal.Width, data = iris)
#' apa.stat(summary(iris_lm), ci = confint(iris_lm))
#'
#' iris_lm2 <- update(iris_lm, formula = . ~ + Sepal.Length:Sepal.Width)
#' apa.stat(anova(iris_lm2, iris_lm))
#' @export

apa.stat <- function(
  x
  , stat_name = NULL
  , n = NULL
  , standardized = FALSE
  , ci = NULL
  , in_paren = FALSE
) {
  # Add alternative method if(is.list(x)) using list names as parameters and values as statistics

  if(in_paren) {
    op <- "["; cp <- "]"
  } else {
    op <- "("; cp <- ")"
  }

  if("htest" %in% class(x)) {
    apa.stat <- apa_htest(x, op, cp, stat_name = stat_name, n = n)
  } else if("summary.lm" %in% class(x)) {
    apa.stat <- apa_lmsummary(x, op, cp, ci = ci, standardized = standardized)
  } else if("anova" %in% class(x)) {
    if(any(apply(x, 1, is.na))) { # Dirty hack
      apa.stat <- apply(x[-1,], 1, make_f_test, op, cp)
    }
  } else {
    stop("No method defined for object class", class(x), ".")
  }

  apa.stat
}



apa_htest <- function(x, op = "(", cp = ")", stat_name = stat_name, n = n) {
  if(is.null(stat_name)) stat_name <- names(x$statistic)
  stat <- printnum(x$statistic)

  if(!is.null(x$sample.size)) n <- x$sample.size

  if(!is.null(x$parameter)) {

    # Assemble statistic and degrees of freedom
    if(tolower(names(x$parameter)) == "df") {
      if(x$parameter %%1 == 0) printdigits <- 0 else printdigits = 2
      if(grepl("X|chi", stat_name, ignore.case = TRUE)) {
        if(is.null(x$sample.size) & is.null(n)) stop("Please provide the sample size to report.") # Demand sample size information if it's a Chi^2 test
        stat_name <- paste0(stat_name, op, printnum(x$parameter[grep("df", names(x$parameter), ignore.case = TRUE)], digits = printdigits), ", n = ", n, cp)
      } else {
        stat_name <- paste0(stat_name, op, printnum(x$parameter[grep("df", names(x$parameter), ignore.case = TRUE)], digits = printdigits), cp)
      }
    }
  }

  # Assemble p-value
  p <- printp(x$p.value)
  if(!grepl("<|>", p)) eq <- "= " else eq <- ""

  apa.stat <- paste0("$", stat_name, " = ", stat, "$, $p ", eq, p, "$")
  apa.stat
}

apa_lmsummary <- function(x, op = "(", cp = ")", ci = ci, standardized = standardized) {
  coefs <- x$coefficients
  if(is.matrix(ci)) {
    coefs <- cbind(coefs, ci = ci)
  } else stop("Please supply estimates of confidence intervals.")

  p_pos <- grep("Pr|p-value", colnames(coefs))
  if(standardized) stat_name <- "\\beta" else stat_name <- "b"

  apa.stat <- apply(coefs, 1, function(y) {
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
  apa.stat <- c(apa.stat, `F-test` = f, `R2` = r2)
  apa.stat
}

#############################
## assumes object of class 'anova' and returns character string
##
make_f_test <- function(x, op = "(", cp = ")") {
  p_pos <- grep("Pr\\(>F\\)", names(x))
  p <- printp(x[p_pos])
  if(!grepl("<|>", p)) eq <- "= " else eq <- ""
  ftest <- paste0("$F", op, x["Df"], ",", x["Res.Df"], cp, " = ", printnum(x["F"]), "$, $p ", eq, p, "$")
  ftest
}


make_confint <- function(x, digits, gt1, zero) {
  ci <- printnum(x, digits = digits, gt1 = gt1, zero = zero)
  ci <- paste0("[", paste(ci, collapse = ", "), "]")
  ci
}
