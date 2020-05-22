#' Format statistics (APA 6th edition)
#'
#' Takes \code{htest} objects from various statistical methods to create
#' formatted character strings to report the results in accordance with APA manuscript guidelines.
#'
#' @param x \code{htest} object. See details.
#' @param stat_name Character. If \code{NULL} (default) the name given in \code{x} (or a formally correct
#'    adaptation, such as \eqn{\chi^2} instead of "x-squared") is used for the \emph{test statistic}, otherwise the
#'    supplied name is used. See details.
#' @param est_name Character. If \code{NULL} (default) the name given in \code{x} (or a formally correct
#'    adaptation, such as \eqn{r_S} instead of "rho") is used for the \emph{estimate}, otherwise the supplied name is
#'    used. See details.
#' @param n Numeric. Size of the sample; required when reporting \eqn{\chi^2} tests, otherwise this parameter
#'    is ignored.
#' @param ci Numeric. If \code{NULL} (default) the function tries to obtain confidence intervals from \code{x}.
#'    Other confidence intervals can be supplied as a \code{vector} of length 2 (lower and upper boundary, respectively)
#'    with attribute \code{conf.level}, e.g., when calculating bootstrapped confidence intervals.
#' @param in_paren Logical. Indicates if the formatted string will be reported inside parentheses. See details.
#' @inheritDotParams printnum
#' @details The function should work on a wide range of \code{htest} objects. Due to the large number of functions
#'    that produce these objects and their idiosyncrasies, the produced strings may sometimes be inaccurate. If you
#'    experience inaccuracies you may report these \href{https://github.com/crsh/papaja/issues}{here} (please include
#'    a reproducible example in your report!).
#'
#'    \code{stat_name} and \code{est_name} are placed in the output string and are thus passed to pandoc or LaTeX through
#'    \pkg{knitr}. Thus, to the extent it is supported by the final document type, you can pass LaTeX-markup to format the
#'    final text (e.g., \code{\\\\tau} yields \eqn{\tau}).
#'
#'    If \code{in_paren} is \code{TRUE} parentheses in the formatted string, such as those surrounding degrees
#'    of freedom, are replaced with brackets.
#'
#' @return \code{apa_print()} returns a list containing the following components according to the input:
#'
#'    \describe{
#'      \item{\code{statistic}}{A character string giving the test statistic, parameters (e.g., degrees of freedom),
#'          and \emph{p} value.}
#'      \item{\code{estimate}}{A character string giving the descriptive estimates and confidence intervals if possible}
#'          % , either in units of the analyzed scale or as standardized effect size.
#'      \item{\code{full_result}}{A joint character string comprised of \code{estimate} and \code{statistic}.}
#'      \item{\code{table}}{A data.frame, which can be passed to \code{\link{apa_table}}.}
#'    }
#'
#' @family apa_print
#' @examples
#' # Comparisions of central tendencies
#' t_stat <- t.test(extra ~ group, data = sleep)
#' apa_print(t_stat)
#' apa_print(t_stat, stat_name = "tee")
#'
#' wilcox_stat <- wilcox.test(extra ~ group, data = sleep)
#' apa_print(wilcox_stat)
#'
#' # Correlations
#' ## Data from Hollander & Wolfe (1973), p. 187f.
#' x <- c(44.4, 45.9, 41.9, 53.3, 44.7, 44.1, 50.7, 45.2, 60.1)
#' y <- c( 2.6,  3.1,  2.5,  5.0,  3.6,  4.0,  5.2,  2.8,  3.8)
#' cor_stat <- cor.test(x, y, method = "spearman")
#' apa_print(cor_stat)
#'
#' # Contingency tables
#' ## Data from Fleiss (1981), p. 139.
#' smokers  <- c(83, 90, 129, 70)
#' patients <- c(86, 93, 136, 82)
#' prop_stat <- prop.test(smokers, patients)
#' apa_print(prop_stat, n = sum(patients))
#' @method apa_print htest
#' @export

apa_print.htest <- function(
  x
  , stat_name = NULL
  , est_name = NULL
  , n = NULL
  , ci = NULL
  , in_paren = FALSE
  , ...
) {
  validate(x, check_class = "htest")
  if(!is.null(stat_name)) validate(stat_name, check_class = "character", check_length = 1)
  if(!is.null(est_name)) validate(est_name, check_class = "character", check_length = 1)
  if(!is.null(n)) validate(n, check_class = "numeric", check_integer = TRUE, check_range = c(0, Inf), check_length = 1)
  if(!is.null(ci)) validate(ci, check_class = "matrix", check_length = 2)
  validate(in_paren, check_class = "logical", check_length = 1)

  args <- list(...)


  # coerce htest to a proper data frame ----

  if(length(x$estimate) == 2L) {
    x$estimate <- unname(diff(rev(x$estimate)))
    names(x$estimate) <- names(x$null.value)
  }
  if(length(x$estimate) > 2L) x$estimate <- NULL

  if(!is.null(x$conf.int)) x$conf.int <- list(x$conf.int)
  if(is.null(x$conf.int)) x$conf.int <- NULL
  if(is.null(x$parameter)) x$parameter <- NULL

  x$null.value  <- NULL
  x$alternative <- NULL
  x$method      <- NULL
  x$data.name   <- NULL

  x_list <- lapply(x, FUN = function(x) {
    if(!is.null(x)) {
      matrix(x, nrow = 1, dimnames = list(NULL, names(x)))
    }
  })

  y <- as.data.frame(
    x_list
    , stringsAsFactors = FALSE
  )

  # Call sanitize_table with args ----

  if(any(c("cor", "rho", "tau") %in% colnames(y)) & is.null(args$gt1)) args$gt1 <- FALSE

  args$x <- y
  x <- do.call("sanitize_table", args)


  # htest-specific modifications ----
  if(is.null(n)) n <- y$sample.size
  if("$\\chi^2$" %in% unlist(variable_labels(x))){
    if(is.null(n)) {
      stop("Please provide the sample size to report.")
    } else {
      n <- paste0(", n = ", n)
    }
  } else {
    n <- NULL
  }

  if(!is.null(est_name)) {
    # todo: if estimate not in table
    variable_label(x) <- c(estimate = paste0("$", est_name, "$"))
  }
  if(!is.null(stat_name)) {
    # todo: if statistic not in table
    variable_label(x) <- c(statistic = paste0("$", stat_name, "$"))
  }
  if(!is.null(ci)) {
    args$x <- sort(as.numeric(ci))
    x$conf.int[] <- paste0(
      "["
      , paste(do.call("printnum", args), collapse = ", ")
      , "]"
    )
  }

  # Build output container ----
  apa_res <- apa_print_container()


  # estimate ----

  estimate_list <- list()
  estimate_list$sep <- ", "

  if(!is.null(x$estimate)) {
    estimate_list$estimate <- paste0(
      gsub(variable_label(x$estimate), pattern = "\\$$", replacement = " ")
      , add_equals(x$estimate)
      , "$"
    )
  }
  if(!is.null(x$conf.int)) {
    estimate_list$conf.int <- paste0(
      attr(x$conf.int, "conf.level") * 100
      , "\\% CI "
      ,
      gsub(
        x = gsub(
          x = gsub(x = gsub(
            x = x$conf.int, pattern = "\\$", replacement = ""
          ), pattern = "\\[", replacement = "$[")
          , pattern = "\\]", replacement = "]$")
        , pattern = ", ", replacement = "$, $")
    )
  }
  apa_res$estimate <- do.call("paste", estimate_list)
  if(in_paren) apa_res$estimate <- in_paren(apa_res$estimate)

  # statistic ----
  dfs <- NULL
  if(!is.null(x$df)) dfs <- paste0("(", x$df, n, ")")
  if(!is.null(x$df1) && !is.null(x$df2)) dfs <- paste0("(", x$df1, ", ", x$df2, ")")

  stat_list <- list()
  stat_list$sep <- ", "

  if(!is.null(x$statistic)) {
    stat_list$statistic <- paste0(
      gsub(x = variable_label(x$statistic), pattern = "\\$$", replacement = "")
      , dfs
      , " "
      , add_equals(x$statistic)
      , "$"
    )
  }
  if(!is.null(x$p.value)) {
    stat_list$p.value <- paste0(
      "$p "
      , add_equals(x$p.value)
      , "$"
    )
  }

  apa_res$statistic <- do.call("paste", stat_list)
  if(in_paren) apa_res$statistic <- in_paren(apa_res$statistic)

  # full_result ----
  apa_res$full_result <- do.call(
    "paste"
    , list(apa_res$estimate, apa_res$statistic, sep = ", ")
  )

  apa_res[1:3] <- lapply(X = apa_res[1:3], FUN = function(x){
    if(length(x) > 1L) return(as.list(x))
    if(length(x) == 1L) return(x)
    NULL
  })

  apa_res$table <- x

  apa_res
}
