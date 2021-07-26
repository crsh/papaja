#' Format Statistics (APA 6th edition)
#'
#' Takes `htest` objects from various statistical methods (e.g., [t.test()], [wilcox.text()], [cor.test()]) to create
#' formatted character strings to report the results in accordance with APA manuscript guidelines.
#'
#' @param x An `htest` object. See details.
#' @param stat_name Character. If `NULL` (the default), the name given in `x` (or a formally correct
#'    adaptation, such as \eqn{\chi^2} instead of "x-squared") is used for the *test statistic*, otherwise the
#'    supplied name is used. See details.
#' @param est_name Character. If `NULL` (the default), the name given in `x` (or a formally correct
#'    adaptation, such as \eqn{r_S} instead of "rho") is used for the *estimate*, otherwise the
#'    supplied name is used. See details.
#' @param n Numeric. Sample size; required when reporting chi-squared tests, otherwise this parameter
#'    is ignored.
#' @param ci Numeric. If \code{NULL} (the default), the function tries to obtain confidence intervals from `x`.
#'    Other confidence intervals can be supplied as a `vector` of length 2 (lower and upper boundary, respectively)
#'    with attribute `conf.level` set, e.g., when calculating bootstrapped confidence intervals.
#' @inheritParams glue_apa_results
#' @inheritDotParams printnum
#' @details The function should work on a wide range of `htest` objects. Due to the large number of functions
#'    that produce these objects and their idiosyncrasies, the produced strings may sometimes be inaccurate. If you
#'    experience inaccuracies you may report these [here]{https://github.com/crsh/papaja/issues} (please include
#'    a reproducible example in your report).
#'
#'    \code{stat_name} and \code{est_name} are placed in the output string and are thus passed to pandoc or LaTeX through
#'    \pkg{knitr}. Thus, to the extent it is supported by the final document type, you can pass LaTeX-markup to format the
#'    final text (e.g., \code{\\\\tau} yields \eqn{\tau}).
#'
#' @return
#'   A list (of additional class `apa_results`) containing the following components is returned:
#'
#'    \describe{
#'      \item{`statistic`}{
#'        A character string giving the test statistic, parameters (e.g., degrees of freedom), and *p* value.
#'      }
#'      \item{`estimate`}{
#'        A character string giving the descriptive estimates and confidence intervals if possible,
#'        either in units of the analysed scale or as standardized effect size.
#'      }
#'      \item{`full_result`}{
#'        A joint character string combining `estimate` and `statistic`.
#'      }
#'      \item{`table`}{
#'        A data frame, which can be passed to [apa_table()].
#'      }
#'    }
#'
#' @family apa_print
#' @examples
#' # Comparisons of central tendencies
#' t_stat <- t.test(extra ~ group, data = sleep)
#' apa_print(t_stat)
#' apa_print(t_stat, stat_name = "tee")
#'
#' wilcox_stat <- wilcox.test(extra ~ group, data = sleep, exact = FALSE)
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
  if(!is.null(ci)) validate(ci, check_class = "numeric", check_length = 2)
  validate(in_paren, check_class = "logical", check_length = 1)

  ellipsis <- list(...)

  # Arrange table, i.e. coerce 'htest' to a proper data frame ----

  if(length(x$estimate) == 2L) {
    if(all(grepl("mean", names(x$estimate)))) {
      # two-sample t test
      x$estimate <- unname(diff(rev(x$estimate)))
      names(x$estimate) <- "difference in means"
    } else if(all(grepl("prop", names(x$estimate))) && is.null(x$null.value)){
      # 2-sample test for **equality** of proportions
      x$estimate <- unname(diff(rev(x$estimate)))
      names(x$estimate) <- "difference in proportions"
    } else {
      x$estimate <- NULL
    }
  }
  if(length(x$estimate) > 2L) x$estimate <- NULL

  if(is.null(ci)) {
    conf_int <- list(x$conf.int)
  } else {
    conf_int <- list(ci)
  }



  if(is.null(x$parameter)) x$parameter <- NULL

  x$conf.int    <- NULL
  x$null.value  <- NULL
  x$alternative <- NULL
  x$method      <- NULL
  x$data.name   <- NULL

  x_list <- list()

  for (i in names(x)) {
    if(is.null(names(x[[i]]))) {
      x_list[[i]] <- x[[i]]
    } else {
      x_list[names(x[[i]])] <- unname(x[[i]])
    }
  }

  y <- as.data.frame(
    x_list
    , stringsAsFactors = FALSE
  )
  if(!identical(conf_int, list(NULL))) y$conf.int <- conf_int

  # sanitize table ----
  ellipsis$x <- canonize(y)

  # Prettify table ----
  if(any(c("cor", "rho", "tau") %in% colnames(y)) & is.null(ellipsis$gt1)) ellipsis$gt1 <- FALSE
  x <- do.call("beautify", ellipsis)


  # htest-specific modifications ----
  if(is.null(n)) n <- y$sample.size
  if("$\\chi^2$" %in% unlist(variable_labels(x))) {
    if(is.null(n)) {
      stop("Please provide the sample size to report.")
    # } else {
    #   n <- paste0(", n = ", n)
    }
  } else {
    n <- NULL
  }

  if(!is.null(est_name)) {
    # todo: if estimate not in table
    if(!("estimate" %in% colnames(x))) stop("No estimate available in results table.")
    variable_label(x) <- c(estimate = paste0("$", est_name, "$"))
  }
  if(!is.null(stat_name)) {
    # todo: if statistic not in table
    if(!("statistic" %in% colnames(x))) stop("No statistic available in results table.")
    variable_label(x) <- c(statistic = paste0("$", stat_name, "$"))
  }

  # Create output object ----
  glue_apa_results(
    x
    , n = as.integer(n)
    , est_glue = construct_glue(x, "estimate")
    , stat_glue = construct_glue(x, "statistic")
    , term_names = sanitize_terms(y$term)
    , in_paren = in_paren
  )
}
