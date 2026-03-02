#' Typeset Statistical Results from Meta-Analytic Models (metafor)
#'
#' Formats model coefficients from \pkg{metafor} objects of class `"rma"` (e.g.,
#' `rma.uni`, `rma.mv`, `rma.glmm`) for reporting in accordance with APA style.
#'
#' @param x A fitted meta-analytic model of class `"rma"` (from metafor).
#' @param conf.int Numeric specifying the required confidence level (e.g., .95).
#'   If `NULL`, the method uses `x$level` when available (metafor stores this
#'   as a percentage, e.g., 95).
#' @param in_paren Logical. Passed to `glue_apa_results()`.
#' @param est_name Optional character. Label used for the coefficient column.
#'   If `NULL`, defaults to `\\hat{\\theta}`.
#' @param ... Further arguments passed to `beautify()`/`apa_num()` (e.g.,
#'   `digits`, `decimal.mark`, `gt1`).
#'
#' @evalRd apa_results_return_value()
#'
#' @family apa_print
#' @rdname apa_print.rma
#'
#' @method apa_print rma
#' @export
apa_print.rma <- function(
    x
    , conf.int = NULL
    , in_paren = FALSE
    , est_name = NULL
    , ...
) {

  # --- dependency check (metafor is in Suggests in most setups) ----
  if(!requireNamespace("metafor", quietly = TRUE)) {
    stop("Package 'metafor' must be installed to use apa_print() for 'rma' objects.")
  }

  # --- input validation / compatibility with papaja patterns ----
  ellipsis <- list(...)

  if(!inherits(x, "rma")) stop("Parameter 'x' must inherit from class 'rma' (e.g., 'rma.uni', ...).")
  if(!is.null(conf.int)) validate(conf.int, check_class = "numeric"  , check_length = 1L)
  if(!is.null(est_name)) validate(est_name, check_class = "character", check_length = 1L)

  # --- determine CI level for metafor (metafor uses percentages) ----

  if(is.null(conf.int)) {
    if(is.null(x$level)) { # if no confidence level can be inferred
      conf.int <- .95
    } else {
      # see the complicated rules in ?metafor::misc_options...
      if(x$level >= 1) { # level contains coverage percentages
        conf.int <- x$level / 100
      } else if(x$level < .5) { # 'level' contains alpha, e.g., .05
        conf.int <- (1 - x$level)
        if(x$level == 0) conf.int <- 0 # level = 0 is always treated as a 0% confidence level
      } else { # 'level' contains coverage proportions
        conf.int <- x$level
      }
    }
  }

  # --- coefficient table from metafor summary ----
  sx <- summary(x, level = conf.int * 100) # which is why we use the 'weird'level

  # coef(summary.rma) returns a data frame with estimate, se, zval/tval, pval, ci.lb, ci.ub
  ct <- as.data.frame(stats::coef(sx))

  # keep term names
  term_names <- rownames(ct)
  rownames(ct) <- NULL

  # build papaja-style results table
  res_table <- ct
  res_table$Term <- term_names

  # build papaja-style conf.int column (list per row), then remove bounds
  if(all(c("ci.lb", "ci.ub") %in% colnames(res_table))) {
    ci_mat <- cbind(res_table$ci.lb, res_table$ci.ub)
    res_table$conf.int <- apply(ci_mat, 1, function(z) as.list(z))
    attr(res_table$conf.int, "conf.level") <- conf.int
    res_table$ci.lb <- NULL
    res_table$ci.ub <- NULL
    res_table$se <- NULL
  }

  # --- default estimate label ----
  if(is.null(est_name)) {
    est_name <- "$\\hat{\\theta}$"
  } else {
    est_name <- paste0("$", strip_math_tags(est_name), "$")
  }

  # --- canonize, beautify, glue ----
  ellipsis$x <- papaja:::canonize(res_table, est_label = est_name)
  beautiful_table <- do.call("beautify", ellipsis)

  glue_apa_results(
    beautiful_table
    , est_glue  = construct_glue(beautiful_table, "estimate")
    , stat_glue = construct_glue(beautiful_table, "statistic")
    , in_paren  = in_paren
    , simplify  = FALSE
  )
}

#' @rdname apa_print.rma
#'
#' @method apa_print rma.uni
#' @export

apa_print.rma.uni <- apa_print.rma
