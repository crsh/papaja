#' Typeset Statistical Results from Estimated Marginal Means
#'
#' Takes various \pkg{emmeans} objects to create formatted character strings to
#' report the results in accordance with APA manuscript guidelines.
#' \emph{\pkg{emmeans} supports a wide range of analyses, not all
#' of which are currently (fully) supported. Proceed with caution.}
#'
#' @param x Object
#' @param est_name Character. If `NULL` (default) the name of the estimate
#' is inferred from the function call of the model object supplied to
#' \pkg{emmeans}.
#' @param contrast_names Character. An optional vector of names to label the
#' calculated contrasts.
#' @param conf.int Numeric. Confidence level for confidence intervals.
#' @inheritParams emmeans::summary.emmGrid
#' @inheritParams glue_apa_results
#' @inheritDotParams apa_num.numeric
#' @details
#'
#' When p-values and confidence intervals are adjusted for multiple testing,
#' the correction method is added as an index to the output (e.g.
#' `p_{Tukey(3)}`). Values in parenthesis indicate the size of the family of
#' tests or the rank of the set of linear functions (for the Scheffé method).
#'
#' If possible, each family of tests is additionally marked in the returned
#' table by alphabetic superscripts.
#'
#' Generally, the `summary_emm` objects returned by `emmeans::summary_emm` omit
#' information that may be needed to add some of the information on the
#' adjustments made to p-values and confidence intervals. It is therefore
#' preferable to pass `emmGrid`-objects if possible. For example, by using
#' `emmeans(object, 1 ~ x1, adjust = "scheffe")`.
#'
#' @evalRd apa_results_return_value()
#'
#' @family apa_print
#' @examples
#'   # From the emmeans manual:
#'   library(emmeans)
#'   warp.lm <- lm(breaks ~ wool*tension, data = warpbreaks)
#'   warp.emm <- emmeans(warp.lm, ~ tension | wool)
#'   warp.contr <- contrast(warp.emm, "poly")
#'
#'   apa_print(warp.contr)
#'
#'   # In this example, because degrees of freedom are equal across all rows
#'   # of the output, it is possible to move that information to the variable
#'   # labels. This is useful if a compact results table is required:
#'
#'   df_into_label(apa_print(warp.contr))
#'
#' @method apa_print emmGrid
#' @export

apa_print.emmGrid <- function(x, infer = TRUE, conf.int = 0.95, ...) {

  ellipsis_ci <- deprecate_ci(conf.int = conf.int, ...)
  conf.int <- ellipsis_ci$conf.int
  ellipsis <- ellipsis_ci$ellipsis

  if(is.null(ellipsis$est_name)) {
    ellipsis$est_name <- est_name_from_call(x)
  }

  ellipsis$x <- summary(x, infer = infer, level = conf.int)

  # Add family size, because it gets lost otherwise
  famSize <- attr(x, "misc")$famSize
  if(!is.null(famSize)) attr(ellipsis$x, "famSize") <- famSize

  do.call("apa_print", ellipsis)
}


#' @rdname apa_print.emmGrid
#' @method apa_print summary_emm
#' @export

apa_print.summary_emm <- function(
  x
  , contrast_names = NULL
  # , stat_name = NULL
  , est_name = "\\hat{\\theta}"
  , in_paren = FALSE
  , ...
) {
  if(class(x)[1] != "summary.ref.grid") validate(x, check_class = "summary_emm", check_NA = FALSE)
  # if(!is.null(stat_name)) validate(stat_name, check_class = "character")
  if(!is.null(est_name)) validate(est_name, check_class = "character")
  validate(in_paren, check_class = "logical", check_length = 1)
  if(!is.null(contrast_names)) validate(contrast_names, check_class = "character")

  # Indentify joint_tests() output
  if(isTRUE(attr(x, "estName") == "F.ratio")) {
    apa_res <- apa_print_summary_emm_joint_tests(x, in_paren = in_paren, ...)
    return(apa_res)
  }

  null.value <- x$null.value
  if(is.null(null.value)) {
    null.value <- 0
  }
  # delta <- attr(x, "delta")
  # one.sided <- attr(x, "side")


  # Attempt to extract family size from messages for summary-objects
  object_messages <- attr(x, "mesg")

  if(!is.null(object_messages)) {
    family_size_mesg <- .str_extract_first(
      object_messages
      , "for (\\d+) (estimates|tests)"
    )
    family_size_mesg <- family_size_mesg[!length(family_size_mesg) == 0]

    if(length(family_size_mesg) > 0) {
      fam_size <- as.numeric(.str_extract_first(family_size_mesg, "\\d+"))
      attr(x, "famSize") <- unique(fam_size)
    }
  }

  adjust <- parse_adjust_name(
    attr(x, "adjust")
    , attr(x, "famSize")
    , object_messages
  )

  tidy_x <- data.frame(broom::tidy(x, null.value = null.value))

  # Hot fix while we wait on a merge of this PR: https://github.com/tidymodels/broom/pull/1047
  tidy_x <- rename_column(
    tidy_x
    , c("prob", "rate", "ratio", "odds.ratio", "asymp.LCL", "asymp.UCL", "z.ratio")
    , c("estimate", "estimate", "estimate", "estimate", "conf.low", "conf.high", "statistic")
  )

  conf_level <- get_emm_conf_level(x)
  ci_supplied <- !length(conf_level) == 0
  p_supplied <- "p.value" %in% colnames(x)
  # if(!ci_supplied & !p_supplied) stop("Object 'x' includes neither confidence intervals nor test statistics (i.e., p-values). See '?emmeans::summary' for details.")

  if(!ci_supplied) {
    if(p_supplied) {
      warning("Object 'x' does not include confidence intervals. APA guidelines recommend to routinely report confidence intervals for all estimates.")
    }

    conf_level <- NULL
  } else {
    if(conf_level < 1) conf_level <- conf_level * 100
    if(is.null(adjust)) {
      conf_level <- paste0(conf_level, "\\% CI")
    } else {
      conf_level <- paste0("$", conf_level, "\\%\\ \\mathrm{CI}_\\mathrm{\\scriptstyle ", adjust["conf.int"], "}$")
    }
  }

  if(!p_supplied) {
    # warning("Object 'x' does not include test statistics (i.e., p-values).")

    df_colname <- NULL
    stat_colnames <- NULL
  } else {
    df_colname <- names(tidy_x)[grepl("df\\.*", names(tidy_x))]
    multiple_df <- !isTRUE(all.equal(min(tidy_x[[df_colname]]), max(tidy_x[[df_colname]])))
    p_value <- names(tidy_x)[grepl("p.value", names(tidy_x), fixed = TRUE)]
    stat_colnames <- c("statistic", df_colname, p_value)
  }

  # Assemble table

  ## Add split variables
  split_by <- attr(x, "by.vars") # lsmeans
  pri_vars <- attr(x, "pri.vars")
  if(is.null(split_by)) { # emmeans
    split_by <- unlist(attr(x, "misc")[c("by.vars")])
  }
  if(is.null(pri_vars)) {
    pri_vars <- unlist(attr(x, "misc")[c("pri.vars")])
  }
  factors <- unique(c(pri_vars, split_by))

  # One-sided regression coefficient test via emtrends(. ~ 1, var = ...)
  # factors <- gsub("^1$", "X1", factors)

  ## Typeset columns
  tidy_x[, factors] <- beautify_terms(tidy_x[, factors], ...)

  tidy_x$estimate <- apa_num(tidy_x$estimate, ...)

  if(ci_supplied) {
    tidy_x$conf.int <- unlist(
      apa_confint(
        tidy_x[, c("conf.low", "conf.high")]
        , use_math = FALSE
      )
      , ...
    )
  }

  if(p_supplied) {
    if(all(tidy_x$null.value == 0)) tidy_x$null.value <- NULL

    tidy_x$statistic <- apa_num(tidy_x$statistic)
    tidy_x$df <- apa_df(tidy_x$df)
    tidy_x[[p_value]] <- apa_p(tidy_x[[p_value]])
  }

  if(!is.null(contrast_names)) tidy_x$contrast <- contrast_names

  ## Reorder columns
  tidy_x <- cbind(
    tidy_x[, 1:which(colnames(tidy_x) == "estimate")]
    , tidy_x[which(colnames(tidy_x) == "conf.int")]
    , tidy_x[which(colnames(tidy_x) == "null.value")]
    , tidy_x[, stat_colnames] # Will be NULL if not supplied
  )

  ## Add variable labels
  tidy_x_labels <- c(estimate = paste0("$", est_name, "$"))

  if("contrast" %in% factors) {
    tidy_x_labels <- c(tidy_x_labels, contrast = "Contrast")
  }

  if(ci_supplied) {
    tidy_x_labels <- c(tidy_x_labels, conf.int = conf_level)
  }

  if(p_supplied) {
    if(all(tidy_x$df == "$\\infty$")) {
      test_stat <- "z"
      tidy_x$df <- NULL
    } else {
      test_stat <- "t"
      tidy_x_labels <- c(tidy_x_labels, df = "$\\mathit{df}$")
    }

    tidy_x_labels <- c(tidy_x_labels, statistic = paste0("$", test_stat, "$"))

    if("null.value" %in% names(tidy_x)) {
      tidy_x_labels <- c(tidy_x_labels, null.value = "$\\theta_0$")
    }

    variable_labels(tidy_x[[p_value]]) <- switch(
      p_value
      , "p.value" = "$p$"
      , "adj.p.value" = paste0("$p_\\mathrm{\\scriptstyle ", adjust["p.value"], "}$")
    )
  }

  variable_labels(tidy_x) <- tidy_x_labels

  # Add default labels for stratifying factors
  tidy_x <- default_label(tidy_x)

  ## Add contrast names
  # rownames(tidy_x) <- if(!is.null(contrast_names)) contrast_names else tidy_x$contrast
  # tidy_x <- tidy_x[, which(colnames(tidy_x) != "contrast")]

  if(length(factors) > 1) {
    contrast_row_names <- apply(
      tidy_x[, c(factors[which(factors != "contrast")], factors[which(factors == "contrast")])]
      , MARGIN = 1L
      , FUN = paste
      , collapse = "_"
    )
  } else if(length(factors) == 1) {
    contrast_row_names <- tidy_x[, factors, drop = TRUE]
  } else {
    stop("Could not determine names to address each result by.")
  }
  terms_sanitized <- sanitize_terms(contrast_row_names)

  ## Mark test families (see below)
  if(!is.null(attr(x, "famSize"))) {
    if(!is.null(split_by)) {
      family_mark <- letters[as.numeric(interaction(tidy_x[, split_by]))]
    } else if(!attr(x, "famSize") < nrow(tidy_x)) {
      family_mark <- letters[1:nrow(tidy_x)]
    } else {
      adjust <- NULL # No marks
    }
  } else {
    adjust <- NULL # No marks
  }

  ## Add structuring columns
  if(length(factors) > 1 && !any(tidy_x[, factors] == ".")) {

    factors[-which(factors == "contrast")] <- rev(factors[-which(factors == "contrast")])
    str_factors <- rev(c(pri_vars[-1], split_by))
    str_cols <- tidy_x[, str_factors, drop = FALSE]
    for(i in seq_along(str_factors)) {
      if(i > 1) {
        tmp <- apply(tidy_x[, str_factors[1:i]], 1, paste, collapse = "_")
      } else {
        tmp <- tidy_x[, str_factors[i]]
      }
      str_cols[, str_factors[i]] <- as.character(tidy_x[, str_factors[i]])
      str_cols[duplicated(tmp), str_factors[i]] <- ""
    }
    tidy_x[, str_factors] <- str_cols[, str_factors]
    str_col_order <- c(str_factors, colnames(tidy_x)[!colnames(tidy_x) %in% str_factors])
    tidy_x <- tidy_x[, str_col_order]
    tidy_x[, pri_vars[1]] <- as.character(tidy_x[, pri_vars[1]])
  }

  if(any(tidy_x[, factors] == ".")) {
    tidy_x[, factors] <- apply(
      tidy_x[, factors]
      , 2
      , gsub
      , pattern = "."
      , replacement = ""
      , fixed = TRUE
    )
  }

  # One-sided regression coefficient test via emtrends(. ~ 1, var = ...)
  # tidy_x["X1"] <- NULL

  # Concatenate character strings and return as named list
  class(tidy_x) <- c("apa_results_table", "data.frame")

  apa_res <- glue_apa_results(
    tidy_x
    , est_glue = est_glue(tidy_x)
    , stat_glue = stat_glue(tidy_x)
    , in_paren = in_paren
    , term_names = make.names(terms_sanitized)
  )

  ## Mark test families
  if(!is.null(adjust)) {
    if(ci_supplied) {
      ci_label <- variable_label(apa_res$table$conf.int)
      apa_res$table$conf.int <- paste0(apa_res$table$conf.int, "${}^", family_mark, "$")
      variable_label(apa_res$table$conf.int) <- ci_label
    }
    if(p_supplied) {
      p_label <- variable_label(apa_res$table[[p_value]])
      apa_res$table[[p_value]] <- paste0(apa_res$table[[p_value]], "${}^", family_mark, "$")
      variable_label(apa_res$table[[p_value]]) <- p_label
    }
  }

  apa_res
}


#' @rdname apa_print.emmGrid
#' @method apa_print lsmobj
#' @export

apa_print.lsmobj <- function(x, ...) {
  apa_print.emmGrid(x, ...)
}

#' @rdname apa_print.emmGrid
#' @method apa_print summary.ref.grid
#' @export

apa_print.summary.ref.grid <- function(x, ...) {
  apa_print.summary_emm(x, ...)
}


apa_print_summary_emm_joint_tests <- function(x, in_paren, ...) {

  # Retain structuring variables
  by_vars <- attr(x, "by.vars")
  if(!is.null(by_vars)) {
    canonical_table <- canonize(x[, -which(names(x) %in% by_vars)])
    by_vars <- default_label(
      as.data.frame(
        lapply(x[by_vars], as.character)
        , stringsAsFactors = FALSE
        , check.names = FALSE
      )
    )
  } else {
    canonical_table <- canonize(x)
  }

  tinylabels::variable_labels(canonical_table) <- c(term = "Effect")

  ellipsis <- list(...)
  ellipsis <- defaults(
    ellipsis
    , set.if.null = list(
      digits = 3L
      , gt1 = FALSE
    )
  )
  ellipsis$x <- canonical_table
  beautiful_table <- do.call("beautify", ellipsis)
  term_names <- attr(beautiful_table, "sanitized_term_names")

  if(!is.null(by_vars)) {
    beautiful_table <- as.data.frame(
      c(by_vars, beautiful_table)
      , stringsAsFactors = FALSE
      , check.names = FALSE
    )

    class(beautiful_table) <- c("apa_results_table", "data.frame")

    term_names <- sanitize_terms(
      apply(cbind(term_names, by_vars), 1, paste, collapse = "_")
    )
  }

  # Glue results
  glue_apa_results(
    beautiful_table
    , est_glue = construct_glue(beautiful_table, "estimate")
    , stat_glue = construct_glue(beautiful_table, "statistic")
    , term_names = term_names
    , in_paren = in_paren
    , est_first = FALSE
    , simplify = FALSE
  )
}

get_emm_conf_level <- function(x) {
  lsm_messages <- attr(x, "mesg")
  conf_level_message <- lsm_messages[grepl("Confidence level", lsm_messages)]
  conf_level <- .str_extract_first(conf_level_message, "0\\.\\d+")
  as.numeric(conf_level)
}

est_name_from_call <- function(x) {
  # analysis_function <- as.character(attr(x, "model.info")$call[[1]])

  misc <- x@misc

  contains_contrasts <- misc$pri.vars[1] == "contrast" || misc$estType %in% c("pairs", "contrast", "rbind")

  type <- misc$predict.type
  if (is.null(type)) type <- "link"
  inv <- (type %in% c("response", "mu", "unlink"))

  link <- NULL
  if(is.character(misc$tran) && (misc$tran != "none")) {
    link <- misc$tran
  } else if(is.character(misc$orig.tran) && (misc$orig.tran != "none")) {
    link <- misc$orig.tran
  }

  # TODO: Review estimate names!
  if ((!inv || is.null(misc$tran)) && !is.null(link)) {
    # See https://github.com/rvlenth/emmeans/blob/be94689c22df5fae23287ce2b7ed7a21232fb32b/R/transformations.R
    est_name <- switch(
      link
      , "logit"     = "\\mathrm{logit}(p)"
      , "probit"    = "\\Phi^{-1}(p)"
      , "cauchit"   = "\\mathrm{cauchit}(p)"
      , "cloglog"   = "\\mathrm{cloglog}(p)"
      , "log"       = "\\log(M)"
      , "sqrt"      = "\\sqrt{M}"
      , "log.o.r."  = "\\log(\\mathit{OR})"
      , "exp"       = "\\exp(M)"
      , "inverse"   = "1/M"
      , "M"
    )
  } else {
    est_name <- switch(
      c(misc$inv.lbl, "NULL")[1] # In case it's NULL return default
      , "prob" = "p"
      , "odds.ratio" = "\\mathit{OR}"
      , "M"
    #   analysis_function
    #   , aov = { "M" }
    #   , lm = ifelse(is_multivariate, "M", "b")
    #   # , glm = ifelse(contains_responses, "\\mathit{OR}", "b")
    #   # , lmer = "b"
    #   # , mixed = "M"
    #   , { warning("apa_print() support for this model class is untested. Proceed with caution! Visit https://github.com/crsh/papaja/issues to request support for this model class."); "Estimate" }
    )
  }

  if(contains_contrasts) {
    if(!is.null(misc$inv.lbl) && (inv & !is.null(misc$tran))) {
      est_name <- switch(
        misc$inv.lbl
        , "ratio" = "M_{i}/M_{j}"
        , "odds.ratio" = "\\mathit{OR}"
        , est_name
      )
    } else {
      est_name <- switch(
        c(link, "NULL")[1] # In case it's NULL return default
        , "log.o.r." = est_name
        , est_name <- paste("\\Delta", est_name)
      )
    }
  }

  est_name
}

parse_adjust_name <- function(x, n = NULL, mesg = NULL) {
  if(is.null(x) || x == "none") return(NULL)

  res <- lookup_adjust_names(x)

  # Attempt to use rank instead of family size
  if(x == "scheffe") {
    n <- NULL

    rank_mesg <- .str_extract_first(
      mesg
      , "rank (\\d+)$"
    )
    rank_mesg <- rank_mesg[!length(rank_mesg) == 0]

    if(length(rank_mesg) > 0) {
      n <- as.numeric(.str_extract_first(rank_mesg, "\\d+"))
    }
  }

  if(is.null(n)) {
    message("The size of the family of tests could not be determined and will be omitted from the output. Consider passing the output of `contrast()` rather than `summary()`.")
    return(res)
  } else {
    setNames(paste0(res, paste0("(", n, ")")), names(res))
  }
}
