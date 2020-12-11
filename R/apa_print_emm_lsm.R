#' Format statistics (APA 6th edition)
#'
#' Takes various \code{lsmeans} and \code{emmeans} objects to create formatted character strings to report the results in
#' accordance with APA manuscript guidelines.  \emph{These methods are not properly tested and should be
#' considered experimental.}
#'
#' @param x Object
# #' @param stat_name Character. If \code{NULL} (default) the name given in \code{x} (or a formally correct
#'    adaptation, such as \eqn{\chi^2} instead of "x-squared") is used for the \emph{test statistic}, otherwise the
# #'    supplied name is used.
#' @param est_name Character. If \code{NULL} (default) the name is guessed from the function call of the model object passed to \code{lsmeans}/\code{emmeans}.
#' @param contrast_names Character. An optional vector of names to identify calculated contrasts.
#' @param in_paren Logical. Indicates if the formated string will be reported inside parentheses.
#' @inheritDotParams printnum
#' @details
#'
#'    ADJUSTED CONFIDENCE INTERVALS
#'
#'    If \code{in_paren} is \code{TRUE} parentheses in the formated string, such as those surrounding degrees
#'    of freedom, are replaced with brackets.
#'
#' @return \code{apa_print()} returns a list containing the following components according to the input:
#'
#'    \describe{
#'      \item{\code{statistic}}{A character string giving the test statistic, parameters (e.g., degrees of freedom),
#'          and \emph{p} value.}
#'      \item{\code{estimate}}{A character string giving the descriptive estimates and confidence intervals if possible}
#'          % , either in units of the analyzed scale or as standardized effect size.
#'      \item{\code{full_result}}{A joint character string comprised of \code{est} and \code{stat}.}
#'      \item{\code{table}}{A data.frame containing the complete contrast table, which can be passed to \code{\link{apa_table}}.}
#'    }
#'
#' @family apa_print
#' @examples
#'    NULL
#' @method apa_print emmGrid
#' @export

apa_print.emmGrid <- function(x, ...) {
  ellipsis <- list(...)
  if(is.null(ellipsis$est_name)) {
    ellipsis$est_name <- est_name_from_call(x)
  }

  ellipsis$x <- summary(x, infer = TRUE)
  do.call("apa_print", ellipsis)
}


#' @rdname apa_print.emmGrid
#' @method apa_print summary_emm
#' @export

apa_print.summary_emm <- function(
  x
  , contrast_names = NULL
  # , stat_name = NULL
  , est_name = NULL
  , in_paren = FALSE
  , ...
) {
  if(class(x)[1] != "summary.ref.grid") validate(x, check_class = "summary_emm", check_NA = FALSE)
  # if(!is.null(stat_name)) validate(stat_name, check_class = "character")
  if(!is.null(est_name)) validate(est_name, check_class = "character")
  validate(in_paren, check_class = "logical", check_length = 1)
  if(!is.null(contrast_names)) validate(contrast_names, check_class = "character")

  # # Indentify joint_tests() output
  # if(attr(x, "estName") == "F.ratio") {
  #   apa_res <- apa_print_summary_emm_joint_tests(x, ...)
  #   return(apa_res)
  # }

  tidy_x <- data.frame(broom::tidy(x))

  conf_level <- get_emm_conf_level(x)
  ci_supplied <- !length(conf_level) == 0
  p_supplied <- "p.value" %in% colnames(x)
  if(!ci_supplied & !p_supplied) stop("Object 'x' includes neither confidence intervals nor test statistics (i.e., p-values). See '?lsmeans::summary' for details.")

  if(!ci_supplied) {
    warning("Object 'x' does not include confidence intervals. APA guidelines recommend to routinely report confidence intervals for all estimates.")

    conf_level <- NULL
  } else {
    if(conf_level < 1) conf_level <- conf_level * 100
    conf_level <- paste0(conf_level, "\\% CI")
  }

  if(!p_supplied) {
    warning("Object 'x' does not include test statistics (i.e., p-values).")

    df_colname <- NULL
    stat_colnames <- NULL
  } else {
    df_colname <- names(tidy_x)[grepl("df\\.*", names(tidy_x))]
    dfdigits <- as.numeric(x[[df_colname]] %%1 > 0) * 2
    dfdigits <- ifelse(is.na(dfdigits), 0, dfdigits) # In case df are Inf
    multiple_df <- !isTRUE(all.equal(max(x[[df_colname]]), min(x[[df_colname]])))
    p_value <- names(tidy_x)[grepl("p.value", names(tidy_x), fixed = TRUE)]
    stat_colnames <- c("statistic", df_colname, p_value)
  }


  # Assamble table

  ## Add split variables
  split_by <- attr(x, "by.vars") # lsmeans
  if(is.null(split_by)) split_by <- unlist(attr(x, "misc")[c("by.vars", "pri.vars")]) # emmeans
  pri_vars <- attr(x, "pri.vars")
  factors <- c(pri_vars, split_by)


  ## Typeset columns
  sanitzied_contrasts <- sanitize_terms(tidy_x[, factors])
  tidy_x[, factors] <- prettify_terms(tidy_x[, factors])

  tidy_x$estimate <- printnum(tidy_x$estimate, ...)

  if(ci_supplied) {
    tidy_x$conf.int <- unlist(print_confint(tidy_x[, c("conf.low", "conf.high")]), ...)
  } else {
    tidy_x$std.error <- NULL
  }

  if(p_supplied) {
    if(all(tidy_x$null.value == 0)) tidy_x$null.value <- NULL

    tidy_x$statistic <- printnum(tidy_x$statistic)
    tidy_x$df <- printnum(tidy_x$df, digits = dfdigits)
    tidy_x[[p_value]] <- printp(tidy_x[[p_value]])
  }

  ## Reorder columns
  tidy_x <- cbind(
    tidy_x[, 1:which(colnames(tidy_x) == "estimate")]
    , tidy_x[which(colnames(tidy_x) == "conf.int")]
    , tidy_x[which(colnames(tidy_x) == "null.value")]
    , tidy_x[, stat_colnames] # Will be NULL if not supplied
  )

  ## Add variable labels
  variable_labels(tidy_x) <- c(estimate = paste0("$", est_name, "$"))

  if("contrast" %in% factors) {
    variable_label(tidy_x) <- c(contrast = "Contrast")
  }

  if(ci_supplied) {
    variable_labels(tidy_x$conf.int) <- conf_level
  }

  if(p_supplied) {
    test_stat <- if(all(tidy_x$df == Inf)) "z" else if(!multiple_df) paste0("t(", unique(tidy_x$df), ")") else paste0("t")
    variable_labels(tidy_x) <- c(statistic = paste0("$", test_stat, "$"))
    variable_labels(tidy_x[[p_value]]) <- if(p_value == "p.value") "$p$" else if(p_value == "adj.p.value") "$p_{adj}$"

    if("null.value" %in% names(tidy_x)) {
      variable_labels(tidy_x) <- c(null.value = "$\\mu_0$")
    }

    if(multiple_df) { # Put df in column heading
      variable_label(tidy_x) <- c(, df = "$\\mathit{df}$")
    }
  }

  tidy_xs <- default_label(tidy_x) # Add default labels for stratifying factors

  ## Add contrast names
  # rownames(tidy_x) <- if(!is.null(contrast_names)) contrast_names else tidy_x$contrast
  # tidy_x <- tidy_x[, which(colnames(tidy_x) != "contrast")]
  if(!is.null(contrast_names)) tidy_x$contrast <- contrast_names
  if(length(factors) > 1) {
    contrast_row_names <- apply(tidy_x[, c(factors[which(factors != "contrast")], factors[which(factors == "contrast")])], 1, paste, collapse = "_")
  } else {
    contrast_row_names <- tidy_x[, factors]
  }

  rownames(tidy_x) <- sanitize_terms(
    gsub( # Leading or double underscores from simple contrasts where there are dots in some columns that are replaced by ""
      "^\\_|\\_(\\_)", "\\1"
      , gsub(" |\\.", "", contrast_row_names)
    )
  )

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

    # contrast_list <- split(x, x[, factors])
    # contrast_list <- lapply(contrast_list, str_column, i)
    # test <- do.call(rbind, contrast_list[grep(paste(levels(unlist(x[, factors[2]])), collapse = "|"), names(contrast_list))])
    # prep_table <- merge_tables(
    #   contrast_list
    #   , row_names = rep(FALSE, length(contrast_list))
    #   , added_stub_head = paste(split_by, collapse = "_")
    # )
    # prep_table <- lapply(prep_table, function(x) {
    #   x[, split_by] <- x[1, split_by]
    #   x
    # })
    # tidy_x <- do.call(rbind, prep_table)
    # tidy_x <- droplevels(tidy_x)
  # } else {
  #   tidy_x <- x
  # }
  }



  if(any(tidy_x[, factors] == ".")) {
    tidy_x[, factors] <- apply(tidy_x[, factors], 2, gsub, pattern = "\\.", replacement = "")
  }

  # Concatenate character strings and return as named list
  apa_res <- init_apa_results()

  if(ci_supplied) {
    apa_res$estimate <- apply(tidy_x, 1, function(y) {
      paste0("$", est_name, " = ", y["estimate"], "$, ", conf_level, " ", y["ci"])
    })
  } else {
    apa_res$estimate <- apply(tidy_x, 1, function(y) {
      paste0("$", est_name, " = ", y["estimate"], "$")
    })
  }

  if(p_supplied) {
    apa_res$statistic <- apply(tidy_x, 1, function(y) {
      paste0("$t(", y["df"], ") = ", y["statistic"], "$, $p ", add_equals(y["p.value"]), "$")
    })

    # tidy_x[, df_colname] <- NULL

    apa_res$full_result <- paste(apa_res$est, apa_res$stat, sep = ", ")
    names(apa_res$full_result) <- names(apa_res$est)
  }

  apa_res[] <- lapply(apa_res, as.list) # use [] to preserve class

  if(p_supplied) {
    if(!multiple_df) { # Remove df column and put df in column heading
      df <- tidy_x$df[1]
      tidy_x <- tidy_x[, which(colnames(tidy_x) != "df")]
      # colnames(tidy_x) <- c("estimate", "ci", "statistic", "p.value")
      # tidy_x$ci <- as.character(tidy_x$ci)
      variable_label(tidy_x) <- c(
        estimate = paste0("$", est_name, "$")
        , conf.int = conf_level
        , statistic = paste0("$t(", df, ")$")
        , p.value = "$p$"
      )
    }
  }

  apa_res$table <- tidy_x
  attr(apa_res$table, "class") <- c("apa_results_table", "data.frame")

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
  validate(x, check_class = "summary.ref.grid", check_NA = FALSE)
  apa_print.summary_emm(x, ...)
}


# apa_print_summary_emm_joint_tests <- function(x, ...) {
#
# }



get_emm_conf_level <- function(x) {
  lsm_messages <- attr(x, "mesg")
  conf_level_message <- lsm_messages[grepl("Confidence level", lsm_messages)]
  ci <- unlist(regmatches(conf_level_message, gregexpr("0\\.\\d+", conf_level_message, useBytes = TRUE)))
  as.numeric(ci)
}

est_name_from_call <- function(x) {
  # analysis_function <- as.character(attr(x, "model.info")$call[[1]])
  contains_contrasts <- attr(x, "misc")$pri.vars[1] == "contrast" || attr(x, "misc")$estType %in% c("paris", "contrast")

  # roles <- attr(x, "roles")
  # is_multivariate <- all(roles$predictors %in% roles$multresp) ||
  #   (roles$predictors == "contrast" & length(roles$multresp) > 0)
  # contains_responses <- attr(x, "misc")$estType == "response"

  if(x@misc$inv.lbl == "ratio" && !is.null(x@misc$predict.type) && x@misc$predict.type == "response") {
    est_name <- "Ratio" # This could certainly be refined (e.g., OR, ratio of geometric means etc.)
  } else {
    est_name <- "M"
    # est_name <- switch(
    #   analysis_function
    #   , aov = { "M" }
    #   , lm = ifelse(is_multivariate, "M", "b")
    #   # , glm = ifelse(contains_responses, "\\mathit{OR}", "b")
    #   # , lmer = "b"
    #   # , mixed = "M"
    #   , { warning("apa_print() support for this model class is untested. Proceed with caution! Visit https://github.com/crsh/papaja/issues to request support for this model class."); "Estimate" }
    # )

    if(est_name != "Estimate") {
      if(contains_contrasts) est_name <- paste("\\Delta", est_name)
    }
  }

  est_name
}
