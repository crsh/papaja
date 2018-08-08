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
  # if(!is.null(stat_name)) validate(est_name, check_class = "character")
  if(!is.null(est_name)) validate(est_name, check_class = "character")
  validate(in_paren, check_class = "logical", check_length = 1)
  if(!is.null(contrast_names)) validate(contrast_names, check_class = "character")

  ci <- get_emm_conf_level(x)
  ci_supplied <- !length(ci) == 0
  p_supplied <- "p.value" %in% colnames(x)
  if(!ci_supplied & !p_supplied) stop("Object 'x' includes neither confidence intervals nor test statistics (i.e., p-values). See '?lsmeans::summary' for details.")

  if(!ci_supplied) {
    warning("Object 'x' does not include confidence intervals. APA guidelines recommend to routinely report confidence intervals for all estimates.")

    conf_level <- NULL
  } else {
    if(ci < 1) ci <- ci * 100
    conf_level <- paste0(ci, "\\% CI")
  }

  if(!p_supplied) {
    warning("Object 'x' does not include test statistics (i.e., p-values).")

    df_colname <- NULL
    stat_colnames <- NULL
  } else {
    dfdigits <- as.numeric(x$df %%1 > 0) * 2
    dfdigits <- ifelse(is.na(dfdigits), 0, dfdigits) # In case df are Inf
    df_colname <- "df"
    stat_colnames <- c("statistic", "p.value")
  }

  split_by <- attr(x, "by.vars") # lsmeans
  if(is.null(split_by)) split_by <- attr(x, "misc")$by.vars # emmeans
  pri_vars <- attr(x, "pri.vars")
  factors <- c(pri_vars, split_by)


  # Assamble table
  contrast_table <- data.frame(x)
  contrast_table[, factors] <- printnum(contrast_table[, factors])

  if("null" %in% colnames(contrast_table)) { # This could go in a table note
    contrast_table$null <- NULL
  }

  contrast_table <- rename_column(
    contrast_table
    , c("lsmean", "emmean", "ratio")
    , "estimate"
  )
  contrast_table <- rename_column(contrast_table, "SE", "std.error")

  if("contrast" %in% pri_vars) {
    variable_label(contrast_table) <- c(contrast = "Contrast")
  }

  contrast_table$estimate <- printnum(contrast_table$estimate, ...)
  variable_label(contrast_table) <- c(estimate = paste0("$", est_name, "$"))

  if(p_supplied) {
    contrast_table <- rename_column(contrast_table, c("t.ratio", "z.ratio"), "statistic")
    contrast_table$statistic <- printnum(contrast_table$statistic)
    contrast_table$df <- printnum(contrast_table$df, digits = dfdigits)
    contrast_table$p.value <- printp(contrast_table$p.value)
    variable_label(contrast_table) <- c(p.value = "$p$")

    if(length(unique(contrast_table$df)) == 1) { # Remove df column and put df in column heading
      variable_label(contrast_table) <- c(
        statistic = paste0("$t(", contrast_table$df[1], ")$")
      )
    } else {
      variable_label(contrast_table) <- c(statistic = "$t$", df = "$df$")
    }
  }

  if(ci_supplied) {
    contrast_table <- rename_column(contrast_table, c("lower.CL", "asymp.LCL"), "ll")
    contrast_table <- rename_column(contrast_table, c("upper.CL", "asymp.UCL"), "ul")

    ci_table <- data.frame(confint = unlist(print_confint(matrix(c(contrast_table$ll, contrast_table$ul), ncol = 2), margin = 2, conf_level = NULL, ...)))
    contrast_table$std.error <- NULL
    contrast_table$ll <- NULL
    contrast_table$ul <- NULL

    contrast_table <- cbind(
      contrast_table[, 1:which(colnames(contrast_table) == "estimate")]
      , ci_table
      , contrast_table[, c(df_colname, stat_colnames)] # Will be NULL if not supplied
    )
    contrast_table$confint <- as.character(contrast_table$confint)
    contrast_table <- rename_column(contrast_table, "confint", "ci")
    variable_label(contrast_table) <- c(ci = conf_level)
  } else {
    contrast_table$std.error <- NULL
  }

  ## Add contrast names
  # rownames(contrast_table) <- if(!is.null(contrast_names)) contrast_names else contrast_table$contrast
  # contrast_table <- contrast_table[, which(colnames(contrast_table) != "contrast")]
  if(!is.null(contrast_names)) contrast_table$contrast <- contrast_names
  if(length(factors) > 1) {
    rownames(contrast_table) <- sanitize_terms(
      gsub( # Leading or double underscores from simple contrasts where there are dots in some columns that are replaced by ""
        "^\\_|\\_(\\_)", "\\1"
        , gsub(" |\\.", "", apply(contrast_table[, factors], 1, paste, collapse = "_"))
      )
    )
  } else {
    rownames(contrast_table) <- sanitize_terms(
      gsub( # Leading or double underscores from simple contrasts where there are dots in some columns that are replaced by ""
        "^\\_|\\_(\\_)", "\\1"
        , gsub(" |\\.", "", contrast_table[, factors])
      )
    )
  }

  ## Add structuring columns
  if(length(factors) > 1 && !any(contrast_table[, factors] == ".")) {

    factors[-which(factors == "contrast")] <- rev(factors[-which(factors == "contrast")])
    str_factors <- rev(c(pri_vars[-1], split_by))
    str_cols <- contrast_table[, str_factors, drop = FALSE]
    for(i in seq_along(str_factors)) {
      if(i > 1) {
        tmp <- apply(contrast_table[, str_factors[1:i]], 1, paste, collapse = "_")
      } else {
        tmp <- contrast_table[, str_factors[i]]
      }
      str_cols[, str_factors[i]] <- as.character(contrast_table[, str_factors[i]])
      str_cols[duplicated(tmp), str_factors[i]] <- ""
    }
    contrast_table[, str_factors] <- str_cols[, str_factors]
    str_col_order <- c(str_factors, colnames(contrast_table)[!colnames(contrast_table) %in% str_factors])
    contrast_table <- contrast_table[, str_col_order]
    contrast_table[, pri_vars[1]] <- as.character(contrast_table[, pri_vars[1]])

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
    # contrast_table <- do.call(rbind, prep_table)
    # contrast_table <- droplevels(contrast_table)
  # } else {
  #   contrast_table <- x
  # }
  }

  # Concatenate character strings and return as named list

  apa_res <- apa_print_container()

  if(ci_supplied) {
    apa_res$estimate <- apply(contrast_table, 1, function(y) {
      paste0("$", est_name, " = ", y["estimate"], "$, ", conf_level, " ", y["ci"])
    })
  } else {
    apa_res$estimate <- apply(contrast_table, 1, function(y) {
      paste0("$", est_name, " = ", y["estimate"], "$")
    })
  }

  if(p_supplied) {
    apa_res$statistic <- apply(contrast_table, 1, function(y) {
      if(!grepl("<|>", y["p.value"])) eq <- "= " else eq <- ""

      paste0("$t(", y["df"], ") = ", y["statistic"], "$, $p ", eq, y["p.value"], "$")
    })

    contrast_table[, df_colname] <- NULL

    apa_res$full_result <- paste(apa_res$est, apa_res$stat, sep = ", ")
    names(apa_res$full_result) <- names(apa_res$est)
  }

  apa_res <- lapply(apa_res, as.list)

  apa_res$table <- contrast_table
  attr(apa_res$table, "class") <- c("apa_results_table", "data.frame")

  apa_res
}


#' @rdname apa_print.emmGrid
#' @export

apa_print.lsmobj <- function(x, ...) {
  apa_print.emmGrid(x, ...)
}

#' @rdname apa_print.emmGrid
#' @export

apa_print.summary.ref.grid <- function(x, ...) {
  validate(x, check_class = "summary.ref.grid", check_NA = FALSE)
  apa_print.summary_emm(x, ...)
}

get_emm_conf_level <- function(x) {
  lsm_messages <- attr(x, "mesg")
  conf_level_message <- lsm_messages[grepl("Confidence level", lsm_messages)]
  ci <- unlist(regmatches(conf_level_message, gregexpr("0\\.\\d+", conf_level_message, useBytes = TRUE)))
  as.numeric(ci)
}

est_name_from_call <- function(x) {
  # analysis_function <- as.character(attr(x, "model.info")$call[[1]])
  contains_contrasts <- attr(x, "misc")$pri.vars[1] == "contrast"

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
