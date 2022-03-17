#' Validate Function Input
#'
#' This function can be used to validate the input to functions.
#' *This function is not exported.*
#'
#' @param x Function input.
#' @param name Character. Name of variable to validate; if \code{NULL} variable name of object supplied to \code{x} is used.
#' @param check_class Character. Name of class to expect.
#' @param check_mode Character. Name of mode to expect.
#' @param check_integer Logical. If \code{TRUE} an object of type \code{integer} or a whole number \code{numeric} is expected.
#' @param check_NA Logical. If \code{TRUE} an non-\code{NA} object is expected.
#' @param check_infinite Logical. If \code{TRUE} a finite object is expected.
#' @param check_length Integer. Length of the object to expect.
#' @param check_dim Numeric. Vector of object dimensions to expect.
#' @param check_range Numeric. Vector of length 2 defining the expected range of the object.
#' @param check_cols Character. Vector of columns that are intended to be in a \code{data.frame}.
#'
#' @importFrom methods is
#' @keywords internal
#' @examples
#' \dontrun{
#' in_paren <- TRUE # Taken from apa_num()
#' validate(in_paren, check_class = "logical", check_length = 1)
#' validate(in_paren, check_class = "numeric", check_length = 1)
#' }

validate <- function(
  x
  , name = NULL
  , check_class = NULL
  , check_mode = NULL
  , check_integer = FALSE
  , check_NA = TRUE
  , check_infinite = TRUE
  , check_length = NULL
  , check_dim = NULL
  , check_range = NULL
  , check_cols = NULL
) {
  if(is.null(name)) name <- deparse(substitute(x))

  if(is.null(x)) stop(paste("The parameter '", name, "' is NULL.", sep = ""))

  if(!is.null(check_dim) && !all(dim(x) == check_dim)) stop(paste("The parameter '", name, "' must have dimensions " , paste(check_dim, collapse=""), ".", sep = ""))
  if(!is.null(check_length) && length(x) != check_length) stop(paste("The parameter '", name, "' must be of length ", check_length, ".", sep = ""))

  if((is.null(check_class) || !check_class == "function") && any(is.na(x))) {
    if(check_NA) stop(paste("The parameter '", name, "' is NA.", sep = ""))
    else return(TRUE)
  }

  if(check_infinite && inherits(x, "numeric") && any(is.infinite(x))) stop(paste("The parameter '", name, "' must be finite.", sep = ""))
  if(check_integer && inherits(x, "numeric") && any(x %% 1 != 0)) stop(paste("The parameter '", name, "' must be an integer.", sep = ""))

  for(x.class in check_class) {
    if(!methods::is(x, x.class)) stop(paste("The parameter '", name, "' must be of class '", x.class, "'.", sep = ""))
  }

  for (x.mode in check_mode) {
    if(!check_mode %in% mode(x)) stop(paste("The parameter '", name, "' must be of mode '", x.mode, "'.", sep = ""))
  }

  if(!is.null(check_cols)) {
    test <- check_cols %in% colnames(x)

    if(!all(test)) {
      stop(paste0("Variable '", check_cols[!test], "' is not present in your data.frame.\n"))
    }
  }

  if(!is.null(check_range) && any(x < check_range[1] | x > check_range[2])) stop(paste("The parameter '", name, "' must be between ", check_range[1], " and ", check_range[2], ".", sep = ""))
  TRUE
}


#' Create Empty Container for Results
#'
#' Creates the default empty container for the results of [apa_print()].
#' *This function is not exported.*
#'
#' @return
#'    A named list (with additional class `apa_results`) containing the following components:
#'
#'    \describe{
#'      \item{`estimate`}{A (named list of) character strings giving effect-size estimates.}
#'      \item{`statistic`}{A (named list of) character strings giving test statistic, parameters, and *p* values.}
#'      \item{`full_result`}{A (named list of) character strings comprised of `estimate` and `statistic`` for each factor.}
#'      \item{`table`}{A data frame containing all results; can, for example, be passed to [apa_table()].
#'      }
#'    }
#' @keywords internal

init_apa_results <- function(){
  structure(
    list(
      estimate = NULL
      , statistic = NULL
      , full_result = NULL
      , table = NULL
    )
    , class = c("apa_results", "list")
  )
}


#' Transform to a Canonical Table
#'
#' Internal function that puts a data frame into a canonical structure by
#' renaming and labelling columns.
#'
#' @param x          A data frame.
#' @param stat_label Character. Label for column `statistic`.
#' @param est_label  Character. Label for column `estimate`.
#'
#' @keywords internal

canonize <- function(
  x
  , stat_label = NULL
  , est_label = NULL
  , interval_type = "CI"
) {

  # args <- list(...)
  if(!is.null(stat_label)) validate(stat_label, check_class = "character", check_length = 1L)
  if(!is.null(est_label))  validate(est_label, check_class = "character", check_length = 1L)


  new_labels <- c(
    lookup_labels
    , "estimate"     = est_label
    , "Estimate"     = est_label
    , "coefficients" = est_label
    , "statistic"    = stat_label
  )

  colnames(x) <- make.names(colnames(x))
  names_in_lookup_names <- colnames(x) %in% names(lookup_names)

  warning_unexpected <- "\nThis implies that your output object was not fully understood by `apa_print()`.
  Therefore, be careful when using its output. Moreover, please visit https://github.com/crsh/papaja/issues and
  file an issue together with the code that generated the output object. In doing so, you help us to fully
  support the type of analysis you just conducted and make papaja a little bit better."

  if(!all(names_in_lookup_names)) {
    warning("Some columns could not be renamed: '", paste(colnames(x)[!names_in_lookup_names], collapse = "', '"), "'", warning_unexpected)
  }

  variable_labels(x) <- new_labels[intersect(colnames(x), names(new_labels))]
  colnames(x)[names_in_lookup_names] <- lookup_names[colnames(x)[names_in_lookup_names]]

  if(!is.null(x$conf.int)) {
    conf_level <- attr(x$conf.int, "conf.level")
    if(is.null(conf_level)) {
      conf_level <- attr(x$conf.int, "conf.level") <- attr(x$conf.int[[1]], "conf.level")
    }
    if(is.null(conf_level) && !is.null(names(x$conf.int[[1]]))) {
      suppressWarnings(
        conf_level <- as.numeric(
          gsub("[^.|\\d]", "", names(x$conf.int[[1]]), perl = TRUE)
        )
      )
      conf_level <- if(anyNA(conf_level)) NULL else conf_level
      if(!is.null(conf_level)) conf_level <- (100 - conf_level[1] * 2) / 100
    }

    conf_label <- paste0(
      if(!is.null(conf_level)) paste0(conf_level * 100, "\\% ")
      , interval_type
    )

    variable_labels(x) <- c("conf.int" = conf_label)
  }

  # Adjust labels if dfs were corrected ----
  # - Hierarchical Linear Models: Kenward-Roger and Satterthwaite
  # - Repeated-measures ANOVA   : Greenhouse-Geisser and Huyhn-Feldt

  df_correction_type <- attr(x, "df_correction")
  if(!is.null(df_correction_type) && df_correction_type != "none") {
    variable_label(x) <- c(
      df = paste0("$\\mathit{df}^{\\mathrm{", df_correction_type, "}}$")
      , df.residual = paste0("$\\mathit{df}_{\\mathrm{res}}^{\\mathrm{", df_correction_type, "}}$")
    )
  }

  x
}

#' Beautify a Canonical Table
#'
#' Internal function that takes an object created by [canonize()] and
#' applies proper rounding. Term names are beautified by removing parentheses and replacing
#' colons with `"$\\times$"`. Moreover, both rows and columns are sorted.
#'
#' @param x An object created by \code{\link{canonize}}.
#' @param standardized Logical. If TRUE the name of the function \code{scale} will be removed from term names.
#' @param ... Further arguments that may be passed to \code{\link{apa_num}} to format estimates (i.e., columns \code{estimate} and \code{conf.int}).
#' @keywords internal

beautify <- function(x, standardized = FALSE, use_math = FALSE, args_stat = NULL, ...) {

  validate(x, check_class = "data.frame", check_infinite = FALSE)
  validate(standardized, check_class = "logical", check_length = 1L) # we could vectorize here!

  args <- list(...)

  y <- x

  # apply apa_num ----
  for (i in colnames(y)) {
    if(i == "p.value") {
      y[[i]] <- apa_p(y[[i]])
    } else if(i %in% c("df", "df.residual", "multivariate.df", "multivariate.df.residual")) {
      y[[i]] <- apa_df(y[[i]])
    } else if(i == "conf.int") {
      tmp <- unlist(lapply(X = y[[i]], FUN = function(x, ...){
        apa_interval(x, use_math = use_math, ...)
      }, ...))
      variable_label(tmp) <- variable_label(y[[i]])
      y[[i]] <- tmp
    } else if (i == "estimate") {
      args$x <- y[[i]]
      y[[i]] <- do.call("apa_num", args)
    } else if (i == "statistic") {
      args_stat$x <- y[[i]]
      y[[i]] <- do.call("apa_num", args_stat)
    } else if (i == "term"){
      y[[i]] <- beautify_terms(as.character(y[[i]]), standardized = standardized)
    } else if (i == "model") {
      tmp <- beautify_model(as.character(y[[i]]), standardized = standardized)
      variable_label(tmp) <- variable_label(y[[i]])
      y[[i]] <- tmp
    } else {
      y[[i]] <- apa_num(y[[i]])
    }
  }

  # rearrange ----
  y <- sort_columns(y)

  identifier <- intersect(colnames(y), c("term", "model"))
  stopifnot(length(identifier) <= 1L)

  if(length(identifier) > 0) {
    perm <- term_order(y[[identifier]])
    y <- y[perm, , drop = FALSE]
    attr(y, "sanitized_term_names") <- sanitize_terms(unlabel(x[[identifier]])[perm])
  }

  rownames(y) <- NULL
  class(y) <- c("apa_results_table", "data.frame")
  y
}


#' Sort ANOVA or Regression Table by Predictors/Effects
#'
#' Sort rows in ANOVA or regression tables produced by [apa_print()]
#' by complexity (i.e., main effects, two-way interactions, three-way interactions, etc.).
#'
#' @param x       A data frame. For example, the table element produced by [apa_print()].
#' @param colname Character. Column name of the \code{data.frame} containing the terms to sort.
#'
#' @return Returns the same data.frame with reordered rows.
#' @keywords internal
#' @export
#'
#' @examples
#' ## From Venables and Ripley (2002) p. 165.
#' npk_aov <- aov(yield ~ block + N * P * K, npk)
#' npk_aov_results <- apa_print(npk_aov)
#' sort_terms(npk_aov_results$table, "term")

sort_terms <- function(x, colname) {
  validate(x, check_class = "data.frame", check_cols = colname)

  perm <- term_order(x[[colname]])
  x[perm, , drop = FALSE]
}

term_order <- function(x) {
  order(
    unlist(
      lapply(
        X = strsplit(x, split = "\\times", fixed = TRUE)
        , FUN = length
      )
    )
  )
}


#' Sort the Columns of an APA Results Table
#'
#' An internal function that sorts the columns of a `data.frame` according to
#' our standards.
#'
#' @param x A \code{data.frame} with standardized column names.
#' @keywords internal

sort_columns <- function(x) {
  multivariate <- paste0("multivariate.", c("statistic", "df", "df.residual"))

  se <- NULL
  if(!any(colnames(x) == "conf.int")) se <- "std.error"

  ordered_cols <- intersect(c("model", "term", "estimate", "conf.int", se, "alternative", multivariate, "statistic", "df", "df.residual", "mse", "p.value", "mcmc.error"), colnames(x))
  x[, ordered_cols, drop = FALSE]
}


rename_column <- function(x, current_name, new_name) {
  colnames(x)[colnames(x) %in% current_name] <- new_name[current_name %in% colnames(x)]
  x
}


#' @keywords internal

remove_residuals_row <- function(x) {

  resid_row <- apply(X = x, MARGIN = 1L, FUN = anyNA)

  if(any(resid_row)) {
    stopifnot(sum(resid_row) == 1)
    x$sumsq_err <- x$sumsq[resid_row]
    x$df.residual <- x$df[resid_row]
    tinylabels::variable_label(x) <- c(df.residual = "$\\mathit{df}_{\\mathrm{res}}$")
    x[!resid_row, , drop = FALSE]
  } else {
    x
  }
}


#' @keywords internal

determine_within_between <- function(data, id, factors) {

  data <- droplevels(data)

  number_of_levels <- function(x) {
    length(unique(x))
  }

  within <- NULL
  between <- NULL

  for (i in factors) {
    n_levels <- stats::aggregate(x = data[[i]], by = list(data[[id]]), FUN = number_of_levels)
    if(any(n_levels$x>1)) {
      within <- c(within, i)
    } else {
      between <- c(between, i)
    }
  }

  # return
  list(
    "within" = within
    , "between" =  between
  )
}

#' Remove Incomplete Observations from Data Frame
#'
#' This is an internal function that is used to remove incomplete observations
#' from a \code{data.frame}. It removes (1) explicit NAs and (2) cases with
#' implicit NAs, i.e. participants who did not provide observations for all
#' combinations of (possibly multiple) within-subjects factors.
#'
#' @param data The \code{data.frame} to be processed.
#' @param id Character. Name of the column containing the subject identifier.
#' @param within Character. Names of the columns containing within-subjects factors.
#' @param dv Character. Name of the column containing the dependent variable.
#' @return
#'   A \code{data.frame} where NAs and incomplete observations are removed.
#'   It also has up to two additional attributes \code{removed_cases_explicit_NA}
#'   and \code{removed_cases_implicit_NA}, carrying the subject identifiers of
#'   participants whose data has been removed.
#'
#' @keywords internal

complete_observations <- function(data, id, within, dv) {

  explicit_NA <- NULL
  implicit_NA <- NULL

  # explicit NAs
  if(anyNA(data[[dv]])) {
    excluded_id <- sort(unique(data[[id]][is.na(data[[dv]])]))
    data <- data[!(data[[id]] %in% excluded_id), ]
    data[[id]] <- droplevels(data[[id]])

    explicit_NA <- as.character(excluded_id)
  }

  # implicit NAs
  if(length(within) > 0) {
    cross_table <- table(data[, c(id, within)])
    obs_per_person <- apply(X = cross_table, MARGIN = 1, FUN = sum)
    within_combinations <- prod(unlist(lapply(X = data[, within, drop = FALSE], FUN = nlevels)))

    if(any(obs_per_person != within_combinations)) {
      excluded_id <- names(obs_per_person[obs_per_person != within_combinations])

      data <- data[!data[[id]] %in% excluded_id, ]
      data[[id]] <- droplevels(data[[id]])
      implicit_NA <- excluded_id
    }
  }
  attr(data, "removed_cases_explicit_NA") <- explicit_NA
  attr(data, "removed_cases_implicit_NA") <- implicit_NA

  data
}


#' Select Parameters
#'
#' If a \code{list} holds vectors of parameter values, this function extracts the i-th parameter value from each vector and creates
#' a new \code{list} with these values. Especially helpful if a function is called repeatedly via \code{do.call} with different
#' parameter values from within a function.
#'
#' @param x List. A list of parameter values.
#' @param i Integer. The i-th element of each vector that is to be extracted.
#'
#' @keywords internal
#' @examples
#' NULL

sel <- function(x, i){
  x <- x[(i-1)%%length(x)+1]
  return(x)
}


#' Set Defaults
#'
#' A helper function that is intended for internal use. A list \code{ellipsis} may be manipulated by overwriting (via \code{set}) or adding (via \code{set.if.null}) list elements.
#'
#' @param ellipsis A \code{list}, usually a list that comes from an ellipsis
#' @param set A named  \code{list} of parameters that are intended to be set.
#' @param set.if.null A named \code{list} of parameters that are intended to be set if and only if the parameter is not already in \code{ellipsis}.
#' @keywords internal

defaults <- function(ellipsis, set = NULL, set.if.null = NULL) {

  ellipsis <- as.list(ellipsis)

  for (i in names(set)) {
    ellipsis[[i]] <- set[[i]]
  }
  for (i in names(set.if.null)) {
    if(is.null(ellipsis[[i]])) ellipsis[[i]] <- set.if.null[[i]]
  }
  return(ellipsis)
}


#' Package Available
#'
#' Internal function to check if a specified package is installed.

#' @param x Character. Name of the package to be checked.
#' @return Logical. Is the specified package installed?
#' @keywords internal

package_available <- function(x) requireNamespace(x, quietly = TRUE)

no_method <- function(x) {
  stop(
    "Objects of class '"
    , class(x)
    , "' are currently not supported (no method defined)."
    , "\nVisit https://github.com/crsh/papaja/issues to request support for this class."
    , call. = FALSE
  )
}
