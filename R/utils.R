#' Validate function input
#'
#' This function can be used to validate the input to functions. \emph{This function is not exported.}
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
#' in_paren <- TRUE # Taken from printnum()
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



#' Create empty container for results
#'
#' Creates the default empty container for the results of \code{\link{apa_print}}. \emph{This function is not exported.}
#'
#' @return
#'    A named list containing the following components according to the input:
#'
#'    \describe{
#'      \item{\code{estimate}}{A (named list of) character strings giving effect size estimates.}
#'      \item{\code{statistic}}{A (named list of) character strings giving test statistic, parameters, and \emph{p} values.}
#'      \item{\code{full_report}}{A (named list of) character strings comprised of \code{estimate} and \code{statistic} for each factor.}
#'      \item{\code{table}}{A \code{data.frame} containing all results; can, for example, be passed to \code{\link{apa_table}}.}
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



#' Escape symbols for LaTeX output
#'
#' This function is a copy of the non-exported function \code{escape_latex} from the \pkg{knitr} package.
#' \emph{This function is not exported.}
#'
#' @param x Character.
#' @param newlines Logical. Determines if \code{\\n} are escaped.
#' @param spaces Logical. Determines if multiple spaces are escaped.
#'
#' @keywords internal
#' @examples
#' \dontrun{
#' in_paren <- TRUE # Taken from printnum()
#' validate(in_paren, check_class = "logical", check_length = 1)
#' validate(in_paren, check_class = "numeric", check_length = 1)
#' }

escape_latex <- function (x, newlines = FALSE, spaces = FALSE) {
  if(is.null(x)) return(x)

  x <- gsub("\\\\", "\\\\textbackslash", x)
  x <- gsub("([#$%&_{}])", "\\\\\\1", x)
  x <- gsub("\\\\textbackslash", "\\\\textbackslash{}", x)
  x <- gsub("~", "\\\\textasciitilde{}", x)
  x <- gsub("\\^", "\\\\textasciicircum{}", x)
  if (newlines)
    x <- gsub("(?<!\n)\n(?!\n)", "\\\\\\\\", x, perl = TRUE)
  if (spaces)
    x <- gsub("  ", "\\\\ \\\\ ", x)

  x
}


#' Convert name of statistic
#'
#' This function converts a character generated by R-functions that describes a statistic and converts it into the
#' corresponding character required by APA guidelines (6th edition). \emph{This function is not exported.}
#'
#' @param x Character.
#'
#' @keywords internal
#' @examples
#' \dontrun{
#' convert_stat_name("rho")
#' convert_stat_name("mean of the differences")
#' convert_stat_name("t")
#' }

convert_stat_name <- function(x) {
  validate(x, check_class = "character")

  new_stat_name <- gsub("-squared", "^2", x, ignore.case = TRUE)

  if(length(new_stat_name) == 2) {
    if(all(grepl("mean", new_stat_name))) {
      new_stat_name <- "\\Delta M"
    } else if(all(grepl("prop", new_stat_name))) {
      new_stat_name <- "\\Delta p"
    }
  }
  if(all(grepl("prop \\d", new_stat_name))) {
    new_stat_name <- NULL
    return(new_stat_name)
  }

  new_stat_name <- switch(
    new_stat_name
    , new_stat_name
    , cor = "r"
    , rho = "r_{\\mathrm{s}}"
    , tau = "\\uptau"
    , `mean of x` = "M"
    , `(pseudo)median` = "Mdn^*"
    , `mean of the differences` = "M_d"
    , `difference in location` = "Mdn_d"
    , `Bartlett's K^2` = "K^2"
  )

  new_stat_name <- gsub("x|chi", "\\\\chi", new_stat_name, ignore.case = TRUE)

  new_stat_name
}



#' Transform to a Canonical Table
#'
#' Internal function that transforms a \code{data.frame} by renaming and labeling columns.
#'
#' @param x A \code{data.frame}.
#' @param stat_label Label for column \code{statistic}.
#' @param est_label  Label for column \code{estimate}.
#'
#' @keywords internal

canonize <- function(
  x
  , stat_label = NULL
  , est_label = NULL
) {

  # args <- list(...)
  if(!is.null(stat_label)) validate(stat_label, check_class = "character", check_length = 1L)
  if(!is.null(est_label))  validate(est_label, check_class = "character", check_length = 1L)

  conf_level <- attr(x$conf.int, "conf.level")
  if(is.null(conf_level)) {
    conf_level <- attr(x$conf.int, "conf.level") <- attr(x$conf.int[[1]], "conf.level")
  }
  conf_label <- paste0(
    if(!is.null(conf_level)) paste0(conf_level * 100, "\\% ")
    , "CI"
  )

  colnames(x) <- make.names(colnames(x))

  new_labels <- c(
    lookup_labels
    , "estimate"     = est_label
    , "Estimate"     = est_label
    , "coefficients" = est_label
    , "conf.int"     = conf_label
    , "statistic"    = stat_label
  )

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
#' Internal function that takes an object created by \code{\link{canonize}} and
#' applies proper rounding. Term names are beautified by removing parentheses and replacing
#' colons with \code{$\\times$}. Moreover, both rows and columns are sorted.
#'
#' @param x An object created by \code{\link{canonize}}.
#' @param standardized Logical. If TRUE the name of the function \code{scale} will be removed from term names.
#' @param ... Further arguments that may be passed to \code{\link{printnum}} to format original-scale estimates (i.e., columns \code{estimate} and \code{conf.int}).
#' @keywords internal

beautify <- function(x, standardized = FALSE, use_math = FALSE, ...) {

  validate(x, check_class = "data.frame")
  validate(standardized, check_class = "logical", check_length = 1L) # we could vectorize here!

  args <- list(...)

  # apply printnum ----
  for (i in colnames(x)) {
    if(i == "p.value") {
      x[[i]] <- printp(x[[i]])
    } else if(i %in% c("df", "df.residual", "multivariate.df", "multivariate.df.residual")) {
      x[[i]] <- print_df(x[[i]])
    } else if(i == "conf.int") {
      tmp <- unlist(lapply(X = x[[i]], FUN = function(x, ...){
        # TODO: Switch to using print_interval here
        paste0("[", paste(printnum(x, use_math = use_math, ...), collapse = ", "), "]")
      }, ...))
      variable_label(tmp) <- variable_label(x[[i]])
      x[[i]] <- tmp
    } else if (i == "estimate"){
      args$x <- x[[i]]
      x[[i]] <- do.call("printnum", args)
    } else if (i == "term"){
      x[[i]] <- prettify_terms(x[[i]], standardized = standardized)  # todo: standardized ???
    } else {
      x[[i]] <- printnum(x[[i]])
    }
  }

  # rearrange ----
  x <- sort_columns(x)

  if(!is.null(x$term)) x <- sort_terms(x, "term")
  rownames(x) <- NULL

  class(x) <- c("apa_results_table", "data.frame")
  x
}




#' Sanitize term names
#'
#' Remove characters from term names that will be difficult to address using the \code{$}-operator. \emph{This function is
#' not exported.}
#'
#' @param x Character. Vector of term-names to be sanitized.
#' @param standardized Logical. If \code{TRUE} the name of the function \code{\link{scale}} will be
#'    removed from term names.
#'
#' @keywords internal
#' @examples
#' \dontrun{
#' sanitize_terms(c("(Intercept)", "Factor A", "Factor B", "Factor A:Factor B", "scale(FactorA)"))
#' }

sanitize_terms <- function(x, standardized = FALSE) {
  if(standardized) x <- gsub("scale\\(", "z_", x)   # Remove scale()
  x <- gsub("\\(|\\)|`", "", x)                     # Remove parentheses and backticks
  x <- gsub("\\W", "_", x)                          # Replace non-word characters with "_"
  x
}


#' Prettify Term Names
#'
#' Remove parentheses, replace colons with \code{$\\times$}. Useful to prettify term names in \code{apa_print()} tables.
#'
#' @param x Character. Vector of term-names to be prettified
#' @param standardized Logical. If \code{TRUE} the name of the function \code{\link{scale}} will be
#'    removed from term names.
#'
#' @examples
#' NULL
#' @keywords internal

prettify_terms <- function(x, standardized = FALSE) {
  if(standardized) x <- gsub("scale\\(", "", x)       # Remove scale()
  x <- gsub(pattern = "\\(|\\)|`|.+\\$", replacement = "", x = x)                 # Remove parentheses and backticks
  x <- gsub('.+\\$|.+\\[\\["|"\\]\\]|.+\\[.*,\\s*"|"\\s*\\]', "", x) # Remove data.frame names
  x <- gsub("\\_|\\.", " ", x)                        # Remove underscores
  for (i in seq_along(x)) {
    x2 <- unlist(strsplit(x[i], split = ":"))
    x2 <- capitalize(x2)
    x[i] <- paste(x2, collapse = " $\\times$ ")
  }
  x
}

capitalize <- function(x) {
  substring(x, first = 1, last = 1) <- toupper(substring(x, first = 1, last = 1))
  x
}


#' Select parameters
#'
#' If a \code{list} holds vectors of parameter values, this function extracts the i-th parameter value from each vector and creates
#' a new \code{list} with these values. Especially helpful if a function is call repeatedly via \code{do.call} with different
#' parameter values from within a function.
#'
#' @param x List. A list of parameter values
#' @param i Integer. The i-th element of each vector that is to be extracted.
#'
#' @keywords internal
#' @examples
#' NULL

sel <- function(x, i){
  x <- x[(i-1)%%length(x)+1]
  return(x)
}


#' Set defaults
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



#' Sort ANOVA or regression table by predictors/effects
#'
#' Sorts rows in ANOVA or regression tables produced by \code{\link{apa_print}}
#' by complexity (i.e., main effects, two-way interactions, three-way interactions, etc.).
#'
#' @param x data.frame. For example, a table element produced by \code{\link{apa_print}}.
#' @param colname Character. Column name of the \code{data.frame} containing the terms to sort.
#'
#' @return Returns the same data.frame with reordered rows.
#' @export
#'
#' @examples
#' ## From Venables and Ripley (2002) p. 165.
#' npk_aov <- aov(yield ~ block + N * P * K, npk)
#' npk_aov_results <- apa_print(npk_aov)
#' sort_terms(npk_aov_results$table, "term")

sort_terms <- function(x, colname) {
  validate(x, check_class = "data.frame", check_cols = colname)

  x[order(sapply(regmatches(x[[colname]], gregexpr("\\\\times", x[[colname]])), length)), ]
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

  ordered_cols <- intersect(c("term", "estimate", "conf.int", se, multivariate, "statistic", "df", "df.residual", "p.value"), colnames(x))
  x[, ordered_cols, drop = FALSE]
}


#' Corresponding author line
#'
#' Internal function. Construct corresponding-author line.
#'
#' @param x List. Meta data of the document as a result from \code{\link[yaml]{yaml.load}}.
#' @keywords internal

corresponding_author_line <- function(x) {
  apa_terms <- getOption("papaja.terms")

  if(is.null(x$name)) stop("\nPlease provide the corresponding author's name in the documents YAML front matter. Use the 'name' element of the 'author' list.\n")
  if(is.null(x$address)) stop("\nPlease provide the corresponding author's complete postal address in the documents YAML front matter. Use the 'address' element of the 'author' list.\n")
  if(is.null(x$email)) stop("\nPlease provide the corresponding author's e-mail address in the documents YAML front matter. Use the 'email' element of the 'author' list.\n")

  corresponding_line <- paste0(
    apa_terms$correspondence, x$name, ", "
    , x$address, ". "
    , apa_terms$email, ": ", x$email
  )

  corresponding_line
}

#' Define phrases according to locale
#'
#' Internal function. Defines phrases used throughout the manuscript.
#'
#' @param x Integer. Locale.
#' @keywords internal

localize <- function(x) {
  switch(
    x
    , list( # Default
      author_note = "Author note"
      , abstract = "Abstract"
      , keywords = "Keywords"
      , word_count = "Word count"
      , table = "Table"
      , figure = "Figure"
      , note = "Note"
      , correspondence = "Correspondence concerning this article should be addressed to "
      , email = "E-mail"
    )
    , german = list(
      author_note = "Anmerkung des Autors"
      , abstract = "Zusammenfassung"
      , keywords = "Schl\u00fcsselw\u00f6rter"
      , word_count = "Wortanzahl"
      , table = "Tabelle"
      , figure = "Abbildung"
      , note = "Anmerkung"
      , correspondence = "Schriftverkehr diesen Artikel betreffend sollte adressiert sein an "
      , email = "E-Mail"
    )
    , dutch = list(
      author_note = "Over de auteur"
      , abstract = "Samenvatting"
      , keywords = "Trefwoorden"
      , word_count = "Aantal woorden"
      , table = "Tabel"
      , figure = "Figuur"
      , note = "Opmerking"
      , correspondence = "Correspondentie betreffende dit artikel wordt geadresseerd aan "
      , email = "E-mail"
    )
  )
}

#' Package available
#'
#' Internal function to check if a specified package is installed.

#' @param x Character. Name of the package to be checked.
#' @return Logical. Is the specified package installed?
#' @keywords internal

package_available <- function(x) x %in% rownames(utils::installed.packages())

no_method <- function(x) {
  stop(
    "Objects of class '"
    , class(x)
    , "' are currently not supported (no method defined)."
    , "\nVisit https://github.com/crsh/papaja/issues to request support for this class."
    , call. = FALSE
  )
}

rename_column <- function(x, current_name, new_name) {
  colnames(x)[colnames(x) %in% current_name] <- new_name
  x
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


#' Add Equals Where Necessary
#'
#' This is an internal function that prepends every element of a character
#' vector with an 'equals' sign if the respective element does not contain one of
#' \code{c("=", "<", ">")}.
#'
#' @param x A character vector.
#'
#' @keywords internal


add_equals <-function(x) {

  validate(x, check_class = "character")

  to_add <- !grepl(x, pattern = "=|<|>") # should we add geq and leq?

  if(any(to_add)) {
    x[to_add] <- paste0("= ", x[to_add])
  }
  x
}
