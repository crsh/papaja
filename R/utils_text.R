#' Escape Symbols for LaTeX Output
#'
#' This function is a copy of the non-exported function `escape_latex()` from the \pkg{knitr} package.
#' *This function is not exported.*
#'
#' @param x Character.
#' @param newlines Logical. Determines if \code{\\n} are escaped.
#' @param spaces Logical. Determines if multiple spaces are escaped.
#'
#' @keywords internal

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


#' Sanitize Term Names
#'
#' Remove characters from term names that will be difficult to address using the \code{$}-operator.
#' *This function is not exported.*
#'
#' @param x Character. Vector of term names to be sanitized.
#' @param standardized Logical. If `TRUE`, the name of the function [[scale()]] will be
#'    removed from term names.
#' @return An object of the same class as `x` containing sanitized term names as
#'    characters.
#' @export
#' @examples
#'   sanitize_terms(c("(Intercept)", "Factor A", "Factor B", "Factor A:Factor B", "scale(FactorA)"))

sanitize_terms <- function(x, standardized = FALSE) {
  UseMethod("sanitize_terms", x)
}

#' @rdname sanitize_terms
#' @method sanitize_terms character
#' @export

sanitize_terms.character <- function(x, standardized = FALSE) {
  if(standardized) x <- gsub("scale\\(", "z_", x)   # Remove scale()
  x <- gsub("\\(|\\)|`", "", x)                     # Remove parentheses and backticks
  x <- gsub("\\.0+$", "", x)                        # Remove trailing 0-digits
  x <- gsub(",", "", x)                             # Remove big mark
  x <- gsub(" \\+ ", "_", x)                        # Remove '+' in model names
  x <- gsub("\\W", "_", x)                          # Replace non-word characters with "_"
  x
}

#' @rdname sanitize_terms
#' @method sanitize_terms factor
#' @export

sanitize_terms.factor <- function(x, standardized = FALSE) {
  factor(sanitize_terms(as.character(x, standardized)))
}

#' @rdname sanitize_terms
#' @method sanitize_terms data.frame
#' @export

sanitize_terms.data.frame <- function(x, standardized = FALSE) {
  as.data.frame(lapply(x, sanitize_terms, standardized), stringsAsFactors = FALSE)
}

#' @rdname sanitize_terms
#' @method sanitize_terms list
#' @export

sanitize_terms.list <- function(x, standardized = FALSE) {
  lapply(x, sanitize_terms, standardized)
}


#' Prettify Term Names
#'
#' Remove parentheses, replace colons with \code{$\\times$}.
#' Useful to prettify term names in [apa_print()] tables.
#'
#' @param x Character. Vector of term names to be prettified.
#' @param standardized Logical. If `TRUE`, the name of the function [scale()] will be
#'    removed from term names.
#' @param retain_period Logical. If `TRUE`, any periods in term names will be
#'    retained, otherwise they will be replaced by a space.
#' @param ... Additional arguments passed to [apa_num()], for numeric values in
#'    `x`, ignored otherwise.
#'
#' @return A character vector or `data.frame` (if `x` is a `data.frame`)
#'   containing term names modified for nicer printing.
#' @examples
#' beautify_terms("a:b")
#' beautify_terms("scale(x)", standardized = TRUE)
#' beautify_terms("snake_case")
#' @export

beautify_terms <- function(x, ...) {
  UseMethod("beautify_terms", x)
}

#' @rdname beautify_terms
#' @method beautify_terms character
#' @export

beautify_terms.character <- function(
    x
    , standardized = FALSE
    , retain_period = FALSE
    , ...
) {
  if(standardized) x <- gsub("scale\\(", "", x)       # Remove scale()
  x <- gsub("\\(|\\)|`|.+\\$", "", x)                 # Remove parentheses and backticks
  x <- gsub('.+\\$|.+\\[\\["|"\\]\\]|.+\\[.*,\\s*"|"\\s*\\]', "", x) # Remove data.frame names
  x <- gsub("\\_", " ", x)                        # Remove underscores
  if(!retain_period) x <- gsub("\\.", " ", x)

  for (i in seq_along(x)) {
    x2 <- unlist(strsplit(x[i], split = ":"))
    x2 <- capitalize(x2)
    x[i] <- paste(x2, collapse = " $\\times$ ")
  }

  x
}

#' @rdname beautify_terms
#' @method beautify_terms numeric
#' @export

beautify_terms.numeric <- function(x, standardized = FALSE, ...) {
  beautify_terms(
    apa_num(x, ...)
    , standardized = standardized
    , retain_period = TRUE
  )
}

#' @rdname beautify_terms
#' @method beautify_terms factor
#' @export

beautify_terms.factor <- function(x, standardized = FALSE, ...) {
  beautify_terms(
    as.character(x)
    , standardized = standardized
    , ...
  )
}

#' @rdname beautify_terms
#' @method beautify_terms data.frame
#' @export

beautify_terms.data.frame <- function(x, ...) {
  as.data.frame(lapply(x, beautify_terms, ...), stringsAsFactors = FALSE)
}


beautify_model <- function(x, ...) {
  terms <- strsplit(x, " \\+ ")
  beautified_terms <- lapply(terms, function(x, ...) paste(beautify_terms(x, ...), collapse = " + "), ...)
  unlist(beautified_terms)
}


#' Add Equals Where Necessary
#'
#' This is an internal function that prepends every element of a character
#' vector with an 'equals' sign if the respective element does not contain one
#' of `c("=", "<", ">")`.
#'
#' @param x A character vector.
#' @return Character vector
#' @examples
#' add_equals(c("42", "<= 42", "> 42", "= 42"))
#' @export

add_equals <- function(x) {

  validate(x, check_class = "character")

  to_add <- !grepl(x, pattern = "=|<|>") # should we add geq and leq?

  if(any(to_add)) {
    x[to_add] <- paste0("= ", x[to_add])
  }
  x
}


#' Strip Math Tags from Variable Labels or Strings
#'
#' Internal function to strip math tags from variable labels or strings. `svl()`
#' returns the stripped variable label of `x`, if available. `strip_math_tags` returns
#' the stripped character `x`.
#'
#' @param x A (labelled) character string.
#'
#' @rdname strip_math_tags
#' @keywords internal

svl <- function(x, use_math = FALSE) {
  y <- variable_labels(x)
  if(is.null(y)) y <- x

  if(!use_math) {
    return(strip_math_tags(y))
  } else {
    y
  }
}

#' @rdname strip_math_tags
#' @keywords internal

strip_math_tags <- function(x) {
  gsub(pattern = "$", replacement = "", x = x, fixed = TRUE)
}


capitalize <- function(x) {
  substring(x, first = 1, last = 1) <- toupper(substring(x, first = 1, last = 1))
  x
}


.str_extract_first <- function(x, pattern, useBytes = TRUE, ...) {
  regmatches(
    x
    , regexpr(pattern, text = x, useBytes = useBytes, ...)
  )
}

.str_extract_all <- function(x, pattern, useBytes = TRUE, ...) {
  regmatches(
    x
    , gregexpr(pattern, text = x, useBytes = useBytes, ...)
  )
}
