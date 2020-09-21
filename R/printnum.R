#' Prepare numeric values for printing
#'
#' Converts numeric values to character strings for reporting.
#'
#' @param x Can be either a single value, vector, matrix, or \code{data.frame}.
#' @param gt1 Logical. Indicates if the absolute value of the statistic can, in principal, greater than 1.
#' @param zero Logical. Indicates if the statistic can, in principal, be 0.
#' @param margin Integer. If \code{x} is a \code{matrix} or \code{data.frame}, the function
#'   is applied either across rows (\code{margin = 1}) or columns (\code{margin = 2}).
#' @param na_string Character. String to print if element of \code{x} is \code{NA}.
#' @param use_math Logical. Indicates whether to insert \code{$} into the output so that \code{Inf} or scientific
#'    notation is rendered correctly.
#' @param add_equals Logical. Indicates if the output string should be prepended with an \emph{equals} sign.
#' @param numerals Logical. Indicates if integers should be returned as words.
#' @param capitalize Logical. Indicates if first letter should be capitalized. Ignored if \code{numerals = TURE}.
#' @param zero_string Character. Word to print if \code{x} is a zero integer.
#' @inheritDotParams base::formatC -x
#'
#' @details If \code{x} is a vector, all arguments can be vectors
#'    according to which each element of the vector is formatted.
#'    Parameters are recycled if length of \code{x} exceeds the length of the parameter vectors.
#'    If \code{x} is a \code{matrix} or \code{data.frame}, the vectors specify the formatting
#'    of either rows or columns according to the value of \code{margin}.
#' @examples
#' printnum(1/3)
#' printnum(1/3, gt1 = FALSE)
#' printnum(1/3, digits = 5)
#'
#' printnum(0)
#' printnum(0, zero = FALSE)
#'
#' printp(0.0001)
#' @rdname printnum
#' @export

printnum <- function(x, ...) {
  UseMethod("printnum", x)
}


#' @rdname printnum
#' @export

printnum.default <- function(x, na_string = getOption("papaja.na_string"), ...) {
  if(is.null(x)) stop("The parameter 'x' is NULL. Please provide a value for 'x'")

  x <- as.character(x)
  if(anyNA(x)) {
    x[is.na(x)] <- na_string
  }
  x
}


#' @rdname printnum
#' @export

printnum.list <- function(x, ...) {
  lapply(x, printnum, ...)
}


#' @rdname printnum
#' @export

printnum.integer <- function(x, numerals = TRUE, capitalize = FALSE, zero_string = "no", na_string = getOption("papaja.na_string"), ...) {
  validate(x, check_integer = TRUE, check_NA = FALSE)
  validate(numerals, check_class = "logical", check_length = 1)
  validate(capitalize, check_class = "logical", check_length = 1)
  validate(na_string, check_class = "character", check_length = 1)

  # Prevent partial matching through printnum.data.frame()
  system_call <- sys.call()
  if(!is.null(system_call[["zero"]]) && is.null(system_call[["zero_string"]])) zero_string <- "no"
  validate(zero_string, check_class = "character", check_length = 1)

  if(numerals) return(as.character(x))
  if(anyNA(x)) return(rep(na_string, length(x)))

  zero_string <- tolower(zero_string)

  # Based on a function that John Fox posted on the R mailing list
  # http://tolstoy.newcastle.edu.au/R/help/05/04/2715.html

  number_to_words <- function(x) {
    if(x == 0) return(zero_string)

    single_digits <- c("", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine")
    names(single_digits) <- 0:9
    teens <- c("ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", " seventeen", "eighteen", "nineteen")
    names(teens) <- 0:9
    tens <- c("twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety")
    names(tens) <- 2:9
    number_names <- c("thousand", "million", "billion", "trillion", "quadrillion", "quintillion", "sextillion", "septillion", "octillion", "nonillion", "decillion")

    digits <- rev(strsplit(as.character(x), "")[[1]])
    n_digits <- length(digits)
    if(n_digits == 1) {
      number <- as.vector(single_digits[digits])
    } else if (n_digits == 2) {
      if (x <= 19) {
        number <- as.vector(teens[digits[1]])
      } else {
        number <- paste(
          tens[digits[2]]
          , Recall(as.numeric(digits[1]))
          , sep = "-"
        )
      }
    } else if(n_digits == 3) {
      number <- paste(
        single_digits[digits[3]]
        , "hundred and"
        , Recall(collapse(digits[2:1]))
      )
    } else {
      required_number_word <- ((n_digits + 2) %/% 3) - 1
      if (required_number_word > length(number_names)) {
        stop("Number is too large.")
      }
      number <- paste(
        Recall(collapse(digits[n_digits:(3*required_number_word + 1)]))
        , number_names[required_number_word]
        , ","
        , Recall(collapse(digits[(3*required_number_word):1])))
    }

    number
  }

  collapse <- function(...) as.numeric(paste(..., collapse = ""))

  clean_number <- function(x) {
    x <- gsub("^\ +|\ +$", "", x)
    x <- gsub("\ +,", ",", x)
    x <- gsub(paste0("( and ", zero_string, "|-", zero_string, "|, ", zero_string, ")"), "", x)
    x <- gsub("(\ *,|-|\ +and)$", "", x)
    if(!grepl(" and ", x)) x <- gsub(", ", " and ", x)
    x
  }

  if(length(x) > 1) {
    return(
      vapply(
        x
        , function(y) {
          y_number <- clean_number(number_to_words(y))
          if(capitalize) x_number <- capitalize(y_number)
          y_number
        }
        , FUN.VALUE = "a"
      )
    )
  }

  x_number <- clean_number(number_to_words(x))
  if(capitalize) x_number <- capitalize(x_number)
  x_number
}


#' @rdname printnum
#' @export

printnum.numeric <- function(
  x
  , gt1 = TRUE
  , zero = TRUE
  , na_string = getOption("papaja.na_string")
  , use_math = TRUE
  , add_equals = FALSE
  , ...
) {

  if(is.null(x)) stop("The parameter 'x' is NULL. Please provide a value for 'x'")
  validate(gt1, check_class = "logical")
  validate(zero, check_class = "logical")
  validate(na_string, check_class = "character")
  validate(use_math, check_class = "logical")
  validate(add_equals, check_class = "logical")

  ellipsis <- defaults(
    list(...)
    , set.if.null = list(
      digits = 2L
      , format = "f"
      , big.mark = ","
    )
  )
  ellipsis$margin <- NULL # for backwards compatibility

  digits <- ellipsis$digits
  validate(digits, check_infinite = TRUE, check_class = "numeric", check_integer = TRUE)

  length_x <- length(x)

  digits <- rep(as.integer(ellipsis$digits), length.out = length_x)
  not_gt1 <- rep(!gt1, length.out = length_x)
  not_zero <- rep(!zero, length.out = length_x)
  is_NA <- is.na(x)

  add_equals <- rep(as.integer(add_equals) + 1L, length.out = length_x)
  prepend <- c("", "= ")[add_equals]

  # unrounded for correct sign if zero = FALSE
  is_negative <- (x < 0)

  # round with vector-valued digits argument:
  is_scientific <- rep(tolower(ellipsis$format), length.out = length_x) %in% c("e", "g", "fg")

  ten_power_digits <- 10^digits
  x[!is_scientific] <- 0 + round(x[!is_scientific] * ten_power_digits[!is_scientific]) / ten_power_digits[!is_scientific]



  if(any(not_zero)) {

    limit <- 1 / ten_power_digits
    abs_too_small <- !is_NA & not_zero & (abs(x) < limit)

    x[abs_too_small & !is_negative] <-  limit[abs_too_small & !is_negative] # x >= 0
    x[abs_too_small & is_negative]  <- -limit[abs_too_small & is_negative] # x < 0

    prepend[abs_too_small] <- ifelse(is_negative[abs_too_small], "> ", "< ")
  }

  if(any(not_gt1)) {
    if(any(not_gt1 & abs(x) > 1)) warning("You specified gt1 = FALSE, but passed absolute value(s) that exceed 1.")

    limit <- 1 - 1/ten_power_digits

    too_large <- !is_NA & not_gt1 & (x == 1)
    x[too_large] <- limit[too_large]
    prepend[too_large] <- "> "

    too_small <- !is_NA & not_gt1 & (x == -1)
    x[too_small] <- - limit[too_small]
    prepend[too_small] <- "< "
  }



  lengths <- sapply(ellipsis, length)

  if(all(lengths == 1)) {
    ellipsis$x <- x
    y <- do.call("formatC", ellipsis)
  } else {
    ellipsis <- lapply(
      X = ellipsis
      , FUN = rep
      , length.out = length_x
    )
    ellipsis$x <- x
    ellipsis$FUN = formatC
    y <- do.call("mapply", ellipsis)
  }


  y[not_gt1] <- sub(y[not_gt1], pattern = "0.", replacement = ".", fixed = TRUE)
  y <- paste0(prepend, y)

  # Handle infinity
  use_math <- rep(use_math, length.out = length_x)

  is_infinite <- is.infinite(x)
  if(any(is_infinite)) {
    y[is_infinite] <- sub(y[is_infinite], pattern = "Inf| Inf", replacement = "\\\\infty")
    y[is_infinite & use_math] <- paste0("$", y[is_infinite & use_math], "$")
  }

  # Typeset scientific
  if(any(is_scientific)) {
    y[is_scientific] <- typeset_scientific(y[is_scientific])
    y[is_scientific & use_math] <- paste0("$", y[is_scientific & use_math], "$")
  }

  y[is_NA] <- rep(na_string, length.out = length_x)[is_NA]
  y
}



#' @rdname printnum
#' @export

printnum.data.frame <- function(
  x
  , margin = 2
  , ... # cleverly recycle (column-wise) over all possible parameters
) {

  if(margin == 1) {
    ellipsis <- list(...)
    ellipsis$x <- x
    ellipsis$margin <- margin
    x_out <- do.call("printnum.matrix", ellipsis)
  } else {
    x_out <- mapply(
      FUN = printnum
      , x = x
      , ...
      , SIMPLIFY = FALSE
    )
  }

  x_out <- as.data.frame(
    x_out
    , stringsAsFactors = FALSE
    , check.names = FALSE
    , fix.empty.names = FALSE
  )

  rownames(x_out) <- rownames(x)
  x_out
}

#' @rdname printnum
#' @export

printnum.matrix <- function(
  x
  , margin = 2
  , ...
) {

  ellipsis <- list(...)

  x_out <- apply(
    X = x
    , MARGIN = (3 - margin) # Parameters are applied according to margin
    , FUN = function(x) {
      ellipsis$x <- x
      do.call("printnum", ellipsis)
    }
    # Inception!
  )

  if(margin == 2 || nrow(x) == 1) {
    x_out <- t(x_out) # Reverse transposition caused by apply
    dimnames(x_out) <- dimnames(x)
  }

  if(!is.matrix(x_out)) x_out <- matrix(x_out, ncol = ncol(x))

  x_out
}


#' @rdname printnum
#' @export

printnum.papaja_labelled <-function(x, ...){
  x_out <- NextMethod("printnum")
  variable_label(x_out) <- variable_label(x)
  x_out
}


#' Prepare Numeric Values for Printing as p value
#'
#' Convenience wrapper for \code{\link{printnum}} to print \emph{p} values.
#'
#' @param x Numeric. The \emph{p} value(s) to report.
#' @param digits Integer. The desired number of digits after the decimal point, passed on to \code{\link{formatC}}.
#' @inheritParams printnum.numeric
#' @examples
#' printp(0.05)
#' printp(0.0005)
#' printp(0.99999999)
#' printp(c(.001, 0), add_equals = TRUE)
#' @export

printp <- function(x, digits = 3L, na_string = "", add_equals = FALSE) {
  validate(x, check_class = "numeric", check_range = c(0, 1), check_NA = FALSE)
  validate(digits, check_class = "numeric")

  printnum(
    x
    , digits = digits
    , gt1 = FALSE
    , zero = FALSE
    , na_string = na_string
    , add_equals = add_equals
  )
}


#' Print Degrees of Freedom
#'
#' This is an internal function for processing degrees of freedom. It takes care
#' of printing trailing digits only if non-integer values are given.
#'
#' @keywords internal

print_df <- function(x, digits = 2L) {

  if(is.null(x))    return(NULL)
  if(is.integer(x)) return(printnum(x))

  validate(digits, check_class = "numeric", check_NA = TRUE)

  if(length(digits) != 1L && length(x) != length(digits)) {
    stop("The parameter `digits` must be of length 1 or equal to length of `x`.")
  }

  x_digits <- ifelse(
    is.finite(x)
    , as.integer(x %% 1 > 0) * digits
    , 0L
  )

  return(printnum(x, digits = x_digits))
}


#' Typset scientific notation
#'
#' Typsets scientific notation of numbers into properly typeset math strings.
#'
#' @param x Character.
#'
#' @return
#'
#' @examples
#' papaja:::typeset_scientific("1.25e+04")

typeset_scientific <- function(x) {
  x <- gsub("e\\+00$", "", x)
  x <- gsub("e\\+0?(\\d+)$", " \\\\times 10\\^\\{\\1\\}", x)
  x <- gsub("e\\-0?(\\d+)$", " \\\\times 10\\^\\{-\\1\\}", x)
  x
}
