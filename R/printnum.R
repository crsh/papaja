#' Prepare numeric values for printing
#'
#' Converts numeric values to character strings for reporting.
#'
#' @param x Numeric. Can be either a single value, vector, or matrix.
#' @param gt1 Logical. Indicates if the absolute value of the statistic can, in principal, greater than 1.
#' @param zero Logical. Indicates if the statistic can, in principal, be 0.
#' @param margin Integer. If \code{x} is a matrix, the function is applied either across rows (\code{margin = 1})
#'    or columns (\code{margin = 2}).
#' @param na_string Character. String to print if element of \code{x} is \code{NA}.
#' @param use_math Logical. Indicates whether to insert \code{$} into the output so that \code{Inf} or scientific
#'    notation is rendered correctly.
#' @param add_equals Logical. Indicates if the output string should be prepended with an \emph{equals} sign.
#' @param numerals Logical. Indicates if integer should be returned as words.
#' @param capitalize Logical. Indicates if first letter should be capitalized. Ignored if \code{numberals = TURE}.
#' @param zero_string Character. Word to print if \code{x} is a zero integer.
#' @inheritDotParams base::formatC -x
#'
#' @details If \code{x} is a vector, \code{digits}, \code{gt1}, and \code{zero} can be vectors
#'    according to which each element of the vector is formated. Parameters are recycled if length of \code{x}
#'    exceeds length of the parameter vectors. If \code{x} is a matrix, the vectors specify the formating
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
  , margin = 1
  , na_string = getOption("papaja.na_string")
  , use_math = TRUE
  , add_equals = FALSE
  , ...
) {
  if(is.null(x)) stop("The parameter 'x' is NULL. Please provide a value for 'x'")

  ellipsis <- list(...)

  validate(gt1, check_class = "logical")
  validate(zero, check_class = "logical")
  validate(margin, check_class = "numeric", check_integer = TRUE, check_length = 1, check_range = c(1, 2))
  validate(na_string, check_class = "character", check_length = 1)
  validate(use_math, check_class = "logical")
  validate(add_equals, check_class = "logical")

  ellipsis <- defaults(
    ellipsis
    , set = list(
      x = x
      , gt1 = gt1
      , zero = zero
      , na_string = na_string
      , use_math = use_math
    )
    , set.if.null = list(
      digits = 2
      , big.mark = ","
    )
  )

  # if(!is.null(ellipsis$digits)) { # if not necessary because default is set beforehand
    validate(ellipsis$digits, "digits", check_class = "numeric", check_integer = TRUE, check_range = c(0, Inf))
  #}

  if(length(x) > 1) {
    # print_args <- list(digits = digits, gt1 = gt1, zero = zero)
    vprintnumber <- function(i, x){
      ellipsis.i <- lapply(X = ellipsis, FUN = sel, i)
      do.call("printnumber", ellipsis.i)
    }
  }

    # not necessary because of.data.frame-method:
    # if(is.data.frame(x)) x_out <- as.data.frame(x_out)

  if(is.numeric(x) & length(x) > 1) {
    # print_args <- lapply(print_args, rep, length = length(x)) # Recycle arguments
    x_out <- sapply(seq_along(x), vprintnumber, x)
    names(x_out) <- names(x)
  } else {
    x_out <- do.call("printnumber", ellipsis)
  }

  if(add_equals) {
    x_out <- add_equals(x_out)
  }
  x_out
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


printnumber <- function(x, gt1 = TRUE, zero = TRUE, na_string = "", use_math = TRUE, ...) {

  ellipsis <- list(...)
  validate(x, check_class = "numeric", check_NA = FALSE, check_length = 1, check_infinite = FALSE)
  if(is.na(x)) return(na_string)
  if(is.infinite(x)) {
    x_out <- paste0(ifelse(x < 0, "-", ""), "\\infty")
    if(use_math) x_out <- paste0("$", x_out, "$")
    return(x_out)
  }
  if(!is.null(ellipsis$digits)) {
    validate(ellipsis$digits, "digits", check_class = "numeric", check_integer = TRUE, check_length = 1, check_range = c(0, Inf))
  }

  validate(gt1, check_class = "logical", check_length = 1)
  validate(zero, check_class = "logical", check_length = 1)
  validate(na_string, check_class = "character", check_length = 1)
  if(!gt1 & abs(x) > 1) warning("You specified gt1 = FALSE, but passed absolute value(s) that exceed 1.")

  ellipsis <- defaults(
    ellipsis
    , set.if.null = list(
      digits = 2
      , format = "f"
      , flag = "0"
      , big.mark = ","
    )
  )

  x_out <- round(x, ellipsis$digits) + 0 # No sign if x_out == 0

  if(sign(x_out) == -1) {
    xsign <- "-"
    lt <- "> "
    gt <- "< "
  } else {
    xsign <- ""
    lt <- "< "
    gt <- "> "
  }

  if(x_out == 0 & !zero) x_out <- paste0(lt, "0.", paste0(rep(0, ellipsis$digits-1), collapse = ""), "1") # Too small to report

  if(!gt1) {
    if(x_out == 1) {
      x_out <- paste0(gt, xsign, ".", paste0(rep(9, ellipsis$digits), collapse = "")) # Never report 1
    } else if(x_out == -1) {
      x_out <- paste0(lt, xsign, ".", paste0(rep(9, ellipsis$digits), collapse = "")) # Never report 1
    }
    ellipsis$x <- x_out
    x_out <- do.call("formatC", ellipsis)
    x_out <- gsub("0\\.", "\\.", x_out)
  } else {
    ellipsis$x <- x_out
    x_out <- do.call("formatC", ellipsis)
  }
  x_out
}



#' Prepare numeric values for printing as p value
#'
#' Convenience wrapper for \code{printnum.numeric} to print \emph{p} values.
#'
#' @param digits Integer. The desired number of digits after the decimal point, passed on to \code{\link{formatC}}.
#' @inheritParams printnum
#' @examples
#' printp(0.05)
#' printp(0.0005)
#' printp(0.99999999)
#' @export

printp <- function(x, digits = 3L, na_string = "", add_equals = FALSE) {
  validate(x, check_class = "numeric", check_range = c(0, 1), check_NA = FALSE)
  validate(na_string, check_class = "character", check_length = 1)
  validate(digits, check_class = "numeric")

  p <- printnum(x, digits = digits, gt1 = FALSE, zero = FALSE, na_string = na_string, add_equals = add_equals)
  p
}


#' @keywords internal

print_df <- function(x, digits = 2L) {

  if(is.null(x))    return(NULL)
  if(is.integer(x)) return(printnum(x))

  return(printnum(x, digits = as.numeric(x %% 1 != 0) * digits))
}

