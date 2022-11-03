#' Typeset Numerical Values for Printing and Reporting
#'
#' Converts numerical values to character strings for printing and reporting.
#'
#' @param x Can be either a single value, vector, `matrix`, `data.frame`.
#' @param gt1 Logical. Indicates if the statistic can, in principle, have an
#'   absolute value greater than 1. If `FALSE`, leading zeros are
#'   omitted.
#' @param zero Logical. Indicates if the statistic can, in principle, be 0. If
#'  `FALSE`, a string of the form `< 0.001` is returned instead of 0.
#' @param margin Integer. If `x` is a `matrix` or `data.frame`, the function
#'   is applied either across rows (`margin = 1`) or columns (`margin = 2`).
#'   See [apply()].
#' @param na_string Character. String to print if any element of `x` is `NA`.
#' @param use_math Logical. Indicates whether to use `$` in the output so that
#'   `Inf` or scientific notation is rendered correctly.
#' @param add_equals Logical. Indicates if the output string should be
#'   prepended with an `=`.
#' @param numerals Logical. Indicates if integers should be converted to words.
#' @param capitalize Logical. Indicates if first letter should be capitalized.
#'   Ignored if `numerals = TRUE`.
#' @param zero_string Character. Word to print if `x` is a zero integer.
#' @inheritDotParams base::formatC -x
#'
#' @details
#'   If `x` is a vector, all arguments can be vectors according to which each
#'   element of the vector is formatted. Parameters are recycled if length of
#'   `x` exceeds the length of the parameter vectors. If `x` is a `matrix` or
#'   `data.frame`, the vectors specify the formatting of either rows or columns
#'   according to the value of `margin`.
#'
#'   We recommend to use `apa_num()`, rather than `printnum()` or `print_num()`,
#'   which are aliases kept only for backward compatibility.
#' @return An object of the same class as `x` with all numeric values converted
#'   to character.
#' @seealso [apa_p()], [apa_df()]
#' @examples
#' apa_num(1/3)
#' apa_num(1/3, gt1 = FALSE)
#' apa_num(1/3, digits = 5)
#'
#' apa_num(0)
#' apa_num(0, zero = FALSE)
#' @rdname apa_num
#' @export

apa_num <- function(x, ...) {
  UseMethod("apa_num", x)
}

#' @rdname apa_num
#' @export

printnum <- apa_num

#' @rdname apa_num
#' @export

print_num <- apa_num


#' @rdname apa_num
#' @export

apa_num.default <- function(x, na_string = getOption("papaja.na_string"), ...) {
  if(is.null(x)) stop("The parameter 'x' is NULL. Please provide a value for 'x'")

  x <- as.character(x)
  if(anyNA(x)) {
    x[is.na(x)] <- na_string
  }
  x
}


#' @rdname apa_num
#' @export

apa_num.list <- function(x, ...) {
  lapply(x, apa_num, ...)
}


#' @rdname apa_num
#' @export

apa_num.integer <- function(x, numerals = TRUE, capitalize = FALSE, zero_string = "no", na_string = getOption("papaja.na_string"), ...) {
  validate(x, check_integer = TRUE, check_NA = FALSE)
  validate(numerals, check_class = "logical", check_length = 1)
  validate(capitalize, check_class = "logical", check_length = 1)
  validate(na_string, check_class = "character", check_length = 1)

  # Prevent partial matching through apa_num.data.frame()
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

     collapse0 <- function(...) as.numeric(paste(..., collapse = ""))

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
        , Recall(collapse0(digits[2:1]))
      )
    } else {
      required_number_word <- ((n_digits + 2) %/% 3) - 1
      if (required_number_word > length(number_names)) {
        stop("Number is too large.")
      }
      number <- paste(
        Recall(collapse0(digits[n_digits:(3*required_number_word + 1)]))
        , number_names[required_number_word]
        , ","
        , Recall(collapse0(digits[(3*required_number_word):1])))
    }

    number
  }

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


#' @rdname apa_num
#' @export

apa_num.numeric <- function(
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

  if(any(not_zero)) {

    limit <- 1 / ten_power_digits
    abs_too_small <- !is_NA & not_zero & (abs(x) < limit)

    x[abs_too_small & !is_negative] <-  limit[abs_too_small & !is_negative] # x >= 0
    x[abs_too_small & is_negative]  <- -limit[abs_too_small & is_negative] # x < 0

    prepend[abs_too_small] <- ifelse(is_negative[abs_too_small], "> ", "< ")
  }

  x[!is_scientific] <- 0 + round(x[!is_scientific] * ten_power_digits[!is_scientific]) / ten_power_digits[!is_scientific]

  if(any(not_gt1)) {
    if(any(not_gt1 & abs(x) > 1, na.rm = TRUE)) warning("You specified gt1 = FALSE, but passed absolute value(s) that exceed 1.")

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
    y[is_scientific] <- print_scientific(y[is_scientific])
    y[is_scientific & use_math] <- paste0("$", y[is_scientific & use_math], "$")
  }

  y[is_NA] <- rep(na_string, length.out = length_x)[is_NA]
  y
}



#' @rdname apa_num
#' @export

apa_num.data.frame <- function(
  x
  , margin = 2
  , ... # cleverly recycle (column-wise) over all possible parameters
) {

  if(margin == 1) {
    ellipsis <- list(...)
    ellipsis$x <- x
    ellipsis$margin <- margin
    x_out <- do.call("apa_num.matrix", ellipsis)
  } else {
    x_out <- mapply(
      FUN = apa_num
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

#' @rdname apa_num
#' @export

apa_num.matrix <- function(
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
      do.call("apa_num", ellipsis)
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


#' @rdname apa_num
#' @export

apa_num.tiny_labelled <-function(x, ...){
  x_out <- NextMethod("apa_num")
  variable_label(x_out) <- variable_label(x)
  x_out
}


#' Prepare Numeric Values for Printing as p value
#'
#' Convenience wrapper for \code{\link{apa_num}} to print \emph{p} values.
#'
#' @param x Numeric. The \emph{p} value(s) to report.
#' @param digits Integer. The desired number of digits after the decimal point, passed on to \code{\link{formatC}}.
#' @inheritParams apa_num.numeric
#' @inherit apa_num return
#' @seealso [apa_num()], [apa_df()]
#' @examples
#' apa_p(0.05)
#' apa_p(0.0005)
#' apa_p(0.99999999)
#' apa_p(c(.001, 0), add_equals = TRUE)
#' @export

apa_p <- function(x, digits = 3L, na_string = "", add_equals = FALSE) {
  validate(x, check_class = "numeric", check_range = c(0, 1), check_NA = FALSE)
  validate(digits, check_class = "numeric")

  apa_num(
    x
    , digits = digits
    , gt1 = FALSE
    , zero = FALSE
    , na_string = na_string
    , add_equals = add_equals
  )
}

#' @rdname apa_p
#' @export

printp <- apa_p

#' @rdname apa_p
#' @export

print_p <- apa_p


#' Typeset Degrees of Freedom
#'
#' This is a function for processing degrees of freedom. It takes care
#' that trailing digits are only printed if non-integer values are given.
#'
#' @param x Numeric. The degrees of freedom to report.
#' @param digits Integer. The desired number of digits after the decimal point to
#'   be used if `x` contains non-integer values.
#' @param elementwise Logical. Determines whether the number of trailing digits
#'   should be determined for each element of `x` separately (the default),
#'   or for the complete vector `x`.
#' @inherit apa_num return
#' @seealso [apa_num()], [apa_p()]
#' @examples
#' apa_df(c(1, 1.23151))
#' @export

apa_df <- function(x, digits = 2L, elementwise = TRUE) {

  if(is.null(x))    return(NULL)
  if(is.integer(x)) return(apa_num(x))

  elementwise <- isTRUE(elementwise)
  digits <- as.integer(digits)

  validate(digits, check_class = "numeric", check_NA = TRUE)

  if(length(digits) != 1L && length(x) != length(digits)) {
    stop("The parameter `digits` must be of length 1 or equal to length of `x`.")
  }

  if(elementwise) {
    digits <- as.integer(round(x, digits = 0L) != round(x, digits = digits + 2L)) * digits
  } else if( all(round(x, digits = 0L) == round(x, digits = digits + 2L)) ) {
    digits <- 0L
  }

  apa_num(x, digits = digits)
}

#' @rdname apa_df
#' @export

print_df <- apa_df


#' Typeset scientific notation
#'
#' Typesets scientific notation of numbers into properly typeset math strings.
#'
#' @param x Character.
#' @keywords internal
#'
# #' @examples
# #' papaja:::print_scientific("1.25e+04")

print_scientific <- function(x) {
  x <- gsub("e\\+00$", "", x)
  x <- gsub("e\\+0?(\\d+)$", " \\\\times 10\\^\\{\\1\\}", x)
  x <- gsub("e\\-0?(\\d+)$", " \\\\times 10\\^\\{-\\1\\}", x)
  x
}

# print_bf <- function(
#   x
#   , ratio_subscript = "10"
#   , auto_invert = FALSE
#   , escape = TRUE
#   , scientific = TRUE
#   , nonscientific_range = c(min = 1/1000, max = 1000)
#   , log = FALSE
#   , use_math = FALSE
#   , ...
# ) {
#   validate(x, check_class = "numeric", check_NA = TRUE, check_infinite = FALSE)
#   validate(ratio_subscript, check_class = "character", check_length = 1)
#   validate(auto_invert, check_class = "logical", check_length = 1)
#   validate(scientific, check_class = "logical", check_length = 1)
#   validate(nonscientific_range, check_class = "numeric", check_length = 2)
#   validate(log, check_class = "logical", check_length = 1)

#   ellipsis <- list(...)
#   ellipsis$x <- x
#   ellipsis$use_math <- use_math

#   if(auto_invert) {
#     to_invert <- ellipsis$x < 1
#     ellipsis$x[to_invert] <- 1 / ellipsis$x[to_invert]

#     ratio_subscript <- rep(ratio_subscript, length(x))
#     ratio_subscript[to_invert] <- invert_subscript(ratio_subscript)
#   }

#   if(escape) {
#     ratio_subscript <- paste0("\\textrm{", escape_latex(ratio_subscript), "}")
#   }

#   if(scientific & (ellipsis$x > nonscientific_range["max"] - 1 | ellipsis$x < nonscientific_range["min"])) {
#     ellipsis$format <- "e"
#     if(is.null(ellipsis$digits)) ellipsis$digits <- 2

#     bf <- do.call("apa_num", ellipsis)
#     bf <- print_scientific(bf)
#   } else {
#     if(is.null(ellipsis$zero)) ellipsis$zero <- FALSE
#     bf <- do.call("apa_num", ellipsis)
#   }

#   bf_name <- if(!log) "BF" else "\\log BF"

#   bf <- paste0("$\\mathrm{", bf_name, "}_{", ratio_subscript, "} ", add_equals(bf), "$")
#   bf
# }

# invert_subscript <- function(x) {
#   seperator <- if(nchar(x) == 2) "" else "/"
#   paste0(rev(unlist(strsplit(x, seperator))), collapse = seperator)
# }
