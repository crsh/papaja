#' Prepare numeric values for printing
#'
#' Formats numeric values according to APA guidelines (6th edition) for reporting.
#' @param x Numeric. Can be either a single value, vector, or matrix.
#' @param digits Integer. Number of decimal places to print.
#' @param gt1 Logical. Indicates if the absolute value of the statistic can, in principal, exceed 1.
#' @param zero Logical. Indicates if the statistic can, in principal, be 0.
#' @param margin Integer. If \code{x} is a matrix, the function is applied either across rows (= 1)
#'    or columns (= 2).
#' @na_string Character. String to print if \code{x} is \code{NA}.
#' @seealso \code{\link{apply}}
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

printnum <- function(x, digits = 2, gt1 = TRUE, zero = TRUE, margin = 1, na_string = "") {
  if(is.null(x)) stop("The parameter 'x' is NULL. Please provide a value for 'x'")

  validate.logical(gt1, "gt1")
  validate.logical(zero, "zero")

  validate.numeric(digits, "digits", type="integer")
  validate.numeric.range(digits, "digits", lower=0, upper=Inf)

  validate.character(na_string, "na_string")

  validate.numeric(margin, "margin", type="integer")
  validate.numeric.range(margin, "margin", lower=1, upper=2)


  if(length(x) > 1) {
    print_args <- list(digits, gt1, zero)
    vprintnumber <- function(i, x)
      printnumber(
        x[i]
        , digits = print_args[[1]][i]
        , gt1 = print_args[[2]][i]
        , zero = print_args[[3]][i]
        , na_string = na_string
      )
  }

  if(is.matrix(x) | is.data.frame(x)) {
    x_out <- apply(
      x
      , (3 - margin) # Parameters are applied according to margin
      , printnum # Inception!
      , digits = print_args[[1]]
      , gt1 = print_args[[2]]
      , zero = print_args[[3]]
      , na_string = na_string
    )

    if(margin == 2) x_out <- t(x_out) # Reverse transposition caused by apply
    dimnames(x_out) <- dimnames(x)
    if(is.data.frame(x)) x_out <- as.data.frame(x_out)

  } else if(is.numeric(x) & length(x) > 1) {
    print_args <- lapply(print_args, rep, length = length(x)) # Recycle arguments
    x_out <- sapply(seq_along(x), vprintnumber, x)
    names(x_out) <- names(x)
  } else {
    x_out <- printnumber(x, digits, gt1, zero, na_string = na_string)
  }
  x_out
}


printnumber <- function(x, digits, gt1, zero, na_string) {
  if(is.na(x)) return(na_string)
  if(!gt1 & abs(x) > 1) stop("You specified gt1 = FALSE, but passed absolute value(s) that exceed 1.")

  x_out <- round(x, digits) + 0 # No sign if x_out == 0

  if(sign(x_out) == -1) {
    xsign <- "-"
    lt <- "> "
    gt <- "< "
  } else {
    xsign <- ""
    lt <- "< "
    gt <- "> "
  }

  if(x_out == 0 & !zero) x_out <- paste0(lt, xsign, ".", paste0(rep(0, digits-1), collapse = ""), "1") # Too small to report

  if(!gt1) {
    if(x_out == 1) {
      x_out <- paste0(gt, xsign, ".", paste0(rep(9, digits), collapse = "")) # Never report 1
    } else if(x_out == -1) {
      x_out <- paste0(lt, xsign, ".", paste0(rep(9, digits), collapse = "")) # Never report 1
    }

    x_out <- formatC(x_out, format = "f", digits = digits, flag = "0") # Fill to desired number of digits
    x_out <- gsub("0\\.", "\\.", x_out)
  } else {
    x_out <- formatC(x_out, format = "f", digits = digits, flag = "0") # Fill to desired number of digits
  }
  x_out
}


#' @describeIn printnum Convenience wrapper for \code{printnum} to print p-values with three decimal places.
#' @export

printp <- function(x, na_string = "") {
  if(any(sign(x) == -1)) stop("P-values cannot be negative.")
  if(any(sign(x) > 1)) stop("P-values cannot be greater than 1.")

  p <- printnum(x, digits = 3, gt1 = FALSE, zero = FALSE, na_string = na_string)
  p
}
