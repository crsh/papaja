#' Create interval strings
#'
#' Creates a character string to report an interval.
#' \emph{This function is not exported.}
#'
#' @param x Numeric. A vector (of length 2, unless `y` is also specified) with,
#'    a two-column `matrix`, or a `data.frame`, which can coerced to a
#'    `matrix`.
#' @param y Numeric. A vector of the same length as `x`.
#' @param conf_level Numeric. Confidence level of the interval. Ignored if level can be infered from attributes of `x`, see Details.
#' @param interval_type Character. Abbreviation indicating the type of interval
#'    estimate, e.g. `CI`.
#' @param use_math Logical. Indicates whether to insert `$` into the 
#'    output so that `Inf` or scientific will be rendered correctly.
#' @inheritDotParams printnum
#' 
#' @details If possible the confidence level of the interval is inferred from
#'    attributes of `x`. For a vector of length 2, the attribute `conf.level` is
#'    is consulted; for a `matrix` or `data.frame` the column names are used, 
#'    if they are of the format "2.5 \%" and "97.5 \%".
#' 
#'    If `x` is a `matrix` or `data.frame` the row names are used as names for
#'    the returned `list` of intervals.
#' 
#' @return A singel interval is returned as a `character` vector of length 1;
#'    multiple intervals are returned as a named `list` of `character` vectors
#'    of length 1.
#'
#' @keywords internal
#' @seealso \code{\link{printnum}}
#' @export
#' @examples
#' \dontrun{
#' print_confint(1, 2, conf_level = 0.95)
#' print_confint(c(1, 2), conf_level = 0.95)
#' print_confint(matrix(c(1, 2), ncol = 2), conf_level = 0.95)
#' print_confint(confint(lm(cars)))
#' print_confint(confint(lm(cars)), digits = 3)
#' }

print_interval <- function(
    x
    , ...
    , conf_level = NULL
    , interval_type = NULL
    , use_math = FALSE
) {
    sapply(x, validate, name = "x", check_class = "numeric", check_infinite = FALSE)
    validate(use_math, check_class = "logical", check_length = 1)
    if(!is.null(interval_type)) validate(interval_type, check_class = "character", check_length = 1)

    if(!is.null(conf_level)) validate(conf_level, check_class = "numeric", check_length = 1, check_range = c(0, 100))

    UseMethod("print_interval", x)
}

#' @method print_interval default

print_interval.default <- function(x, ...) {
    stop("No method for objects of class ", class(x))
}

#' @rdname print_interval
#' @method print_interval data.frame

print_interval.data.frame <- function(x, ...) {
    x <- as.matrix(x)
    print_interval(x, ...)
}

#' @rdname print_interval
#' @method print_interval numeric

print_interval.numeric <- function(
    x
    , y = NULL
    , conf_level = NULL
    , interval_type = NULL
    , use_math = FALSE
    , ...
) {
    # Manualy construct matrix
    if(!is.null(y)) {
        sapply(y, validate, name = "y", check_class = "numeric", check_infinite = FALSE)
        if(!is.vector(x) && !is.vector(y)) stop("When passing 'x' and 'y' both must be vectors.")
        if(!length(x) == length(y)) stop("When passing 'x' and 'y' both must be of the same length.")

        if(length(x) > 1) {
            x <- matrix(c(x, y), ncol = 2)
            res <- print_interval(x, conf_level = conf_level, use_math = use_math, interval_type = interval_type, ...)
            return(res)
        } else {
            x <- c(x, y)
        }
    }

    validate(x, check_length = 2)

    if(!is.null(attr(x, "conf.level"))) conf_level <- attr(x, "conf.level")

    if(is.null(interval_type)) conf_level <- NULL

    if(!is.null(conf_level)) {
        if(conf_level < 1) conf_level <- conf_level * 100
        conf_level <- paste0(conf_level, "\\% ", interval_type, " ")
    }

    x <- printnum(x, use_math = use_math, ...)
    
    interval <- paste0(
        conf_level
        , "$["
        , paste(x, collapse = "$, $")
        , "]$"
    )

    interval
}

#' @rdname print_interval
#' @method print_interval matrix

print_interval.matrix <- function(
    x
    , conf_level = NULL
    , interval_type = NULL
    , use_math = FALSE
    , ...
) {
    if(!is.null(colnames(x))) {
        col_conf_level <- as.numeric(gsub("[^.|\\d]", "", colnames(x), perl = TRUE))
        col_conf_level <- if(anyNA(col_conf_level)) NULL else col_conf_level
        if(!is.null(col_conf_level)) conf_level <- 100 - col_conf_level[1] * 2
    }

    # if(nrow(x) == 1) {
    #     x <- as.vector(x)
    #     res <- print_interval(x, conf_level = conf_level, use_math = use_math, interval_type = interval_type, ...)
    #     retrun(res)
    # }

    if(is.null(interval_type)) conf_level <- NULL

    if(!is.null(conf_level)) {
        if(conf_level < 1) conf_level <- conf_level * 100
        conf_level <- paste0(conf_level, "\\% ", interval_type, " ")
    }

    x <- printnum(x, use_math = use_math, ...)

    if(!is.null(rownames(x))) {
        terms <- sanitize_terms(rownames(x))
    } else {
        terms <- 1:nrow(x)
    }

    interval <- list()
    for(i in 1:length(terms)) {
        interval[[terms[i]]] <- paste0(
            conf_level
            , "$["
            , paste(x[i, ], collapse = "$, $")
            , "]$"
        )
    }

    if(length(interval) == 1) interval <- unlist(interval)

    interval
}

#' @rdname print_interval

print_confint <- function(
  x
  , ...
  , interval_type = "CI"
) {
  print_interval(x, interval_type = interval_type, ...)
}

#' @rdname print_interval

print_hdint <- function(
  x
  , ...
  , interval_type = "HDI"
) {
  print_interval(x, interval_type = interval_type, ...)
}
