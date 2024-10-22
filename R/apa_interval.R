#' Typeset Interval Estimate
#'
#' Creates a character string to report interval estimates, such as frequentist
#' confidence or Bayesian credible intervals.
#'
#' @param x Numeric. A vector (of length 2, unless `y` is also specified) with,
#'   a two-column `matrix`, or a `data.frame`, which can coerced to a `matrix`.
#' @param y Numeric. An optional vector of the same length as `x`.
#' @param conf.int Numeric. Confidence level of the interval. Ignored if
#'   level can be inferred from attributes of `x`, see Details.
#' @param interval_type Character. Abbreviation indicating the type of interval
#'   estimate, e.g. `CI`.
#' @param enclose_math Logical. Indicates whether the interval should be
#'   enclosed in `$` (i.e., a math environment).
#' @param ... Further arguments passed on to [apa_num()].
#'
#' @details If possible the confidence level of the interval is inferred from
#'   attributes of `x`. For a vector of length 2, the attribute `conf.level` is
#'   is consulted; for a `matrix` or `data.frame` the column names are used,
#'   if they are of the format "2.5 \%" and "97.5 \%".
#'
#'   If `x` is a `matrix` or `data.frame` the row names are used as names for
#'   the returned `list` of intervals.
#'
#' @return A single interval is returned as a `character` vector of length 1;
#'   multiple intervals are returned as a named `list` of `character` vectors
#'   of length 1.
#'
#' @seealso [apa_num()]
#' @export
#' @examples
#' apa_confint(1, 2, conf.int = 0.95)
#' apa_confint(c(1, 2), conf.int = 0.95)
#' apa_confint(matrix(c(1, 2), ncol = 2), conf.int = 0.95)
#' apa_confint(confint(lm(cars)))
#' apa_confint(confint(lm(cars)), digits = 3)

apa_interval <- function(
    x
    , conf.int = NULL
    , interval_type = NULL
    , enclose_math = FALSE
    , ...
) {
    sapply(x, validate, name = "x", check_class = "numeric", check_infinite = FALSE)
    validate(enclose_math, check_class = "logical", check_length = 1)
    if(!is.null(interval_type)) validate(interval_type, check_class = "character", check_length = 1)

    if(!is.null(conf.int)) validate(conf.int, check_class = "numeric", check_length = 1, check_range = c(0, 100))

    UseMethod("apa_interval", x)
}

#' @rdname apa_interval
#' @export

print_interval <- apa_interval

#' @method apa_interval default
#' @export

apa_interval.default <- function(x, ...) {
    stop("No method for objects of class ", class(x))
}

#' @rdname apa_interval
#' @method apa_interval numeric
#' @export

apa_interval.numeric <- function(
    x
    , y = NULL
    , conf.int = NULL
    , interval_type = NULL
    , enclose_math = FALSE
    , ...
) {
    ellipsis_ci <- deprecate_ci(conf.int = conf.int, ...)
    conf.int <- ellipsis_ci$conf.int
    ellipsis <- ellipsis_ci$ellipsis

    # Manually construct matrix
    if(!is.null(y)) {
        sapply(y, validate, name = "y", check_class = "numeric", check_infinite = FALSE)
        if(!is.vector(x) && !is.vector(y)) stop("When passing 'x' and 'y' both must be vectors.")
        if(!length(x) == length(y)) stop("When passing 'x' and 'y' both must be of the same length.")

        if(length(x) > 1) {
            x <- matrix(c(x, y), ncol = 2)
            res <- apa_interval(x, conf.int = conf.int, enclose_math = enclose_math, interval_type = interval_type, ...)
            return(res)
        } else {
            x <- c(x, y)
        }
    }

    validate(x, check_length = 2, check_infinite = FALSE)

    if(is.null(conf.int)) {
      if(!is.null(attr(x, "conf.level"))) {
        conf.int <- attr(x, "conf.level")
      } else if(!is.null(names(x))) {
        suppressWarnings(
          conf.int <- as.numeric(
            gsub("[^.|\\d]", "", names(x), perl = TRUE)
          )
        )
        conf.int <- if(anyNA(conf.int)) NULL else conf.int
        if(!is.null(conf.int)) conf.int <- 100 - conf.int[1] * 2
      }
    }

    if(is.null(interval_type)) conf.int <- NULL

    if(!is.null(conf.int)) {
        if(conf.int < 1) conf.int <- conf.int * 100
        conf.int <- paste0(conf.int, "\\% ", interval_type, " ")
    }

    ellipsis$x <- x
    x <- do.call("apa_num", ellipsis)

    if(enclose_math) {
      interval <- paste0(
          conf.int
          , "$["
          , paste(strip_math_tags(x), collapse = ",\ ")
          , "]$"
      )
    } else {
      interval <- paste0(
        conf.int
        , "["
        , paste(x, collapse = ", ")
        , "]"
      )
    }

    interval
}

#' @rdname apa_interval
#' @method apa_interval matrix
#' @export

apa_interval.matrix <- function(
    x
    , conf.int = NULL
    , interval_type = NULL
    , enclose_math = FALSE
    , ...
) {
    ellipsis_ci <- deprecate_ci(conf.int = conf.int, ...)
    conf.int <- ellipsis_ci$conf.int
    ellipsis <- ellipsis_ci$ellipsis

    if(!is.null(colnames(x)) && is.null(conf.int)) {
      suppressWarnings(
        col_conf.int <- as.numeric(
          gsub("[^.|\\d]", "", colnames(x), perl = TRUE)
        )
      )
      col_conf.int <- if(anyNA(col_conf.int)) NULL else col_conf.int
      if(!is.null(col_conf.int)) conf.int <- 100 - col_conf.int[1] * 2
    }

    # if(nrow(x) == 1) {
    #     x <- as.vector(x)
    #     res <- apa_interval(x, conf.int = conf.int, use_math = use_math, interval_type = interval_type, ...)
    #     retrun(res)
    # }

    if(is.null(interval_type)) conf.int <- NULL

    if(!is.null(conf.int)) {
        if(conf.int < 1) conf.int <- conf.int * 100
        conf.int <- paste0(conf.int, "\\% ", interval_type, " ")
    }

    ellipsis$x <- x
    x <- do.call("apa_num", ellipsis)

    if(!is.null(rownames(x))) {
        terms <- sanitize_terms(rownames(x))
    } else {
        terms <- 1:nrow(x)
    }

    interval <- list()
    if(enclose_math) {
      for(i in 1:length(terms)) {
        interval[[terms[i]]] <- paste0(
            conf.int
            , "$["
            , paste(strip_math_tags(x[i, ]), collapse = ", ")
            , "]$"
        )
      }
    } else {
      for(i in 1:length(terms)) {
        interval[[terms[i]]] <- paste0(
          conf.int
          , "["
          , paste(x[i, ], collapse = ", ")
          , "]"
        )
      }
    }

    if(length(interval) == 1) interval <- unlist(interval)

    interval
}

#' @rdname apa_interval
#' @method apa_interval data.frame
#' @export

apa_interval.data.frame <- function(
  x
  , conf.int = NULL
  , interval_type = NULL
  , enclose_math = FALSE
  , ...
) {
    x <- as.matrix(x)
    apa_interval(x, ...)
}

#' @rdname apa_interval
#' @method apa_interval list
#' @export

apa_interval.list <- function(
  x
  , conf.int = NULL
  , interval_type = NULL
  , enclose_math = FALSE
  , ...
) {
  x <- as.data.frame(x)
  apa_interval(x, ...)
}


#' @rdname apa_interval
#' @export

apa_confint <- function(
  x
  , conf.int = NULL
  , interval_type = "CI"
  , enclose_math = FALSE
  , ...
) {
  apa_interval(x, interval_type = interval_type, ...)
}

#' @rdname apa_interval
#' @export

print_confint <- apa_confint

#' @rdname apa_interval
#' @export

apa_hdint <- function(
  x
  , conf.int = NULL
  , interval_type = "HDI"
  , enclose_math = FALSE
  , ...
) {
  apa_interval(x, interval_type = interval_type, ...)
}

#' @rdname apa_interval
#' @export

print_hdint <- apa_hdint
