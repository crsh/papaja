tidy_es <- function(x) {
  UseMethod("tidy_es", x)
}

tidy_es.default <- function(x) {
  tryCatch(
    broom::tidy(x)
    , error = function(e) {
      stop("The object returned by the function passed to 'es' could not be tidied, that is converted to a data.frame with columns 'estimate', 'conf.low', and 'conf.high'. Please supply a function that returns a data.frame of this structure or pass such a data.frame directly.")
    }
  )
}

tidy_es.data.frame <- function(x) {
  es_cols <- colnames(x)
  if(
    !"estimate" %in% es_cols |
    !"conf.low" %in% es_cols |
    !"conf.high" %in% es_cols
  ) {
    stop("Effect size data.frame must contain the colums 'estimate', 'conf.low', and 'conf.high'")
  }

  x
}

tidy_es.effectsize_table <- function(x) {

  x <- as.data.frame(x, stringsAsFactors = FALSE)
  x$conf.int <- apply(X = x[, c("CI_low", "CI_high"), drop = FALSE], MARGIN = 1L, FUN = list)
  x$conf.int <- lapply(X = x$conf.int, unlist)
  attr(x$conf.int, "conf.level") <- unique(x$CI)
  x$Effect <- x$Parameter
  x$CI_low <- x$CI_high <- x$CI <- x$Parameter <- NULL

  canonize(x)
}

tidy_es.parameters_model <- function(x) {
  tidy_es.effectsize_table(x)
}

tidy_es.parameters_distribution <- function(x) {
  tidy_es.effectsize_table(x)
}
