#' @keywords internal

add_custom_effect_sizes <- function(estimate, ...) {
  UseMethod("add_custom_effect_sizes", estimate)
}

#' @keywords internal

add_custom_effect_sizes.character <- function(estimate, canonical_table, .x = NULL, ...) {

  if(length(estimate) > 1L) warning("Calculating more than one effect-size measure is now deprecated. Only the first one will be calculated.")
  estimate <- estimate[[1L]]

  add_effect_sizes(x = canonical_table, es = estimate, ...)
}

#' @keywords internal

add_custom_effect_sizes.data.frame <- function(estimate, canonical_table, intercept = FALSE, ...) {

  if(!intercept) canonical_table <- canonical_table[canonical_table$term != "(Intercept)", , drop = FALSE]

  y <- merge(
    x = canonical_table
    , y = tidy_es(estimate)
    , sort = FALSE
    , all.x = TRUE # Do not drop terms from main results object
  )

  if(anyNA(y)) {
    warning("Custom effect sizes were not available for some model terms. These have been dropped from the output object.", call. = FALSE)
    y <- stats::na.omit(y)
  }
  y
}

#' @keywords internal

add_custom_effect_sizes.function <- function(estimate, canonical_table, intercept = FALSE, .x = NULL, observed = NULL, ...) {

  if(is.null(.x)) stop("Cannot apply custom effect-size function to this class of object.", call. = FALSE)

  estimate_formals <- names(formals(estimate))

  estimate_args <- list(
    .x
  )

  if(length(observed)) {
    if(any(estimate_formals == "observed"))    estimate_args$observed    <- observed
    if(any(estimate_formals == "generalized")) estimate_args$generalized <- observed

    # warnings
    if(!any(estimate_formals == "observed") & !any(estimate_formals == "generalized")) {
      warning(
        "Some terms have been specified as being observed, but the provided effect-size function does not seem to support observed terms."
        , call. = FALSE
      )
    }
  }

  if(isTRUE(intercept) & any(estimate_formals == "include_intercept")) {
    estimate_args$include_intercept <- TRUE
    message("Turning on intercept")
  }


  add_custom_effect_sizes(
    estimate = do.call(what = estimate, args = estimate_args)
    , canonical_table = canonical_table
    , .x = .x
    , ...
  )
}
