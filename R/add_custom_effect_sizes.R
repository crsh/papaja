

add_custom_effect_sizes <- function(estimate, ...) {
  UseMethod("add_custom_effect_sizes", estimate)
}


add_custom_effect_sizes.character <- function(estimate, canonical_table, .x = NULL, ...) {

  if(length(estimate) > 1L) warning("Calculating more than one effect-size measure is now deprecated. Only the first one will be calculated.")
  estimate <- estimate[[1L]]

  add_effect_sizes(x = canonical_table, es = estimate, ...)
}


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


add_custom_effect_sizes.function <- function(estimate, .x = NULL, ...) {

  if(is.null(.x)) stop("Cannot apply custom effect-size function to this class of object.", call. = FALSE)

  add_custom_effect_sizes(estimate = estimate(.x), .x = .x, ...)
}
