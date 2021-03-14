

add_custom_effect_sizes <- function(es, ...) {
  UseMethod("add_custom_effect_sizes", es)
}


add_custom_effect_sizes.character <- function(es, canonical_table, .x = NULL, ...) {

  if(length(es) > 1L) warning("Calculating more than one effect-size measure is now deprecated.")
  es <- es[[1L]]

  add_effect_sizes(x = canonical_table, es = es, ...)
}


add_custom_effect_sizes.data.frame <- function(es, canonical_table, intercept = FALSE, ...) {

  if(!intercept) canonical_table <- canonical_table[canonical_table$term != "(Intercept)", , drop = FALSE]

  y <- merge(canonical_table,
             tidy_es(es)
             , sort = FALSE
             , all.x = TRUE # Do not drop
  )
  if(anyNA(y)) {
    warning("Custom effect sizes were not available for some model terms. These have been dropped from the output object.", call. = FALSE)
    y <- stats::na.omit(y)
  }
  y
}


add_custom_effect_sizes.function <- function(es, .x = NULL, ...) {

  if(is.null(.x)) stop("Cannot apply custom effect-size function to this class of object.", call. = FALSE)

  add_custom_effect_sizes(es = es(.x), .x = .x, ...)
}
