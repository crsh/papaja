#' Replace Parentheses with Brackets
#'
#' Takes a single character or a list of characters and replaces parentheses
#' with brackets. Can be used to prepare a string of statistics (e.g. containing
#' degrees of freedom) for reporting within parentheses.
#'
#' @param x Character. Single character or list of characters.
#' @return An object of the same type as `x`.
#'
#' @seealso apa_print
#' @examples
#' t_stat <- t.test(extra ~ group, data = sleep)
#' t_test_res <- apa_print(t_stat)
#' in_paren(t_test_res$stat)
#' in_paren(t_test_res[1:3])
#' @export

in_paren <- function(x) {
  if(is.list(x)) {
    lapply(
      X = x
      , FUN = function(x) {
        gsub(
          gsub(x, pattern = "(", replacement = "[", fixed = TRUE)
          , pattern = ")", replacement = "]", fixed = TRUE
        )
      }
    )
  } else {
    gsub(
      gsub(x, pattern = "(", replacement = "[", fixed = TRUE)
      , pattern = ")", replacement = "]", fixed = TRUE
    )
  }
}
