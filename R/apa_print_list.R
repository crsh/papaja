#' Title
#'
#' @param x
#' @param ci
#' @param boot_samples
#' @param in_paren
#' @param ...
#'
#' @return
#' @export
#'
#' @examples

apa_print.list <- function(
  x
  , anova_fun = anova
  , ci = 0.90
  , boot_samples = 10000
  , in_paren = FALSE
  , ...
) {
  if(length(x) == 1) apa_print(x[[1]])

  if(!is.null(ci)) validate(ci, check_class = "numeric", check_length = 1, check_range = c(0, 1))
  validate(boot_samples, check_class = "numeric", check_length = 1)

  # Compare models
  model_comp <- do.call(anova_fun, x, ...)

  variance_table <- arrange_anova(model_comp)

  if("apa_model_comp" %in% class(variance_table)) { # Model comparison object
    return(print_model_comp(variance_table, models = x, ci = ci, boot_samples = boot_samples, in_paren = in_paren))
  }
}
