#
#
#
#
# change <- function(x, ...) {
#   UseMethod("change", x)
# }
#
#
#
# change.apa_plot_list <- function(x, ...) {
#   x$plots <- lapply(X = x$plots, FUN = change, ...)
#   x
# }
#
#
#
# change.apa_plot <- function(x, ...) {
#   x$.state <- "modify"
#   x
# }
#
#
#
#
# add <- function(x, .call) {
#   UseMethod("add")
# }
#
#
#
# add.apa_plot_list <- function(x, .call) {
#   x |> .call
# }
#
#
# add.apa_plot <- function(x, .call) {
#   x$.state <- "add"
#   x |> list() |> do.call()
# }
