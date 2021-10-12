

#' @export

apa_plot <- function(x, ...) {
  UseMethod("apa_plot")
}



#' @examples
#' apa_plot(yield ~ (1 | block), data = npk) |>
#'  x_axis() |>
#'  y_axis() |>
#'  annotation() |>
#'  swarms() |>
#'  error_bars() |>
#'  points()
#' apa_plot(yield ~ (N | block), data = npk) |>
#'  x_axis() |>
#'  y_axis() |>
#'  annotation() |>
#'  swarms() |>
#'  error_bars() |>
#'  points()
#' apa_plot(yield ~ (N * P * K| block), data = npk) |>
#'  x_axis() |>
#'  y_axis() |>
#'  annotation() |>
#'  swarms() |>
#'  error_bars() |>
#'  points()
#'  \dontrun{
#'  # Both left-hand side and right-hand side must be provided:
#'  apa_plot(~ N * P)
#'  # One and only one identifier variable has to be provided:
#'  apa_plot(yield ~ 1)
#'  apa_plot(yield ~ (1|N*P), data = npk)
#'  }
#'
#' @export

apa_plot.formula <- function(formula, data, ...) {
  formula_processor(formula = formula, data = data, .fun = apa_plot, ...)
}



#' @keywords internal

formula_processor <- function(formula, data, .fun, ...) {
  if(length(formula) < 3L) stop("Please provide a formula with both left- and right-hand side variables.", call. = FALSE)

  lhs <- all.vars(formula[[2L]])
  rhs <- all.vars(formula[[3L]])

  deparsed_terms <- lapply(formula[[3L]], deparse)
  bar_term <- grep(deparsed_terms, pattern = "|", fixed = TRUE)

  if(length(bar_term) != 1L) stop("Please provide one and only one id variable.", call. = FALSE)
  all_rhs <- all.vars(formula[[3L]])

  split_bar_term <- strsplit(deparsed_terms[[bar_term]], split = "|", fixed = TRUE)[[1L]][[2L]]
  id_var <- gsub(split_bar_term, pattern = ")| ", replacement = "")

  .fun(
    data = data
    , dv = lhs
    , id = id_var
    , factors = setdiff(all_rhs, id_var)
    , ...
  )
}


#' @method apa_plot data.frame
#' @export

apa_plot.data.frame <- function(
  data
  , dv
  , id
  , factors = NULL
  , use = "all.obs"
  , tendency = mean
  , dispersion = conf_int
  , level = .95
  , fun_aggregate = mean
  , jit = .3
  , mfrow = NULL
  , reference = 0
  , ...
) {

  ellipsis <- list(...)

  validate(jit, check_class = "numeric")


  # Sanitize data --------------------------------------------------------------

  # remove extraneous columns from dataset
  data <- data[, c(id, factors, dv), drop = FALSE]

  # Add missing variable labels
  data <- default_label(data)

  # todo: consider using make.names()
  factors <- gsub(pattern = " ", replacement = "_", factors, fixed = TRUE)
  id <- gsub(pattern = " ", replacement = "_", id, fixed = TRUE)
  dv <- gsub(pattern = " ", replacement = "_", dv, fixed = TRUE)
  colnames(data) <- gsub(pattern = " ", replacement = "_", colnames(data))
  original_labels <- variable_label(data)

  # Handling of factors:
  # a) convert to factor
  for (i in c(id, factors)){
    data[[i]] <- as.factor(data[[i]])
  }
  # b) drop factor levels
  data <- droplevels(data)
  # Handling of dependent variable:
  data[[dv]] <- as.numeric(data[[dv]]) # Todo: allow length(dv) > 1


  # Check if specified factors contain more than one level after applying `droplevels`
  for (i in factors) {
    nl <- nlevels(data[[i]])
    if(nl < 2L) {
      fct <- encodeString(i, quote = "'")
      warning("Factor ", fct, " contains only ", nl, " level, and is thus ignored.")
      factors <- setdiff(factors, i)
    }
  }

  variable_label(data) <- original_labels

  # Aggregate data -------------------------------------------------------------

  # is dplyr available?
  use_dplyr <- requireNamespace("dplyr", quietly = TRUE) # much faster than `package_available("dplyr")`

  # Aggregate subject data ----
  # Check if aggregation is necessary
  # this is a sub-optimal solution: if we had information about which factors are
  # within, this would be faster; for this purpose, we could use the code-bit
  # from `papaja::wsci`
  # `base::table` is a bit faster than `stats::xtabs`
  if(any(table(data[, c(id, factors)]) > 1L)){
    if(use_dplyr) {
      aggregated <- fast_aggregate(data = data, dv = dv, factors = c(id, rev(factors)), fun = fun_aggregate)
      variable_label(aggregated[[dv]]) <- variable_label(data[[dv]])
    } else {
      aggregated <- stats::aggregate.data.frame(x = data[dv, drop = FALSE], by = data[, c(id, rev(factors))], FUN = fun_aggregate)
    }
  } else {
    aggregated <- data
  }

  # Check if there are incomplete observations and eventually remove them ------
  if(use=="complete.obs") {
    # excluded_id <- sort(unique(aggregated[[id]][is.na(aggregated[[dv]])]))
    #
    # data <- data[!data[[id]]%in%excluded_id, ]
    # aggregated <- aggregated[!aggregated[[id]]%in%excluded_id, ]
    tmp <- determine_within_between(data = aggregated, id = id, factors = factors)
    aggregated <- complete_observations(data = aggregated, id = id, within = tmp$within, dv = dv)
    removed_cases <- unlist(attributes(aggregated)[c("removed_cases_implicit_NA", "removed_cases_explicit_NA")])
    if(!is.null(removed_cases)) {
      excluded_id <- sort(unique(removed_cases))
      data <- data[!data[[id]] %in% excluded_id, ]
    }
  }

  # Calculate y coordinates ----------------------------------------------------
  fun_dispersion <- deparse(substitute(dispersion))

  if(fun_dispersion == "within_subjects_conf_int" || fun_dispersion == "wsci") {
    y.values <- summary(wsci(data = aggregated, id = id, factors = factors, level = level, method = "Morey", dv = dv))
    y.values[[dv]] <- Map(
      f = list
      , y.values$lower_limit
      , y.values$mean
      , y.values$upper_limit
    )
    y.values$mean <- y.values$lower_limit <- y.values$upper_limit <- NULL
  } else {
    if(fun_dispersion == "conf_int") {
      agg_fun <- function(x, level, ...) {
        y <- tendency(x, ...) + c(-1, 0, +1) * dispersion(x, level, ...)
        variable_label(y) <- variable_label(x)
        y
      }
    } else {
      agg_fun <- function(x, level, ...) { # 'level' goes to hell
        y <- tendency(x, ...) + c(-1, 0, +1) * dispersion(x, ...)
        variable_label(y) <- variable_label(x)
        y
      }
    }

    y.values <- stats::aggregate(
      x = aggregated[, dv, drop = FALSE]
      , by = aggregated[, rev(factors), drop = FALSE] # rev() for correct ordering
      , FUN = agg_fun
      , level = level
      , na.rm = TRUE
      , simplify = FALSE
    )
  }



  # Default for ylim: Cover all (potentially) plotted shapes ----
  default_ylim <- range(
    0
    , y.values[, dv, drop = FALSE]
    , aggregated[, dv, drop = FALSE]
    , reference
    , na.rm = TRUE
  )

  # allow to partially define 'ylim', e.g. `c(20, NA)`
  ylim <- ellipsis$ylim

  if(is.null(ellipsis$ylim)) {
    ylim <- default_ylim
  } else if (anyNA(ellipsis$ylim)){
    ylim[is.na(ellipsis$ylim)] <- default_ylim[is.na(ellipsis$ylim)]
  }


  # Calculate 'x' coordinates -----
  space <- 1 - jit

  if( length(factors) > 0L ) {
    numeric_fct1     <- as.numeric( y.values  [[ factors[[1L]] ]] )
    numeric_fct1_agg <- as.numeric( aggregated[[ factors[[1L]] ]] )

    if( length(factors) > 1L ) {
      numeric_fct2     <- as.numeric( y.values  [[ factors[[2L]] ]] )
      numeric_fct2_agg <- as.numeric( aggregated[[ factors[[2L]] ]] )
      nlevels_fct2 <- nlevels( y.values[[ factors[[2L]] ]] )

      x_coords     <- numeric_fct1     - 1 + space/2 + (1-space) / (nlevels_fct2 - 1) * (numeric_fct2     - 1)
      x_coords_agg <- numeric_fct1_agg - 1 + space/2 + (1-space) / (nlevels_fct2 - 1) * (numeric_fct2_agg - 1)

    } else {
      x_coords     <- numeric_fct1     - 0.5
      x_coords_agg <- numeric_fct1_agg - 0.5
    }
  } else {
    x_coords     <- 0.5
    x_coords_agg <- rep(0.5, nrow(aggregated))
  }


  # split data and output ----
  not_fct12 <- -c(1:2)

  x_split <- split(x_coords, f = cbind(y.values[, factors[not_fct12], drop = FALSE], 1L))
  y_split <- split(y.values, f = cbind(y.values[, factors[not_fct12], drop = FALSE], 1L))
  x_split_agg <- split(x_coords_agg, f = cbind(aggregated[, factors[not_fct12], drop = FALSE], 1L))
  y_split_agg <- split(aggregated, f = cbind(aggregated[, factors[not_fct12], drop = FALSE], 1L))

  as_apa_plot <- function(...) {
    structure(list(..., .state = "add"), class = c("apa_plot", "list"))
  }

  return(
    structure(
      list(
        plots = mapply(
          x = x_split
          , y = y_split
          , x_agg = x_split_agg
          , y_agg = y_split_agg
          , MoreArgs = list(
            input = list(
              id = id
              , dv = dv
              , factors = factors
              , reference = reference
              , jit = jit
            )
            , visuals = list(
              plot.new = list(
                .f = "plot.new"
                , .creator = "apa_plot"
                , args = list()
              )
              , plot.window = list(
                .f = "plot.window"
                , .creator = "apa_plot"
                , args = list(
                  xlim = if(length(factors) > 0L) c(0, nlevels(y.values[[factors[[1L]]]])) else c(0, 1)
                  , ylim = ylim
                  , log = gsub(paste0(ellipsis$log, ""), pattern = "x", replacement = "", fixed = TRUE)
                  , xaxs = ellipsis$xaxs
                  , yaxs = ellipsis$yaxs
                  , lab  = ellipsis$lab
                )
              )
            )
          )
          , FUN = as_apa_plot
          , SIMPLIFY = FALSE
        )
        , input = list(
          mfrow = mfrow
        )
      )
      , class = c("apa_plot_list", "list")
    )
  )

}


