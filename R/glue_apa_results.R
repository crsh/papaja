#' Create a New `apa_results` Object
#'
#' Typeset the contents of an object according to the specified expression
#' strings and create a new or extend an existing `apa_results` object.
#'
#' @param x An environment, list or data frame used to look up values for
#'    substitution.
#' @param est_glue Character. (Named vector of) expressions string(s) to
#'     format. Each string creates a new (named) element in the
#'    `estimate` sub-list.
#' @param stat_glue Character. (Named vector of) expressions string(s) to
#'     format. Each string creates a new (named) element in the
#'    `statistic` sub-list.
#' @param container List of class `apa_results` to add the glued results to.
#' @param sublist Character. Name of (new) sub-list in `estimate`
#'    `statistics`, and `full_result` to append glued results to (e.g.,
#'    `modelfit`).
#' @param term_names Character. Used as names for the `estimate`-,
#'    `statistics`-, and `full_result` sub-lists, if multiple estimates or
#'    statistics are glued.
#' @param in_paren Logical. Whether the formatted string is to be reported in
#'    parentheses. If `TRUE`, parentheses in the formatted string (e.g., those
#'    enclosing degrees of freedom) are replaced with brackets.
#' @param est_first Logical. Determines in which order `estimate` and `statistic`
#'    are glued together to `full_result`.
#' @param simplify Logical. Determines whether the `estimate`, `statistic`, and
#'    `full_result` sub-lists should be simplified if only one term is available
#'    from the model object.
#' @inheritParams glue::glue
#'
#' @return Returns a list of class `apa_results`
#' @export
#'
#' @examples
#' # Tidy and typeset output
#' iris_lm <- lm(Sepal.Length ~ Petal.Length + Petal.Width, iris)
#' tidy_iris_lm <- broom::tidy(iris_lm, conf.int = TRUE)
#' tidy_iris_lm$p.value <- printp(tidy_iris_lm$p.value)
#'
#' glance_iris_lm <- broom::glance(iris_lm)
#' glance_iris_lm$p.value <- printp(glance_iris_lm$p.value, add_equals = TRUE)
#' glance_iris_lm$df <- printnum(as.integer(glance_iris_lm$df))
#' glance_iris_lm$df.residual <- printnum(as.integer(glance_iris_lm$df.residual))
#'
#' # Create `apa_results`-list
#' lm_results <- glue_apa_results(
#'     x = tidy_iris_lm
#'     , df = glance_iris_lm$df.residual
#'     , est_glue = "$b = <<estimate>>, 95% CI $[<<conf.low>>,~<<conf.high>>]$"
#'     , stat_glue = "$t(<<df>>) = <<statistic>>$, $p <<p.value>>$"
#'     , term_names = make.names(names(coef(iris_lm)))
#' )
#'
#' # Add modelfit information
#' add_glue_to_apa_results(
#'     .x = glance_iris_lm
#'     , container = lm_results
#'     , sublist = "modelfit"
#'     , est_glue = c(
#'         r2 = "$R^2 = <<r.squared>>$"
#'         , aic = ""
#'     )
#'     , stat_glue = c(
#'         r2 = "$F(<<df>>, <<df.residual>>) = <<statistic>>$, $p <<papaja:::add_equals(p.value)>>$"
#'         , aic = "$\\mathrm{AIC} = <<AIC>>$"
#'     )
#' )

glue_apa_results <- function(x = NULL, ...) {
    if(!is.null(x)) validate(x, check_class = "data.frame")

    apa_res <- add_glue_to_apa_results(
        .x = x
        , ...
        , container = init_apa_results()
    )

    if(!is.null(x) && is.data.frame(x)) {
      if(!inherits(x, "apa_results_table")) {
        if("term" %in% names(x)) x$term <- prettify_terms(x$term)
      }
      if("conf.int" %in% names(x)) x$conf.int <- gsub("\\\\infty", "$\\\\infty$", x$conf.int)
      apa_res$table <- x
    }

    apa_res
}

#' @rdname glue_apa_results
#' @export

add_glue_to_apa_results <- function(
    ...
    , est_glue
    , stat_glue
    , container
    , sublist = NULL
    , term_names = NULL
    , in_paren = FALSE
    , est_first = TRUE
    , simplify = TRUE
) {
  validate(container, check_class = "apa_results")

  in_paren <- isTRUE(in_paren)
  simplify <- isTRUE(simplify)


    est_list <- unlist(lapply(
        est_glue
        , apa_glue
        , ...
    ))
    stat_list <- unlist(lapply(
        stat_glue
        , apa_glue
        , ...
    ))

    if(in_paren) {
        est_list <- in_paren(est_list)
        stat_list <- in_paren(stat_list)
    }

    # full_result ----
    paste_pars <- list(sep = ", ")
    paste_pars$est  <- est_list
    paste_pars$stat <- stat_list
    if(!est_first) paste_pars <- paste_pars[c("sep", "stat", "est")]

    full_list <- do.call("paste", paste_pars)
    names(full_list) <- names(est_list)

    res <- list(
        estimate = est_list
        , statistic = stat_list
        , full_result = full_list
    )

    # return as lists if more than one term
    res[1:3] <- lapply(X = res, FUN = function(x) {

        # Remove empty elements (e.g., only estimate or statistic defined)
        x <- gsub(x, pattern = "^, |, $", replacement = "")
        x <- x[x != ""]

        if(!simplify | length(x) > 1L) {
          x <- as.list(x)
          if(!is.null(term_names) & length(x) > 0L) names(x) <- term_names
        }
        return(x)
    })

    if(is.null(sublist)) {
        if(length(res$estimate) > 0L) container[["estimate"]] <- res$estimate
        if(length(res$statistic) > 0L) container[["statistic"]] <- res$statistic
        if(length(res$full_result) > 0L) container[["full_result"]] <- res$full_result
    } else {
        if(length(res$estimate) > 0L) container[["estimate"]][[sublist]] <- res$estimate
        if(length(res$statistic) > 0L) container[["statistic"]][[sublist]] <- res$statistic
        if(length(res$full_result) > 0L) container[["full_result"]][[sublist]] <- res$full_result
    }

    container
}


apa_glue <- function(glue_str, ...) {
    validate(glue_str, check_class = "character")

    ellipsis <- list(...)

    ellipsis <- c(ellipsis, glue_str)
    if(is.null(ellipsis$.open)) ellipsis$.open <- "<<"
    if(is.null(ellipsis$.close)) ellipsis$.close <- ">>"
    if(is.null(ellipsis$.transformer)) ellipsis$.transformer <- printnum_transformer

    do.call(glue::glue_data, ellipsis)
}

printnum_transformer <- function(text, envir) {
  res <- eval(parse(text = text, keep.source = FALSE), envir)
  printnum(res)
}


construct_glue <- function(x, type) {
  if(type == "estimate") {
    return(est_glue(x))
  } else if(type == "statistic") {
    return(stat_glue(x))
  } else stop("'type' must be either 'estimate' or 'statistic'.")
}

est_glue <- function(x) {
  if(is.null(x$estimate)) return("")
  est_glue <- "$<<svl(estimate)>> = <<estimate>>$"
  if(!is.null(x$conf.int)) {
      est_glue <- paste0(est_glue, ", <<svl(conf.int)>> $<<conf.int>>$")
  }
  est_glue
}

stat_glue <- function(x) {
  if(is(x, "tbl_df")) x <- as.data.frame(x)
  if(is.null(x$statistic)) return("")

  # # Remove degrees of freedom
  # id <- gsub("^([a-zA-Z0-9_\\^\\\\ \\{\\}]+).*", "\\1", svl(x))

  # selected_glue <- unname(statistic_glues[id])

  # if(is.na(selected_glue)) stop("No glue found for '", id, "''")

  # if(length(selected_glue) == 0) {
  #   return("")
  # } else {
  #   return(selected_glue)
  # }

  df <- NULL
  if(!is.null(x$df)) {
    if(!is.null(x$df.residual)) {
      df <- "(<<df>>, <<df.residual>>)"
    } else if( # Chi^2-Test
      variable_labels(x$statistic) == "$\\chi^2$" &&
      exists("n", where = parent.frame(2), mode = "numeric") &&
      !is.null(get("n", envir = parent.frame(2), mode = "numeric"))
    ) {
      df <- "(<<df>>, n = <<n>>)"
    } else {
      df <- "(<<df>>)"
    }
  }

  stat_list <- c()

  if(!is.null(x$multivariate.statistic)) {
    stat_list <- c(
      stat_list
      , "$<<svl(multivariate.statistic)>> <<add_equals(multivariate.statistic)>>$"
    )
  }
  if(!is.null(x$statistic)) {
    stat_list <- c(
      stat_list
      , glue::glue_collapse(
        c("$<<svl(statistic)>>", df, " ", "<<add_equals(statistic)>>$")
      )
    )
  }
  if(!is.null(x$mse)) {
    stat_list <- c(
      stat_list
      , "$<<svl(mse)>> <<add_equals(mse)>>$"
    )
  }


  p_value <- names(x)[grepl("p.value", names(x), fixed = TRUE)]
  if(length(p_value) > 0) {
    stat_list <- c(
      stat_list
      , paste0("$<<svl(", p_value, ")>> <<add_equals(", p_value, ")>>$")
    )
  }

  constructed_glue <- glue::glue_collapse(
    stat_list
    , sep = ", "
  )

  unclass(constructed_glue)
}


#' Strip Math Tags from Variable Labels or Strings
#'
#' Internal function to strip math tags from variable labels or strings. `svl()`
#' returns the stripped variable label of `x`, if available. `strip_math_tags` returns
#' the stripped character `x`.
#'
#' @param x A (labelled) character string.
#'
#' @rdname strip_math_tags
#' @keywords internal

svl <- function(x) {
  y <- variable_labels(x)
  if(is.null(y)) y <- x

  strip_math_tags(y)
}

#' @rdname strip_math_tags
#' @keywords internal

strip_math_tags <- function(x) {
  gsub(pattern = "$", replacement = "", x = x, fixed = TRUE)
}
