# Modified percentile bootstrap
# Algina, J., Keselman, H. J., & Penfield, R. D. (2007). Confidence Intervals for an Effect Size Measure in Multiple Linear Regression.
# Educational and Psychological Measurement, 67(2), 207-218. https://doi.org/10.1177/0013164406292030

# x: apa_model_comp object
# models: list of lm-objects

delta_r2_ci <- function(x, models, ci = 0.90, R = 100, ...) {
  if(!package_available("boot")) stop("Please install the package 'boot' to calculate bootstrap confidence intervals.")

  validate(x, check_class = "data.frame")
  validate(length(models), "length(models)", check_range = c(2, Inf))
  validate(ci, check_range = c(0, 1))

  model_summaries <- lapply(models, summary)
  r2s <- sapply(model_summaries, function(x) x$r.squared)
  delta_r2s <- diff(r2s)

  percent_cis <- lapply(seq_along(delta_r2s), function(y) {

    delta_r2_samples <- boot::boot(
      get(as.character(model_summaries[[y]]$call$data))
      , function(data, i, calls) {
        bdata <- data[i, ]

        calls[[1]]$data <- bdata
        mod1 <- eval(calls[[1]])

        calls[[2]]$data <- bdata
        mod2 <- eval(calls[[2]])

        summary(mod2)$r.squared - summary(mod1)$r.squared
      }
      , calls = list(model_summaries[[y]]$call, model_summaries[[y + 1]]$call)
      , R = R
      , ...
    )

    boot_r2_ci <- boot::boot.ci(delta_r2_samples, conf = ci, type = "perc")

    # If difference is not significant set lower bound (closest to zero) == 0 (p. 210, Algina, Keselman & Penfield, 2007)
    if(x[y, "p.value"] >= (1 - ci) / 2) {
      lower_bound <- which(boot_r2_ci$percent[1, 4:5] == min(abs(boot_r2_ci$percent[1, 4:5])))
      boot_r2_ci$percent[1, 3 + lower_bound] <- 0
    }

    boot_r2_ci
  })

  percent_cis <- t(cbind(sapply(percent_cis, function(x) x$percent))) # Reformat to one CI per line
  percent_cis <- percent_cis[, 4:5, drop = FALSE] # Preserv matrix structure
  ci_levels <- c((1 - ci) / 2, (1 - ci) / 2 + ci) * 100
  colnames(percent_cis) <- paste(ci_levels, "%")
  attr(percent_cis, "ci.level") <- ci

  percent_cis
}



#' Within-subjects confidence intervals
#'
#' Calculate Cousineau-Morey within-subjects confidence intervals
#' @param data A \code{data.frame} that contains the data.
#' @param id Character. Variable name that identifies subjects.
#' @param factors Character. A vector of variable names that is used to stratify the data.
#' @param dv Character. The name of the dependent variable.
#' @param level Numeric. Defines the width of the interval. Defaults to 0.95
#'    for 95\% confidence intervals.
#' @param method Character. The method that is used to calculate. Actually,
#'          "Morey" and "Cousineau" are supported. Defaults to "Morey".
#' @references
#'    Morey, R. D. (2008). Confidence Intervals from Normalized Data: A correction to Cousineau (2005).
#'    \emph{Tutorials in Quantitative Methods for Psychology}, 4(2), 61--64.
#'
#'    Cousineau, D. (2005). Confidence intervals in within-subjects designs:
#'    A simpler solution to Loftus and Masson's method.
#'    \emph{Tutorials in Quantitative Methods for Psychology}, 1(1), 42--45.
#'
#'
#'
#' @examples
#' wsci(
#'    data = npk
#'    , id = "block"
#'    , dv = "yield"
#'    , factors = c("N", "P")
#' )
#' @export

wsci <- function(data, id, factors, dv, level = .95, method = "Morey") {
  # comment out again!
  # data <- fast_aggregate(data = data, factors = c(id, factors), dv = dv, fun = mean)

  # `split()` (below) needs standard factors, because it does not apply `as.factor`
  # by default
  for(i in c(id, factors)){
    data[[i]] <- as.factor(data[[i]])
  }

  # for (i in 1:length(factors)) {
  #   if (all(rowSums(table(data[[id]], data[[factors[i]]])>0)==1)) {
  #     between <- c(between, factors[i])
  #   } else {
  #     within <- c(within, factors[i])
  #   }
  # }

  tmp <- determine_within_between(data = data, id = id, factors = factors)

  between <- tmp$between
  within <- tmp$within

#   print(sapply(X = factors, FUN = function(x){
#     ifelse(all(rowSums(table(data[[id]], data[[x]]))==1), "between", "within")
#   }))

  test <- tapply(data[[dv]], as.list(data[, c(id, factors)]), FUN = function(x){sum(!is.na(x))})

  if(!all(test<=1||is.na(test))){
    stop("More than one observation per cell. Ensure you aggregated multiple observations per participant/within-subjects condition combination.")
  }

  # ----------------------------------------------------------------------------
  # Handling of missing values

  data <- complete_observations(data = data, id = id, dv = dv, within = within)

  # print warnings ----
  if("removed_cases_explicit_NA" %in% names(attributes(data))) {
    warning(
      "Because of NAs in the dependent variable, the following cases were removed from calculation of within-subjects confidence intervals:\n"
      , id
      , ": "
      , paste(attr(data, "removed_cases_explicit_NA"), collapse = ", ")
    )
  }
  if("removed_cases_implicit_NA" %in% names(attributes(data))) {
    warning(
      "Because of incomplete data, the following cases were removed from calculation of within-subjects confidence intervals:\n"
      , id
      , ": "
      , paste(attr(data, "removed_cases_implicit_NA"), collapse = ", ")
    )
  }


  # split by between-subjects factors ----
  if (is.null(between)) {
    splitted <- list(data)
  } else if(length(between)>1){
    splitted <- split(data, f=as.list(data[, c(between)]), sep = ":")
  } else if (length(between)==1) {
    splitted <- split(data, f=data[, c(between)])
  }

  if(!is.null(within)) {

    Morey_CI <- lapply(X = splitted, FUN = function(x){
      y <- tapply(x[[dv]], as.list(x[, c(id, within)]), FUN = as.numeric) # transform to matrix
      z <- y - rowMeans(y, na.rm = TRUE) + mean(y, na.rm = TRUE) # normalise
      CI <- apply(z, MARGIN = (1:(length(within)+1))[-1], FUN = conf_int, level) # calculate CIs for each condition

      # Morey correction
      if(method=="Morey"){
        M <- prod(apply(X = as.matrix(x[, within, drop = FALSE]), MARGIN = 2, FUN = function(x){nlevels(as.factor(x))}))
        Morey_CI <- CI * sqrt(M/(M-1))
      } else {
        method <<- "Cousineau"
        Morey_CI <- CI
      }

      # reshape to data.frame
      Morey_CI <- as.data.frame(as.table(Morey_CI))
      if(length(within)==1){
        colnames(Morey_CI)[colnames(Morey_CI)=="Var1"] <- within
      }
      colnames(Morey_CI)[colnames(Morey_CI)=="Freq"] <- dv
      # return
      Morey_CI
    })

    if(is.null(between)) {
      ee <- data.frame(unlist(Morey_CI, recursive=FALSE))
    } else {
      names <- strsplit(names(Morey_CI), split = ":")
      for (i in 1:length(Morey_CI)) {
        for ( j in 1:length(between)){
          Morey_CI[[i]][[between[j]]] <- names[[i]][j]
        }
      }
    }

    if(package_available("dplyr")) {
      ee <- fast_aggregate(data = dplyr::bind_rows(Morey_CI), factors = factors, dv = dv, fun = mean)
    } else {
      ee <- stats::aggregate(formula = stats::as.formula(paste0(dv, "~", paste(factors, collapse = "*"))), data = do.call(rbind, Morey_CI), FUN = mean)
    }

  } else {
    stop("No within-subjects factors specified.")
  }
  values <- ee
  attr(values, "Between-subjects factors") <- if(is.null(between)){"none"} else {between}
  attr(values, "Within-subjects factors") <- within
  attr(values, "Dependent variable") <- dv
  attr(values, "Subject identifier") <- id
  attr(values, "Confidence level") <- level
  attr(values, "Method") <- method
  return(values)
}



#' @rdname wsci
#' @export

within_subjects_conf_int <- wsci



#' Between-subjects confidence intervals
#'
#' Calculates the deviation that is needed to construct confidence intervals for a vector of observations.
#'
#' @param x Numeric. A vector of observations from your dependent variable.
#' @param level Numeric. Defines the width of the interval if confidence intervals are plotted. Defaults to 0.95
#'    for 95\% confidence intervals.
#' @param na.rm Logical. Specifies if missing values are removed.
#' @export

conf_int <- function(x, level = 0.95, na.rm = TRUE){
  validate(x, check_class = "numeric", check_NA = FALSE)
  validate(level, check_class = "numeric", check_length = 1, check_range = c(0, 1))

  a <- (1 - level)/2
  n <- sum(!is.na(x))
  fac <- -suppressWarnings(stats::qt(a, df = n-1))

  if(n < 2){
    message("Less than two non-missing values in at least one cell of your design: Thus, no confidence interval can be computed.")
  }

  ee <- (stats::sd(x, na.rm = na.rm) * fac) / sqrt(n)
  return(ee)
}

#' @rdname conf_int
#' @export

conf.int <- conf_int

#' @rdname conf_int
#' @export

ci <- conf_int


#' Standard errors
#'
#' Calculates the standard error of the mean
#'
#' @param x Numeric. A vector of observations.
#' @param na.rm Logical. Specifies if missing values should be removed.
#' @export

se <- function(x, na.rm = TRUE) {
  n <- sum(!is.na(x))
  ee <- stats::sd(x, na.rm = na.rm) / sqrt(n)
  return(ee)
}


#' Highest density interval
#'
#' Calculates the highest density interval of a vector of values
#'
#' @param x Numeric. A vector of observations.
#' @param level Numeric. Defines the width of the interval. Defaults to 95\% highest density intervals.

hd_int <- function(x, level = 0.95) {
  validate(x, check_class = "numeric")
  validate(level, check_class = "numeric", check_length = 1, check_range = c(0, 1))

  sorted_estimate_posterior <- sort(x)
  n_samples <- length(sorted_estimate_posterior)
  gap <- max(1, min(n_samples - 1, round(n_samples * level)))
  init <- 1:(n_samples - gap)
  lower_index <- which.min(sorted_estimate_posterior[init + gap] - sorted_estimate_posterior[init])
  hdinterval <- cbind(sorted_estimate_posterior[lower_index], sorted_estimate_posterior[lower_index + gap])
  colnames(hdinterval) <- c(paste((1 - level) / 2 * 100, "%"), paste(((1 - level) / 2 + level) * 100, "%"))
  attr(hdinterval, "conf.level") <- level
  hdinterval
}


#' Effect sizes for Analysis of Variance
#'
#' Calculates effect-size measures for Analysis of Variance output objects.
#'
#' @param x An object of class \code{apa_variance_table}.
#' @param es Character. A vector naming all to-be-computed effect-size measures.
#'   Currently, partial eta-quared (\code{"pes"}), generalized eta-squared
#'   (\code{"ges"}), and eta-squared (\code{"es"}) are supported.
#' @param observed Character. A vector naming all factors that are observed
#'   (i.e., \emph{not} manipulated).
#' @param mse Logical. Should means-squared errors be computed?
#' @param intercept Logical. Should the sum of squares of the intercept (i.e., the
#'   deviation of the grand mean from 0) be included in the calculation of eta-squared?

add_effect_sizes <- function(x, es = "ges", observed = NULL, mse = TRUE, intercept = FALSE) {
  # ----------------------------------------------------------------------------
  # We don't validate here because this function is intended to be used
  # internally, validation, should have happened earlier in the processing chain.

  # validate(x, check_class = "apa_variance_table", check_NA = FALSE)
  # validate(es, check_class = "character", check_NA = FALSE)

  if(!is.null(es)) {
    # Stop if the user requires a non-supported effect-size measure ----
    if(!all(es %in% c("pes", "ges", "es"))) {
      stop("Requested effect size measure(s) currently not supported: ", paste(es, collapse = ", "), ".")
    }

    # --------------------------------------------------------------------------
    # Calculate generalized eta-squared
    #
    # This code is a copy from the afex package by Henrik Singmann et al.
    # In the package's source code, it is stated that the code is basically a copy
    # from ezANOVA by Mike Lawrence
    if("ges" %in% es) {
      if(!is.null(observed)) {
        obs <- rep(FALSE, nrow(x))
        for(i in observed) {
          if (!any(grepl(paste0("\\<", i, "\\>", collapse = "|"), rownames(x)))) {
            stop(paste0("Observed variable not in data: ", i, collapse = " "))
          }
          obs <- obs | grepl(paste0("\\<", i, "\\>", collapse = "|"), rownames(x))
        }
        obs_SSn1 <- sum(x$sumsq*obs, na.rm = TRUE)
        obs_SSn2 <- x$sumsq*obs
      } else {
        obs_SSn1 <- 0
        obs_SSn2 <- 0
      }
      x$ges <- x$sumsq / (x$sumsq + sum(unique(x$sumsq_err)) + obs_SSn1 - obs_SSn2)
    }

    # --------------------------------------------------------------------------
    # Calculate eta-squared
    #
    # In it's current implementation, correct calculation of eta-squared relies
    # on the fact that the design is balanced (otherwise, the summation below)
    # is simply false. Replacing this term by the sum of squared deviations of
    # individual observations from the grand mean (the general specification)
    # would be highly desirable. However, most ANOVA outputs do not provide
    # the necessary information, so we have to go with this hack.
    if("es" %in% es) {
      index <- rep(TRUE, nrow(x))
      if(!intercept){
        index <- x$term!="(Intercept)"
      }
      x$es <- x$sumsq / sum(x$sumsq[index], unique(x$sumsq_err))
      message("Note that eta-squared is calculated correctly if and only if the design is balanced.")
    }

    # --------------------------------------------------------------------------
    # Calculate partial eta-squared
    #
    # This one should be unproblematic and work in all cases.
    if("pes" %in% es) {
      x$pes <- x$sumsq / (x$sumsq + x$sumsq_err)
    }
  }

  # ----------------------------------------------------------------------------
  # Only calculate MSE if required (otherwise, Levene tests give an error).
  if(mse) {
    x$mse <- x$sumsq_err / x$df_res
  }

  x
}

