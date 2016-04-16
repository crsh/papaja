# Modified percentile bootstrap
# Algina, J., Keselman, H. J., & Penfield, R. D. (2007). Confiddence Intervals for an Effect Size Measure in Multiple Linear Regression.
# Educational and Psychological Measurement, 67(2), 207–218. http://doi.org/10.1177/0013164406292030

# x: apa_model_comp object
# models: list of lm-objects

delta_r2_ci <- function(x, models, ci = 0.90, R = 100, ...) {
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
  # data <- papaja:::fast_aggregate(data = data, factors = c(id, factors), dv = dv, fun = mean)
  between <- c()
  within <- c()

  for (i in 1:length(factors)) {

    if (all(rowSums(table(data[[id]], data[[factors[i]]])>0)==1)) {
      between <- c(between, factors[i])
    } else {
      within <- c(within, factors[i])
    }
  }

#   print(sapply(X = factors, FUN = function(x){
#     ifelse(all(rowSums(table(data[[id]], data[[x]]))==1), "between", "within")
#   }))

  test <- tapply(data[[dv]], as.list(data[, c(id, factors)]), FUN = function(x){sum(!is.na(x))})

  if(!all(test<=1||is.na(test))){
    stop("More than one observation per cell. Ensure you aggregated multiple observations per participant/within-subjects condition combination.")
  }

  # split by between factors
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
      z <- y - array(rowMeans(y, na.rm = TRUE), dim(y)) + mean(y, na.rm=TRUE) # normalise
      CI <- apply(z, MARGIN = (1:(length(within)+1))[-1], FUN = conf_int, level) # calculate CIs for each condition

      # Morey correction
      if(method=="Morey"){
        M <- prod(apply(X = as.matrix(x[, within]), MARGIN = 2, FUN = function(x){nlevels(as.factor(x))}))
        Morey_CI <- CI * M/(M-1)
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
    ee <- papaja:::fast_aggregate(data = dplyr::bind_rows(Morey_CI), factors = factors, dv = dv, fun =mean)
  } else {
    stop("No within-subjects factors specified.")
  }
  values <- ee
  attr(values, "Between-subjects factors") <- if(is.null(between)){"none"} else {between}
  attr(values, "Within-subjects factors") <- if(is.null(within)){"none"} else {within}
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
#' Returns the deviation that is needed to construct confidence intervals for a vector of observations.
#'
#' @param x Numeric. A vector of observations from your dependent variable.
#' @param level Numeric. Defines the width of the interval if confidence intervals are plotted. Defaults to 0.95
#'    for 95\% confidence intervals.
#' @param na.rm Logical. Specifies if missing values are removed.
#' @export

conf_int<-function(x, level = 0.95, na.rm = TRUE){
  a <- (1-level)/2
  n <- sum(!is.na(x))
  fac <- -suppressWarnings(qt(a, df = n-1))
  if(n==1){
    message("Only one observation in a cell. Thus, no confidence interval can be computed.")
  }
  ee <- (sd(x, na.rm = na.rm)*fac)/sqrt(n)
  return(ee)
}

#' @rdname conf_int
#' @export
conf.int <- conf_int




#' Standard errors
#'
#' Returns the standard error of a vector
#'
#' @param x Numeric. A vector of observations from your dependent variable.
#' @param na.rm Logical. Specifies if missing values are removed.
#' @export

se <- function(x, na.rm=TRUE){
  n <- sum(!is.na(x))
  ee <- sd(x, na.rm = na.rm)/sqrt(n)
  return(ee)
}
