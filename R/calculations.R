# Modified percentile bootstrap
# Algina, J., Keselman, H. J., & Penfield, R. D. (2007). Confiddence Intervals for an Effect Size Measure in Multiple Linear Regression.
# Educational and Psychological Measurement, 67(2), 207–218. http://doi.org/10.1177/0013164406292030

# x: apa_model_comp object
# models: list of lm-objects

delta_r2_ci <- function(x, models, ci = 0.90, R = 100, ...) {
  # validate(x, check_class = "list")
  # validate(length(x), "length(x)", check_range = c(2, Inf))
  # validate(ci, check_range = c(0, 1))

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

    boot_r2_ci <- boot::boot.ci(delta_r2_samples, ci = ci, type = "perc")

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
#'
#'
#'
#'
#'
#' wsci(
#'    data = npk
#'    , id = "block"
#'    , dv = "yield"
#'    , factors = c("N")
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
    stop("More than one observation per cell. Ensure you aggregated multiple observations per participant/within-subjects condition combination")
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


