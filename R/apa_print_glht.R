apa_print.glht <- function(x, test = adjusted(), ...) {
  summary_x <- summary(x, test = test)

  apa_print(summary_x, ...)
}

apa_print.summary.glht <- function(x) {
  FALSE
}

apa_print.lsmobj <- function(x, ...) {
  summary_x <- summary(x, ...)

  apa_print(summary_x, ...)
}

apa_print.summary.ref.grid <- function(x, ci = 0.95, ...) {
  split_by <- attr(x, "by.vars")

  apa_res <- list()

  # Calculate confidence intervals (can't use confint() because it's a summary object)
  norm_quant <- 1 - (1 - ci) / 2
  x$ll <- x$estimate - x$SE * qnorm(norm_quant)
  x$ul <- x$estimate + x$SE * qnorm(norm_quant)

  if(ci < 1) conf_level <- ci * 100
  conf_level <- paste0("$", conf_level, "% CI $")

  # Assamble table
  contrast_list <- split(x, x[, split_by])
  contrast_list <- lapply(contrast_list, function(x) x[, -which(colnames(x) == split_by)])
  prep_table <- merge_tables(
    contrast_list
    , row_names = rep(FALSE, length(contrast_list))
    , added_colnames = split_by
  )
  contrast_table <- do.call(rbind, prep_table)
  contrast_df <- unique(contrast_table$df)

  ## Add confindence interval
  table_ci <- unlist(print_confint(matrix(c(ll, ul), ncol = 2), margin = 2, conf_level = NULL, ...))
  contrast_table <- cbind(
    contrast_table[, 1:which(colnames(contrast_table) == "estimate")]
    , data.frame(ci = table_ci)
    , contrast_table[, which(colnames(contrast_table) == "df"):ncol(contrast_table)]
  )

  contrast_table$estimate <- printnum(contrast_table$estimate, ...)
  contrast_table$p.value <- printp(contrast_table$p.value)
  contrast_table[, c("df", "t.ratio")] <- printnum(contrast_table[, c("df", "t.ratio")], margin = 2, digits = 2)

  if(length(contrast_df) == 1) { # Remove df column and put df in column heading
    contrast_table <- subset(contrast_table, select = -df)
    colnames(contrast_table) <- c(split_by, "Contrast", "$\\Delta M$", conf_level, paste0("$\frac{t}{t}(", contrast_df, ")$"), "$p$")
  } else {
    contrast_table <- cbind(
      contrast_table[, 1:which(colnames(contrast_table) == "ci")]
      , contrast_table[, c("t.ratio", "df", "p.value")]
    )
    colnames(contrast_table) <- c(split_by, "Contrast", "$\\Delta M$", conf_level, "$\frac{t}{t}$", "$df$", "$p$")

  }

  apa_res$table <- contrast_table

  apa_res
}
