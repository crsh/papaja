printnumber <- function(x, digits, gt1, zero) {
  if(is.na(x)) return("")
  x_out <- round(x, digits) + 0 # No sign if x_out == 0
  
  if(sign(x_out) == -1) {
    xsign <- "-"
    lt <- "> "
  } else {
    xsign <- ""
    lt <- "< "
  }
  
  if(x_out == 0 & !zero) x_out <- paste0(lt, xsign, ".", paste0(rep(0, digits-1), collapse = ""), "1") # Too small to report

  if(!gt1) {
    if(x_out %in% c(-1,1)) x_out <- paste0(lt, xsign, ".", paste0(rep(9, digits), collapse = "")) # Never report 1

    x_out <- formatC(x_out, format = "f", digits = digits, flag = "0") # Fill to desired number of digits
    x_out <- gsub("0\\.", "\\.", x_out)
  } else {
    x_out <- formatC(x_out, format = "f", digits = digits, flag = "0") # Fill to desired number of digits
  }
  return(x_out)
}


printnum <- function(x, digits = 2, gt1 = TRUE, zero = TRUE, margin = 1) {
  if(length(x) > 1) {
    print_args <- list(digits, gt1, zero)
    vprintnumber <- function(i, x) printnumber(x[i], digits = print_args[[1]][i], gt1 = print_args[[2]][i], zero = print_args[[3]][i])
  }
  
  if(is.matrix(x)) {
    x_out <- apply(x, margin, printnum, digits = print_args[[1]], gt1 = print_args[[2]], zero = print_args[[3]]) # Inception!
    if(margin == 1) x_out <- t(x_out)
    colnames(x_out) <- colnames(x)
  } else if(is.vector(x) & length(x) > 1) {
    print_args <- lapply(print_args, rep, length = length(x)) # Recycle arguments
    x_out <- sapply(seq_along(x), vprintnumber, x)
  } else {
    x_out <- printnumber(x, digits, gt1, zero)
  }
  return(x_out)
}

printp <- function(x, margin = 1) {
  p <- printnum(x, digits = 3, gt1 = FALSE, zero = FALSE, margin = margin)
  return(p)
}