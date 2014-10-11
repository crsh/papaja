printnumber <- function(x, digits = 2, gt1 = TRUE, zero = TRUE) {
  if(sign(x) == -1) {
    xsign <- "-"
    lt <- "> "
  } else {
    xsign <- ""
    lt <- "< "
  }
  x_out <- round(x, digits)
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


printnum <- function(x, ...) {
  if(is.matrix(x)) {
    x_out <- matrix(mapply(printnumber, x, ...), ncol = ncol(x))
  } else if(is.vector(x)) {
    x_out <- sapply(x, printnumber, ...)
  } else {
    x_out <- printnumber(x, ...)
  }
  return(x_out)
}
