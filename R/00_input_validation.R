validate <- function(x, name, check.class=NULL, check.integer=FALSE, check.NA=TRUE, check.infinite=TRUE, check.length=NULL, check.dim=NULL, check.range=NULL) {
  if(is.null(x)) stop(paste("The parameter '", name, "' is NULL.", sep=""))

  if(!is.null(check.dim) && !all(dim(x) == check.dim)) stop(paste("The parameter '", name, "' must have dimensions " , paste(check.dim, collapse=""), ".", sep=""))
  if(!is.null(check.length) && length(x) != check.length) stop(paste("The parameter '", name, "' must be of length ", check.length, ".", sep=""))

  if(is.na(x)) {
    if(check.NA) stop(paste("The parameter '", name, "' is NA.", sep=""))
    else return(TRUE)
  }

  if(check.infinite && "numeric" %in% is(x) && is.infinite(x)) stop(paste("The parameter '", name, "' must be finite.", sep=""))
  if(check.integer && "numeric" %in% is(x) && x %% 1 != 0) stop(paste("The parameter '", name, "' must be an integer.", sep=""))

  for(x.class in check.class) {
    if(!is(x, x.class)) stop(paste("The parameter '", name, "' must be of class '", x.class, "'.", sep=""))
  }

  if(!is.null(check.range) && any(x < check.range[1] | x > check.range[2])) stop(paste("The parameter '", name, "' must be between ", check.range[1], " and ", check.range[2], ".", sep=""))
  TRUE
}
