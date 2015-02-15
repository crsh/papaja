validate <- function(
  x
  , name = NULL
  , check_class = NULL
  , check_integer = FALSE
  , check_NA = TRUE
  , check_infinite = TRUE
  , check_length = NULL
  , check_dim = NULL
  , check_range = NULL
) {
  if(is.null(name)) name <- deparse(substitute(x))

  if(is.null(x)) stop(paste("The parameter '", name, "' is NULL.", sep=""))

  if(!is.null(check_dim) && !all(dim(x) == check_dim)) stop(paste("The parameter '", name, "' must have dimensions " , paste(check_dim, collapse=""), ".", sep=""))
  if(!is.null(check_length) && length(x) != check_length) stop(paste("The parameter '", name, "' must be of length ", check_length, ".", sep=""))

  if(any(is.na(x))) {
    if(check_NA) stop(paste("The parameter '", name, "' is NA.", sep = ""))
    else return(TRUE)
  }

  if(check_infinite && "numeric" %in% is(x) && is.infinite(x)) stop(paste("The parameter '", name, "' must be finite.", sep=""))
  if(check_integer && "numeric" %in% is(x) && x %% 1 != 0) stop(paste("The parameter '", name, "' must be an integer.", sep=""))

  for(x.class in check_class) {
    if(!is(x, x.class)) stop(paste("The parameter '", name, "' must be of class '", x.class, "'.", sep=""))
  }

  if(!is.null(check_range) && any(x < check_range[1] | x > check_range[2])) stop(paste("The parameter '", name, "' must be between ", check_range[1], " and ", check_range[2], ".", sep=""))
  TRUE
}
