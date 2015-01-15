validate.numeric <- function(x, name, type="number") {
  if(any(is.na(x))) stop(paste("The parameter '", name, "' is NA", sep=""))
  if(any(is.infinite(x))) stop(paste("The parameter '", name, "' must be finite", sep=""))
  if(!is(x, "numeric") || length(x) != 1 || (type == "integer" && x %% 1 != 0)) stop(paste("The parameter '", name, "' must be a single ", type, sep=""))
}

validate.character <- function(x, name, vector.length=1) {
  if(any(is.na(x))) stop(paste("The parameter '", name, "' is NA", sep=""))
  if(!is(x, "character") || !(length(x) %in% vector.length)) stop(paste("The parameter '", name, "' must be a ", ifelse(length(vector.length) == 1 && vector.length == 1, "single", paste0("vector of ", paste(vector.length, collapse=" or "))), " character string", if(any(vector.length > 1)) "s", sep=""))
}

validate.numeric.range <- function(x, name, lower, upper) {
  if(x < lower || x > upper) stop(paste("The parameter '", name, "' must be a single number between ", lower, " and ", upper, sep=""))
}

validate.logical <- function(x, name) {
  if(any(is.na(x))) stop(paste("The parameter '", name, "' is NA", sep=""))
  if(!is(x, "logical") || length(x) != 1) stop(paste("The parameter '", name, "' must be a single logical value", sep=""))
}
