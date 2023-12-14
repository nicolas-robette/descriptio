weighted.sd <- function(x, weights = NULL, na.rm = FALSE) {
  if(is.null(weights)) weights <- rep(1, length(x))
  if(any(is.na(weights))) stop("There are empty values in weights.")
  if(na.rm) {
    complete <- !is.na(x)
    x <- x[complete]
    weights <- weights[complete]
  } else {
    if(any(is.na(x))) stop("There are empty values in x. \nPlease consider transforming your data (filtering, recoding, imputation, etc.) or set na.rm to TRUE.")
  }
  mx <- stats::weighted.mean(x, weights)
  var <- stats::weighted.mean((x-mx)^2, weights)
  sd <- sqrt(var)
  return(sd)
}
