weighted.cov2 <- function(x, y = NULL, weights = NULL, na.rm = FALSE) {
  if (is.null(weights)) weights <- rep(1, nrow(x))
  if (any(is.na(weights))) stop("There are empty values in weights.")
  if(is.null(y)) {
    sapply(x, function(z) sapply(x, weighted.cov, y = z, weights = weights, na.rm = na.rm))
  } else {
    sapply(y, function(z) sapply(x, weighted.cov, y = z, weights = weights, na.rm = na.rm))
  }}
