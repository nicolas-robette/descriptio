weighted.cor2 <- function(x, y  = NULL, weights = NULL, method = "pearson", na.rm = FALSE) {
  if (is.null(weights)) weights <- rep(1, nrow(x))
  if (any(is.na(weights))) stop("There are empty values in weights.")
  if(is.null(y)) {
    sapply(x, function(z) sapply(x, weighted.cor, y = z, weights = weights, method = method, na.rm = na.rm))
  } else {
    sapply(y, function(z) sapply(x, weighted.cor, y = z, weights = weights, method = method, na.rm = na.rm))
  }
}
