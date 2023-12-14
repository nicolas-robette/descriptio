weighted.cor <- function(x, y, weights = NULL, method = "pearson", na.rm = FALSE) {
  if(is.null(weights)) weights <- rep(1, length(x))
  if(any(is.na(weights))) stop("There are empty values in weights.")
  if(na.rm) {
    complete <- !(is.na(x) | is.na(y))
    x <- x[complete]
    y <- y[complete]
    weights <- weights[complete]
  } else {
    if(any(is.na(x) | is.na(y))) stop("There are empty values in x, y or both. \nPlease consider transforming your data (filtering, recoding, imputation, etc.) or set na.rm to TRUE.")
  }
  if(method=="pearson") { 
    res <- weighted.prho(x,y,weights)
  } else if(method=="spearman") {
    res <- weighted.srho(x,y,weights)
  } else if(method=="kendall") {
    res <- weighted.ktau(x,y,weights)
  }
  return(res)
}
