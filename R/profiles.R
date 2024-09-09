profiles <- function(X, y, weights = NULL, stat = "cprop", mar = TRUE, digits = 1) {

  if(!is.factor(y)) stop("y should be a factor.")
  if(any(!sapply(X,is.factor))) stop("All variables in X should be factors.")
  
  if(is.null(weights)) weights <- rep(1, length(y))
  if(any(is.na(weights))) stop("There are empty values in weights.")
  
  freq <- t(dichot(X, out = "numeric")) %*% diag(weights) %*% as.matrix(dichot(as.data.frame(y), out = "numeric"))
  colnames(freq) <- levels(y)
  
  if(isTRUE(mar)) {
    freq <- cbind.data.frame(freq, total = rowSums(freq))
    names <- rownames(freq)
    freq <- rbind(freq, colSums(freq) / ncol(X))
    rownames(freq) <- c(names, "total")
  }
  
  if(stat == "freq") {
    res <- freq
  } else if(stat == "rprop") {
    res <- round(100*t(apply(freq, 1, function(x) x/sum(x))), digits)
    if(isTRUE(mar)) res[,"total"] <- rep(100, nrow(res))
  } else if(stat == "cprop") {
    res <- freq
    if(isTRUE(mar)) res <- res[-nrow(res),]
    res <- round(100*apply(res, 2, function(x) x*ncol(X)/sum(x)), digits)
  } else if(stat == "prop") {
    res <- round(100*freq/sum(weights), digits)
  }

  return(res)
}  
