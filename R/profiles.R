# X <- Movies[,c(2,4,5)]
# y <- Movies$Country
# weights <- rep(1, nrow(X))

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
    res <- round(100*apply(freq, 2, function(x) x*ncol(X)/sum(x)), digits)
    if(isTRUE(mar)) res <- res[-nrow(res),]
  } else if(stat == "prop") {
    res <- round(100*freq/sum(weights), digits)
  }

  return(res)
}  

# View(profiles(musique[,c(13,15:19)], musique$age))
# profiles(musique[,c(13,15:19)], musique$age, digits = 3)
# profiles(musique[,c(13,15:19)], musique$age, stat = "freq")
# profiles(musique[,c(13,15:19)], musique$age, stat = "prop")
# profiles(musique[,c(13,15:19)], musique$age, stat = "rprop")
# 
# profiles(Movies[,c(2,4,5)], Movies$Country, digits = 3)
# profiles(Movies[,c(2,4,5)], Movies$Country, stat = "freq")
# profiles(Movies[,c(2,4,5)], Movies$Country, stat = "prop")
# profiles(Movies[,c(2,4,5)], Movies$Country, stat = "rprop")
# profiles(Movies[,c(2,4,5)], Movies$Country, digits = 3, mar = FALSE)
# profiles(Movies[,c(2,4,5)], Movies$Country, stat = "freq", mar = FALSE)
# profiles(Movies[,c(2,4,5)], Movies$Country, stat = "prop", mar = FALSE)
# profiles(Movies[,c(2,4,5)], Movies$Country, stat = "rprop", mar = FALSE)

