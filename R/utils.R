lag1 <- function(x, default = 0) {
  return(c(default, x[1:(length(x)-1)]))
  }
  

dichot <- function(data, out='numeric') {
  data <- as.data.frame(data)
  res <- matrix(nrow=nrow(data),ncol=length(levels(data[,1])))
  for(i in 1:ncol(data)) {
    if(is.factor(data[,i])==FALSE) data[,i] <- factor(data[,i])
    nlevels <- length(levels(data[,i]))
    temp <- matrix(nrow=nrow(data),ncol=nlevels)
    for(j in 1:nlevels) {
      temp[,j] <- 0
      temp[data[,i]==levels(data[,i])[j] ,j] <- 1
    }
    colnames(temp) <- paste(names(data)[i],levels(data[,i]),sep=".")
    if(i==1) res <- temp else res <- cbind(res,temp)
    }
  res <- as.data.frame(res)
  if(out=='factor') for(i in 1:ncol(res)) res[,i] <- as.factor(res[,i])
  return(res)
}


test.values <- function(x, y, weights = NULL) {
  if(is.null(weights)) weights <- rep(1, length(x))
  ww <- length(weights)*weights/sum(weights)
  N <- length(x)
  moyennes <- sapply(split(data.frame(y,ww), x), function(x) weighted.mean(x[,1],x[,2]))
  effectifs <- as.numeric(weighted.table(x, weights = ww))
  moy <- weighted.mean(y, ww)
  dev <- weighted.sd(y, ww)
  deviations <- sqrt(sapply(effectifs, function(x) (N-x)/(N-1)*dev*dev/x))
  res <- (moyennes-moy)/deviations
  return(res)
}
