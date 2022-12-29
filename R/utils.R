
weighted.quantile <- function(x, w, probs = .5, method = "raw") {
  if(method=="raw") {
    w <- w[order(x)]
    x <- x[order(x)]
    Fx = cumsum(w)/sum(w)
    rang <- max(which(Fx<probs))
    res <- x[rang] + (0.5 - Fx[rang])/(Fx[rang+1] - Fx[rang]) * (x[rang+1] - x[rang])
  }
  if(method=="density") {
    res <- with(density(x, weights = w/sum(w), n = 4096), 
                x[which.max(cumsum(y*(x[2L] - x[1L])) >= probs)])
  }
  return(res)
}

weighted.mad <- function(x, w, method="raw") {
  med <- weighted.quantile(x=x, w=w, method=method)
  ad <- abs(x-med)
  mad <- weighted.quantile(x=ad, w=w, method=method)
  return(mad)
}

weighted.sd <- function(x, w) {
  xm <- weighted.mean(x, w)
  var <- weighted.mean((x-xm)^2, w)
  sd <- sqrt(var)
  return(sd)
}

lag1 <- function(x, default = 0) {
  return(c(default, x[1:(length(x)-1)]))
  }
  
dichot <- function(data,out='numeric') {
  # if(!is.data.frame(data)) data <- data.frame(data)
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
  res
}
