
weighted.quantile <- function(x, weights, probs = .5, method = "raw") {
  if(method=="raw") {
    weights <- weights[order(x)]
    x <- x[order(x)]
    Fx = cumsum(weights)/sum(weights)
    rang <- max(which(Fx<probs))
    res <- x[rang] + (0.5 - Fx[rang])/(Fx[rang+1] - Fx[rang]) * (x[rang+1] - x[rang])
  }
  if(method=="density") {
    res <- with(stats::density(x, weights = weights/sum(weights), n = 4096), 
                x[which.max(cumsum(y*(x[2L] - x[1L])) >= probs)])
  }
  return(res)
}


weighted.mad <- function(x, weights, method = "raw") {
  med <- weighted.quantile(x=x, weights=weights, method=method)
  ad <- abs(x-med)
  mad <- weighted.quantile(x=ad, weights=weights, method=method)
  return(mad)
}


weighted.sd <- function(x, weights) {
  mx <- stats::weighted.mean(x, weights)
  var <- stats::weighted.mean((x-mx)^2, weights)
  sd <- sqrt(var)
  return(sd)
}


weighted.prho <- function(x, y, weights) {
  mx <- stats::weighted.mean(x, weights)
  my <- stats::weighted.mean(y, weights)
  sx <- weighted.sd(x, weights)
  sy <- weighted.sd(y, weights)
  cov <- stats::weighted.mean((x-mx)*(y-my), weights)
  prho <- cov / (sx*sy)
  return(prho)
}


weighted.srho <- function(x, y, weights, ties.method = "average") {
  rx <- rank(x, na.last = "keep", ties.method)
  ry <- rank(y, na.last = "keep", ties.method)
  srho <- weighted.prho(rx, ry, weights)
  return(srho)
}


# x <- c(1,5,4,12,3,3,6)
# y <- c(34,87,35,12,23,98,3)
# w <- c(1,1.5,2,1.5,1,1.5,2)
# w <- rep(1,7)
# cor(x, y, method = "kendall")
# wdm::wdm(x, y, method = "kendall", weights = w)
# weighted.ktau(x,y,w)
# wdm::wdm(x, y, method = "pearson", weights = w)
# weighted.cor(x,y,w, method = "pearson")
# wdm::wdm(x, y, method = "spearman", weights = w)
# weighted.cor(x,y,w, method = "spearman")
# wdm::wdm(x, y, method = "kendall", weights = w)
# weighted.cor(x,y,w, method = "kendall")

weighted.ktau <- function(x, y, weights) {
  weights <- weights*length(weights)/sum(weights)
  combi <- utils::combn(1:length(x), 2)
  i1 <- combi[1,]
  i2 <- combi[2,]
  x1 <- x[i1]
  x2 <- x[i2]
  y1 <- y[i1]
  y2 <- y[i2]
  w1 <- weights[i1]
  w2 <- weights[i2]
  ww <- w1*w2
  ww <- ww*length(ww)/sum(ww)
  C <- (x1<x2 & y1<y2) | (x1>x2 & y1>y2)
  D <- (x1<x2 & y1>y2) | (x1>x2 & y1<y2)
  # T <- x1==x2 | y1==y2
  # ktau <- weighted.mean((C-D)[!T], ww[!T])
  # ti <- as.numeric(table(x))
  # ti <- ti[ti>1]
  temp <- data.frame(x,weights)
  temp <- temp[order(temp$x),]
  temp <- split(temp, temp$x)
  nlignes <- sapply(temp,nrow)
  ti <- sapply(temp,function(x) sum(x$weights))
  ti <- ti[nlignes>1]
  # uj <- as.numeric(table(y))
  # uj <- uj[uj>1]
  temp <- data.frame(y,weights)
  temp <- temp[order(temp$y),]
  temp <- split(temp, temp$y)
  nlignes <- sapply(temp,nrow)
  uj <- sapply(temp,function(x) sum(x$weights))
  uj <- uj[nlignes>1]
  n1 <- 0.5*(sum(ti*(ti-1)))
  n2 <- 0.5*(sum(uj*(uj-1)))
  n <- length(x)
  n0 <- 0.5*n*(n-1)
  nc <- sum(ww*C)
  nd <- sum(ww*D)
  ktau <- (nc-nd) / sqrt((n0-n1)*(n0-n2))
  return(ktau)
}

# x <- c(1,NA,4,12,3,3,6)
# y <- c(34,87,35,12,23,98,NA)
# weights <- c(NA,1.5,2,1.5,1,1.5,2)

weighted.cor <- function(x, y, weights, method = "pearson", remove_missing = TRUE) {
  if(remove_missing) {
    complete <- !(is.na(x) | is.na(y) | is.na(weights))
    x <- x[complete]
    y <- y[complete]
    weights <- weights[complete]
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


lag1 <- function(x, default = 0) {
  return(c(default, x[1:(length(x)-1)]))
  }
  

dichot <- function(data, out='numeric') {
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
  return(res)
}
