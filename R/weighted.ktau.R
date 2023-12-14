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
  temp <- data.frame(x,weights)
  temp <- temp[order(temp$x),]
  temp <- split(temp, temp$x)
  nlignes <- sapply(temp,nrow)
  ti <- sapply(temp,function(x) sum(x$weights))
  ti <- ti[nlignes>1]
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
