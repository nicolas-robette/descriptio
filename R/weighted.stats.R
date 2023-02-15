
# weighted.quantile <- function(x, weights, probs = .5, method = "raw") {
#   if(method=="raw") {
#     weights <- weights[order(x)]
#     x <- x[order(x)]
#     Fx = cumsum(weights)/sum(weights)
#     rang <- max(which(Fx<probs))
#     res <- x[rang] + (0.5 - Fx[rang])/(Fx[rang+1] - Fx[rang]) * (x[rang+1] - x[rang])
#   }
#   if(method=="density") {
#     res <- with(stats::density(x, weights = weights/sum(weights), n = 4096), 
#                 x[which.max(cumsum(y*(x[2L] - x[1L])) >= probs)])
#   }
#   return(res)
# }

# https://stackoverflow.com/questions/2748725/is-there-a-weighted-median-function


weighted.quantile <- function(x, weights = NULL, probs = seq(0, 1, 0.25),
                              na.rm = FALSE, names = FALSE) {
  if (any(probs > 1) | any(probs < 0)) stop("probs are outside [0,1]")
  # if (length(weights) == 1) weights <- rep(weights, length(x))
  # if (length(weights) != length(x)) stop("weights must have length 1 or be as long as x")
  if(is.null(weights)) weights <- rep(1, length(x))
  if(any(is.na(weights))) stop("There are empty values in weights.")
  if(na.rm) {
    complete <- !is.na(x)
    x <- x[complete]
    weights <- weights[complete]
  } else {
    if(any(is.na(x))) stop("There are empty values in x. \nPlease consider transforming your data (filtering, recoding, imputation, etc.) or set na.rm to TRUE.")
  }
  weights <- weights[order(x)] / sum(weights)
  x <- x[order(x)]
  cum_w <- cumsum(weights) - weights * (1 - (seq_along(weights) - 1) / (length(weights) - 1))
  res <- stats::approx(x = cum_w, y = x, xout = probs)$y
  if (isTRUE(names)) res <- setNames(res, paste0(format(100 * probs, digits = 7), "%"))
  return(res)
}


weighted.mad <- function(x, weights = NULL, na.rm = FALSE) {
  if(is.null(weights)) weights <- rep(1, length(x))
  if(any(is.na(weights))) stop("There are empty values in weights.")
  if(na.rm) {
    complete <- !is.na(x)
    x <- x[complete]
    weights <- weights[complete]
  } else {
    if(any(is.na(x))) stop("There are empty values in x. \nPlease consider transforming your data (filtering, recoding, imputation, etc.) or set na.rm to TRUE.")
  }
  med <- weighted.quantile(x=x, weights=weights, probs = .5)
  ad <- abs(x-med)
  mad <- weighted.quantile(x=ad, weights=weights, probs = .5)
  return(mad)
}


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


# x0 <- c(1,3,4,12,3,3,6)
# y0 <- c(34,87,35,12,23,98,56)
# w0 <- c(2,1.5,2,1.5,1,1.5,2)
# x1 <- c(1,NA,4,12,3,3,6)
# y1 <- c(34,87,35,12,23,98,NA)
# w1 <- c(NA,1.5,2,1.5,1,1.5,2)

# weighted.cor(x0, y0, w0, na.rm = FALSE)
# weighted.cor(x0, y0, w0, na.rm = TRUE)
# weighted.cor(x0, y0, w1, na.rm = FALSE)
# weighted.cor(x0, y0, w1, na.rm = TRUE)
# weighted.cor(x1, y0, w0, na.rm = FALSE)
# weighted.cor(x1, y0, w0, na.rm = TRUE)
# weighted.cor(x0, y1, w0, na.rm = FALSE)
# weighted.cor(x0, y1, w0, na.rm = TRUE)
# weighted.cor(x1, y1, w0, na.rm = FALSE)
# weighted.cor(x1, y1, w0, na.rm = TRUE)

# assoc.twocont(x0, y0, w0, na.rm = FALSE)
# assoc.twocont(x0, y0, w0, na.rm = TRUE)
# assoc.twocont(x0, y0, w1, na.rm = FALSE)
# assoc.twocont(x0, y0, w1, na.rm = TRUE)
# assoc.twocont(x1, y0, w0, na.rm = FALSE)
# assoc.twocont(x1, y0, w0, na.rm = TRUE)
# assoc.twocont(x0, y1, w0, na.rm = FALSE)
# assoc.twocont(x0, y1, w0, na.rm = TRUE)
# assoc.twocont(x1, y1, w0, na.rm = FALSE)
# assoc.twocont(x1, y1, w0, na.rm = TRUE)

# weighted.sd(x0, w0, na.rm = FALSE)
# weighted.sd(x0, w0, na.rm = TRUE)
# weighted.sd(x0, w1, na.rm = FALSE)
# weighted.sd(x0, w1, na.rm = TRUE)
# weighted.sd(x1, w0, na.rm = FALSE)
# weighted.sd(x1, w0, na.rm = TRUE)

# weighted.quantile(x0, w0, na.rm = FALSE)
# weighted.quantile(x0, w0, na.rm = TRUE)
# weighted.quantile(x0, w1, na.rm = FALSE)
# weighted.quantile(x0, w1, na.rm = TRUE)
# weighted.quantile(x1, w0, na.rm = FALSE)
# weighted.quantile(x1, w0, na.rm = TRUE, names = TRUE)

# weighted.mad(x0, w0, na.rm = FALSE)
# weighted.mad(x0, w0, na.rm = TRUE)
# weighted.mad(x0, w1, na.rm = FALSE)
# weighted.mad(x0, w1, na.rm = TRUE)
# weighted.mad(x1, w0, na.rm = FALSE)
# weighted.mad(x1, w0, na.rm = TRUE)
