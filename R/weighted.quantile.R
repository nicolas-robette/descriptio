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
  if(length(x)==1) {
    res <- rep(x, length(probs)) 
  } else {
    res <- stats::approx(x = cum_w, y = x, xout = probs)$y
  }
  if (isTRUE(names)) res <- setNames(res, paste0(format(100 * probs, digits = 7), "%"))
  return(res)
}
