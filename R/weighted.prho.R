weighted.prho <- function(x, y, weights) {
  mx <- stats::weighted.mean(x, weights)
  my <- stats::weighted.mean(y, weights)
  sx <- weighted.sd(x, weights)
  sy <- weighted.sd(y, weights)
  cov <- stats::weighted.mean((x-mx)*(y-my), weights)
  prho <- cov / (sx*sy)
  return(prho)
}