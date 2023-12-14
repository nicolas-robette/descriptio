weighted.srho <- function(x, y, weights, ties.method = "average") {
  rx <- rank(x, na.last = "keep", ties.method)
  ry <- rank(y, na.last = "keep", ties.method)
  srho <- weighted.prho(rx, ry, weights)
  return(srho)
}