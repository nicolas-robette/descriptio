weighted.cramer <- function(x, y, weights = NULL, na.rm = FALSE) {
  
  t <- weighted.table(x, y, weights, na.rm = na.rm, digits = NULL)
  expected <- sapply(colSums(t), function(x) x*rowSums(t)/sum(t))
  chi.squared <- sum((t-expected)*(t-expected)/expected)
  cramer.v <- sqrt(chi.squared / (length(x)*(min(nrow(t),ncol(t))-1)))
  
  return(cramer.v)
}