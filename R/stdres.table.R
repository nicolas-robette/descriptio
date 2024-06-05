stdres.table <- function(x, y, weights = NULL, na.rm = FALSE, na.value = "NAs", digits = 3, residuals = "std") {
  
  t <- weighted.table(x, y, weights, na.rm = na.rm, na.value = "NAs", digits = NULL)
  temp <- suppressWarnings(stats::chisq.test(t))
  std <- as.table(temp$residuals)
  adj <- as.table(temp$stdres)

  if(residuals == "std") {
    return(round(std, digits))
  } else if(residuals == "adj") {
    return(round(adj, digits))
  }
  
}