assoc.twocont.by <- function(x, y, by, weights = NULL, na.rm = FALSE, nperm = NULL, distrib = "asympt") {
  df <- data.frame(x,y)
  spl <- split(df, by)
  res <- lapply(spl, function(z) with(z, assoc.twocont(x,y,weights = weights,na.rm=na.rm,nperm=nperm,distrib=distrib)))
  return(res)
}
