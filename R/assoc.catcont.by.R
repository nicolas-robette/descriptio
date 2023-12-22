assoc.catcont.by  <- function(x, y, by, weights = NULL, 
                              na.rm.cat = FALSE, na.value.cat = "NA", na.rm.cont = FALSE,
                              nperm = NULL, distrib = "asympt", digits = 3) {

  df <- data.frame(x,y)
  spl <- split(df, by)
  ass <- lapply(spl, function(z) with(z, assoc.catcont(x,y,weights = weights,
                                                       na.rm.cat=na.rm.cat,
                                                       na.value.cat=na.value.cat,
                                                       na.rm.cont=na.rm.cont,
                                                       nperm=nperm,distrib=distrib,digits=digits)))

  res <- list()
  res$summary <- lapply(ass, function(x) x$summary)
  res$eta.squared <- lapply(ass, function(x) x$eta.squared)
  res$permutation.pvalue <- lapply(ass, function(x) x$permutation.pvalue)
  res$cor <- lapply(ass, function(x) x$cor)
  res$cor.perm.pval <- lapply(ass, function(x) x$cor.perm.pval)
  res$test.values <- lapply(ass, function(x) x$test.values)
  res$test.values.pval <- lapply(ass, function(x) x$test.values.pval)
  return(res)
}
