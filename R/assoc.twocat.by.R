assoc.twocat.by  <- function(x, y, by, weights = NULL, na.rm = FALSE, na.value = "NAs", nperm = NULL, distrib = "asympt") {

  df <- data.frame(x,y)
  spl <- split(df, by)
  ass <- lapply(spl, function(z) with(z, assoc.twocat(x,y,weights = weights,
                                                      na.rm=na.rm,na.value=na.value,
                                                      nperm=nperm,distrib=distrib)))

  res <- list()
  res$tables$freq <- lapply(ass, function(x) x$tables$freq)
  res$tables$prop <- lapply(ass, function(x) x$tables$prop)
  res$tables$rprop <- lapply(ass, function(x) x$tables$rprop)
  res$tables$cprop <- lapply(ass, function(x) x$tables$cprop)
  res$tables$expected <- lapply(ass, function(x) x$tables$expected)

  res$global$chi.squared <- lapply(ass, function(x) x$global$chi.squared)
  res$global$cramer.v <- lapply(ass, function(x) x$global$cramer.v)
  res$global$permutation.pvalue <- lapply(ass, function(x) x$global$permutation.pvalue)
  res$global$global.pem <- lapply(ass, function(x) x$global$global.pem)
  res$global$GK.tau.xy <- lapply(ass, function(x) x$global$GK.tau.xy)
  res$global$GK.tau.yx <- lapply(ass, function(x) x$global$GK.tau.yx)
  
  res$local$std.residuals <- lapply(ass, function(x) x$local$std.residuals)
  res$local$adj.residuals <- lapply(ass, function(x) x$local$adj.residuals)
  res$local$adj.res.pval <- lapply(ass, function(x) x$local$adj.res.pval)
  res$local$odss.ratios <- lapply(ass, function(x) x$local$odss.ratios)
  res$local$local.pem <- lapply(ass, function(x) x$local$local.pem)
  res$local$phi <- lapply(ass, function(x) x$local$phi)
  res$local$phi.perm.pval <- lapply(ass, function(x) x$local$phi.perm.pval)
  
  res$gather <- lapply(ass, function(x) x$gather)
    
  return(res)
}
