assoc.twocont <- function(x, y, weights = NULL, na.rm = FALSE, nperm = NULL, distrib = "asympt") {
  
  if(is.null(weights)) weights <- rep(1, length(x))
  if(any(is.na(weights))) stop("There are empty values in weights.")
  
  pearson <- weighted.cor(x,y,method="pearson",weights=weights,na.rm=na.rm)
  spearman <- weighted.cor(x,y,method="spearman",weights=weights,na.rm=na.rm)
  kendall <- weighted.cor(x,y,method="kendall",weights=weights,na.rm=na.rm)
  
  if(!is.null(nperm)) {
    h0P <- numeric()
    h0S <- numeric()
    h0K <- numeric()
    for(i in 1:nperm) {
      permy <- sample(y)
      h0P[i] <- weighted.cor(x,permy,method="pearson",weights=weights,na.rm=na.rm)
      h0S[i] <- weighted.cor(x,permy,method="spearman",weights=weights,na.rm=na.rm)
      h0K[i] <- weighted.cor(x,permy,method="kendall",weights=weights,na.rm=na.rm)
    }
    if(distrib=='approx') {
      pearson <- c(pearson, sum(pearson<=h0P)/nperm)
      spearman <- c(spearman, sum(spearman<=h0S)/nperm)
      kendall <- c(kendall, sum(kendall<=h0K)/nperm)
    } else {
      fit <- MASS::fitdistr(h0P,"normal")$estimate
      if(pearson>=0) { 
        pearson <- c(pearson, 1-stats::pnorm(pearson,fit[1],fit[2]))
      } else {
        pearson <- c(pearson, stats::pnorm(pearson,fit[1],fit[2]))
      }
      fit <- MASS::fitdistr(h0S,"normal")$estimate
      if(spearman>=0) {
        spearman <- c(spearman, 1-stats::pnorm(spearman,fit[1],fit[2]))
      } else {
        spearman <- c(spearman, stats::pnorm(spearman,fit[1],fit[2]))
      }  
      fit <- MASS::fitdistr(h0K,"normal")$estimate
      if(kendall>=0) {
        kendall <- c(kendall, 1-stats::pnorm(kendall,fit[1],fit[2]))
      } else {
        kendall <- c(kendall, stats::pnorm(kendall,fit[1],fit[2]))
      }
    }
  }
  
  res <- data.frame(pearson,spearman,kendall)
  rownames(res)[1] <- "value"
  if(!is.null(nperm)) rownames(res)[2] <- "permutation.pvalue"
  return(res)
}
