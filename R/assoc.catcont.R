assoc.catcont <- function(x, y, weights = NULL, 
                          na.rm.cat = FALSE, na.value.cat = "NA", na.rm.cont = FALSE,
                          nperm = NULL, distrib = "asympt", digits = 3) {

  if(is.null(weights)) weights <- rep(1, length(x))
  if(any(is.na(weights))) stop("There are empty values in weights.")
  
  if(na.rm.cont) {
    complete <- !is.na(y)
    x <- factor(x[complete])
    y <- y[complete]
    weights <- weights[complete]
  } else {
    if(any(is.na(y))) stop("There are empty values in y. \nPlease consider transforming your data (filtering, recoding, imputation, etc.) or set na.rm.cont to TRUE.")
  }
  
  if(na.rm.cat==FALSE) {
    x <- factor(x, levels=c(levels(x), na.value.cat))
    x[is.na(x)] <- na.value.cat
    x <- factor(x)
  } else {
    complete <- !is.na(x)
    x <- factor(x[complete])
    y <- y[complete]
    weights <- weights[complete]
  }
  
  eta.squared <- summary.lm(aov(y~x, weights = weights))$r.squared
  
  if(!is.null(nperm)) {
    h0distrib <- numeric()
    for(i in 1:nperm) h0distrib[i] <- summary.lm(aov(sample(y)~x,weights=weights))$r.squared
    if(distrib=='approx') {
      permutation.pvalue <- sum(eta.squared<=h0distrib)/nperm
    } else {
      fit <- MASS::fitdistr(h0distrib,"normal")$estimate
      permutation.pvalue <- 1-stats::pnorm(eta.squared,fit[1],fit[2])   
    }
  }
  if(is.null(nperm)) permutation.pvalue <- NULL
  
  cor.coeff <- numeric(length=nlevels(x))
  for(i in 1:nlevels(x)) cor.coeff[i] <- weighted.cor(as.numeric(x==levels(x)[i]), y, 
                                                      method="pearson", weights=weights)
  names(cor.coeff) <- levels(x)
  
  if(!is.null(nperm)) {
    ppval <- numeric(length=nlevels(x))
    for(i in 1:nlevels(x)) {
      obs <- cor.coeff[i]
      h0d <- numeric()
      for(j in 1:nperm) h0d[j] <- weighted.cor(as.numeric(x==levels(x)[i]), sample(y), 
                                               method="pearson", weights=weights)
      if(distrib=='approx') {
        if(obs>=0) ppval[i] <- sum(obs<=h0d)/nperm
        if(obs<0) ppval[i] <- sum(obs>h0d)/nperm
      } else {
        fit <- MASS::fitdistr(h0d,"normal")$estimate
        if(obs>=0) ppval[i] <- 1-stats::pnorm(obs,fit[1],fit[2])
        if(obs<0) ppval[i] <- stats::pnorm(obs,fit[1],fit[2])
      }
    }
    names(ppval) <- levels(x)
  }
  if(is.null(nperm)) ppval <- NULL

  cor.coeff <- round(cor.coeff,digits)
  return(list('eta.squared'=eta.squared, 'permutation.pvalue'=permutation.pvalue, 'cor'=cor.coeff, 'cor.perm.pval'=ppval))
}

# x0 <- x1 <- Movies$Country
# y0 <- y1 <- Movies$BoxOffice
# w0 <- w1 <- Movies$Critics
# w1[c(1,3)] <- NA
# x1[c(2,4)] <- NA
# y1[c(5,6)] <- NA
# assoc.catcont(x0, y = y0, weights = w0, na.rm.cat = FALSE, na.value.cat = "99")
# assoc.catcont(x0, y0, weights = w0, na.rm.cat = TRUE, na.value.cat = "99")
# assoc.catcont(x0, y0, weights = w1, na.rm.cat = FALSE, na.value.cat = "99")
# assoc.catcont(x0, y0, weights = w1, na.rm.cat = TRUE, na.value.cat = "99")
# assoc.catcont(x1, y0, weights = w0, na.rm.cat = FALSE, na.value.cat = "99")
# assoc.catcont(x1, y0, weights = w0, na.rm.cat = TRUE, na.value.cat = "99")
# assoc.catcont(x0, y1, weights = w0, na.rm.cont = FALSE)
# assoc.catcont(x0, y1, weights = w0, na.rm.cont = TRUE)
