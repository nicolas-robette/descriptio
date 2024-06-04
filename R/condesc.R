condesc <- function(y, x, weights = NULL, 
                    na.rm.cat = FALSE, na.value.cat = "NAs", na.rm.cont = FALSE,
                    limit = NULL, correlation = "kendall", robust = TRUE, 
                    nperm = NULL, distrib = "asympt", digits = 2) {

  if(is.null(weights)) weights <- rep(1, length(y))
  if(any(is.na(weights))) stop("There are empty values in weights.")

  if(na.rm.cont==TRUE) {
    if(any(is.na(y))) {
      complete <- !is.na(y)
      x <- x[complete,]
      weights <- weights[complete]
      y <- y[complete]
    }
  }
    
  icat <- which(sapply(x,is.factor))
  xcat <- as.data.frame(x[,icat])
  names(xcat) <- names(x)[icat]
  for(i in 1:ncol(xcat)) xcat[,i] <- factor(xcat[,i])
  
  if(na.rm.cat==FALSE) {
    for(i in 1:ncol(xcat)) {
      if(any(is.na(xcat[,i]))) {
        xcat[,i] <- factor(xcat[,i], levels=c(levels(xcat[,i]), na.value.cat))
        xcat[is.na(xcat[,i]),i] <- na.value.cat
        xcat[,i] <- factor(xcat[,i])
      }
    }
  }
  
  xcat.dic <- dichot(xcat)

  overall.median <- round(weighted.quantile(y, weights = weights, probs = .5, na.rm = na.rm.cont), digits)
  overall.mad <- round(weighted.mad(y, weights = weights, na.rm = na.rm.cont), digits)
  overall.mean <- round(weighted.mean(y, weights = weights), digits)
  overall.sd <- round(weighted.sd(y, weights = weights), digits)
  
  categories <- list()
  for(i in 1:ncol(xcat)) {
    asso <- assoc.catcont(xcat[,i], y, weights = weights,
                          na.rm.cat = na.rm.cat, na.value.cat = na.value.cat, na.rm.cont = FALSE,
                          nperm = nperm, distrib = distrib)
    categories[[i]] <- data.frame(asso$summary[,c("mean","sd","median","mad")], correlation = asso$cor)
    if(!is.null(nperm)) categories[[i]] <- data.frame(categories[[i]], pvalue = round(asso$cor.perm.pval,5))
  }
  categories <- do.call("rbind.data.frame", categories)
  categories <- data.frame(categories, overall.median, overall.mad, overall.mean, overall.sd)
  categories$categories <- names(xcat.dic)
  names(categories)[1:4] <- paste0(names(categories)[1:4], ".in.category")
  for(i in 1:4) categories[,i] <- round(categories[,i], digits)
  
  if(robust==FALSE) {
    noms <- c("categories","mean.in.category","overall.mean", "sd.in.category","overall.sd","correlation")
  } else if(robust==TRUE) {
    noms <- c("categories","median.in.category","overall.median", "mad.in.category","overall.mad","correlation")
  }
  if(!is.null(nperm)) noms <- c(noms, "pvalue")
  categories <- categories[, noms]

  categories <- categories[order(-categories$correlation),]
  
  if(!is.null(limit)) categories <- categories[abs(categories$correlation)>=limit,]
  rownames(categories) <- NULL
  
  v <- assoc.yx(y, x, xx = FALSE, weights = weights, correlation = correlation,
                na.rm.cat = na.rm.cat, na.value.cat = na.value.cat, na.rm.cont = na.rm.cont,
                nperm = nperm, distrib = distrib, dec = c(3,5))$YX
  if(!is.null(nperm)) names(v)[4] <- "pvalue"
  
  res <- list(variables = v,
              categories = categories)
  return(res)
}
