condesc <- function(y, x, weights = NULL, 
                    na.rm.cat = FALSE, na.value.cat = "NA", na.rm.cont = FALSE,
                    min.cor = NULL, robust = TRUE, 
                    nperm = NULL, distrib = "asympt", dec = c(3,3,0,3)) {

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
  
  cor <- numeric()
  median.y.in.xcat <- numeric()
  median.y.global <- numeric()
  mad.y.in.xcat <- numeric()
  mad.y.global <- numeric()
  for(i in 1:ncol(xcat.dic)) {
    cor[i] <- weighted.cor(y, xcat.dic[,i], weights=weights, method="pearson", na.rm = na.rm.cont)
    if(robust==TRUE) {
      median.y.in.xcat[i] <- weighted.quantile(rep(y[xcat.dic[,i]==1],2), weights=rep(weights[xcat.dic[,i]==1],2), probs = .5, na.rm = na.rm.cont)
      median.y.global[i] <- weighted.quantile(y, weights=weights, probs = .5, na.rm = na.rm.cont)
      mad.y.in.xcat[i] <- weighted.mad(rep(y[xcat.dic[,i]==1],2), weights=rep(weights[xcat.dic[,i]==1],2), na.rm = na.rm.cont)
      mad.y.global[i] <- weighted.mad(y, weights=weights, na.rm = na.rm.cont)
    }
    if(robust==FALSE) {
      median.y.in.xcat[i] <- weighted.mean(rep(y[xcat.dic[,i]==1],2), weights=rep(weights[xcat.dic[,i]==1],2))
      median.y.global[i] <- weighted.mean(y, weights=weights)
      mad.y.in.xcat[i] <- weighted.sd(rep(y[xcat.dic[,i]==1],2), weights=rep(weights[xcat.dic[,i]==1],2))
      mad.y.global[i] <- weighted.sd(y, weights=weights)      
    }
  }
  
  categories <- data.frame(categories=names(xcat.dic),
                           median.y.in.xcat=round(median.y.in.xcat,dec[3]),
                           median.y.global=round(median.y.global,dec[3]),
                           mad.y.in.xcat=round(mad.y.in.xcat,dec[3]),
                           mad.y.global=round(mad.y.global,dec[3]),
                           cor=round(cor,dec[4]))
  if(robust==FALSE) names(categories) <- c("categories","mean.y.in.xcat","mean.y.global",
                                           "sd.y.in.xcat","sd.y.global","cor")
  categories <- categories[order(-categories$cor),]
  if(!is.null(min.cor)) categories <- categories[abs(categories$cor)>=min.cor,]
  
  res <- list(variables=assoc.yx(y,x,xx=FALSE,weights=weights,
                                 na.rm.cat = na.rm.cat, na.value.cat = na.value.cat, na.rm.cont = na.rm.cont,
                                 nperm=nperm,distrib=distrib,dec=dec[1:2])$YX,
              categories=categories)
  return(res)
}


# data(Movies)
# y0 = y1 = Movies$BoxOffice
# a0 = a1 = Movies$Budget
# b0 = b1 = Movies$Genre
# c0 = c1 = Movies$Country
# w0 = w1 = Movies$Critics
# w1[1:2] <- NA
# a1[3:4] <- NA
# b1[5:6] <- NA
# c1[7:8] <- NA
# y1[9:10] <- NA
# 
# condesc(y0, data.frame(a0,b0,c0), weights = w0)
# condesc(y0, data.frame(a0,b0,c0), weights = w1)
# condesc(y0, data.frame(a0,b0,c0), weights = w0, robust = FALSE)
# condesc(y0, data.frame(a1,b0,c0), na.rm.cont = FALSE)
# condesc(y0, data.frame(a1,b0,c0), na.rm.cont = TRUE)
# condesc(y0, data.frame(a0,b1,c0), na.rm.cat = FALSE, na.value.cat = "99", robust = FALSE)
# condesc(y0, data.frame(a0,b1,c0), na.rm.cat = TRUE, na.value.cat = "99", robust = FALSE)
# condesc(y1, data.frame(a0,b0,c0), na.rm.cont = FALSE, robust = FALSE)
# condesc(y1, data.frame(a0,b0,c0), na.rm.cont = TRUE, robust = FALSE)
