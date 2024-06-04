darma <- function(y, x, weights = NULL, target = 1,
                  na.rm.cat = FALSE, na.value.cat = "NAs", na.rm.cont = FALSE,
                  correlation = "kendall",
                  nperm = NULL, distrib = "asympt", dec = c(1,3,3)) {
  
  if(is.null(weights)) weights <- rep(1, length(y))
  if(any(is.na(weights))) stop("There are empty values in weights.")

  if(is.factor(y)) {
    if(any(is.na(y))) stop("There are empty values in y. \nPlease consider transforming your data (filtering, recoding, imputation, etc.).")
  }
  
  x <- as.data.frame(x)
    
  ldf <- list()
  for(i in 1:ncol(x)) {
    if(is.factor(y) & is.factor(x[,i])) {
      if(na.rm.cat==FALSE) {
        if(any(is.na(x[,i]))) {
          x[,i] <- factor(x[,i], levels=c(levels(x[,i]), na.value.cat))
          x[,i][is.na(x[,i])] <- na.value.cat
          x[,i] <- factor(x[,i])
        }
      }
      biv <- assoc.twocat(y,x[,i],weights=weights,nperm=nperm,distrib=distrib,
                          na.rm = na.rm.cat, na.value = na.value.cat)
      pct <- biv$tables$cprop[target,1:nlevels(x[,i])]
      assoc <- biv$local$phi[target,]
      pval <- biv$local$phi.perm.pval[target,]
      if(is.null(nperm)) pval <- rep(NA,length(assoc))
      var <- c(names(x)[i], rep("",nlevels(x[,i])-1))
      mod <- names(assoc)
      ldf[[i]] <- data.frame(variable=var,category=mod,percent=pct,association=assoc,perm.pvalue=pval, stringsAsFactors = FALSE)
    }
    if(is.factor(y) & is.numeric(x[,i])) {
      biv <- assoc.catcont(y,x[,i],weights=weights,nperm=nperm,distrib=distrib,
                           na.rm.cat = na.rm.cat, na.value.cat =na.value.cat, na.rm.cont = na.rm.cont)
      pval <- biv$permutation.pvalue
      if(is.null(nperm)) pval <- NA
      ldf[[i]] <- data.frame(variable=names(x)[i],category="",percent=NA,
                             association=biv$cor[target],perm.pvalue=pval,
                             stringsAsFactors = FALSE)
    }
    if(is.numeric(y) & is.factor(x[,i])) {
      if(na.rm.cat==FALSE) {
        if(any(is.na(x[,i]))) {
          x[,i] <- factor(x[,i], levels=c(levels(x[,i]), na.value.cat))
          x[,i][is.na(x[,i])] <- na.value.cat
          x[,i] <- factor(x[,i])
        }
      }
      biv <- assoc.catcont(x[,i],y,weights=weights,nperm=nperm,distrib=distrib,
                           na.rm.cat = na.rm.cat, na.value.cat =na.value.cat, na.rm.cont = na.rm.cont)
      # med <- sapply(split(data.frame(y,weights),x[,i]), function(X) weighted.mean(X[,1],X[,2]))
      med <- round(sapply(split(data.frame(y,weights),x[,i]), function(X) weighted.quantile(X[,1],X[,2],probs=.5,
                                                                                            na.rm = na.rm.cont)),2)
      assoc <- biv$cor
      pval <- biv$cor.perm.pval
      if(is.null(nperm)) pval <- rep(NA,length(assoc))
      var <- c(names(x)[i], rep("",nlevels(x[,i])-1))
      mod <- names(assoc)
      ldf[[i]] <- data.frame(variable=var,category=mod,median=med,
                             association=assoc,perm.pvalue=pval,
                             stringsAsFactors = FALSE)
    }
    if(is.numeric(y) & is.numeric(x[,i])) {
      biv <- assoc.twocont(y,x[,i],weights=weights,nperm=nperm,distrib=distrib,
                           na.rm = na.rm.cont)
      ldf[[i]] <- data.frame(variable=names(x)[i],category="",median=NA,
                             association=biv[1,correlation],perm.pvalue=biv[2,correlation],
                             stringsAsFactors = FALSE)
    }
  }
  res <- do.call('rbind.data.frame', ldf)
  rownames(res) <- NULL
  res[,3] <- round(res[,3], dec[1])
  res$association <- round(res$association, dec[2])
  res$perm.pvalue <- round(res$perm.pvalue, dec[3])
  if(is.null(nperm)) res$perm.pvalue <- NULL
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
 
# darma(y0, data.frame(a0,b0,c0), weights = w0)
# darma(y0, data.frame(a0,b0,c0), weights = w1)
# darma(y0, data.frame(a1,b0,c0), na.rm.cont = FALSE)
# darma(y0, data.frame(a1,b0,c0), na.rm.cont = TRUE)
# darma(y0, data.frame(a0,b1,c0), na.rm.cat = FALSE, na.value.cat = "99")
# darma(y0, data.frame(a0,b1,c0), na.rm.cat = TRUE, na.value.cat = "99")
# darma(y1, data.frame(a0,b0,c0), na.rm.cont = FALSE)
# darma(y1, data.frame(a0,b0,c0), na.rm.cont = TRUE)
# 
# z0 = z1 = Movies$ArtHouse
# z1[9:10] <- NA
# darma(z0, data.frame(a0,b0,c0), weights = w0)
# darma(z0, data.frame(a0,b0,c0), weights = w1)
# darma(z0, data.frame(a1,b0,c0), na.rm.cont = FALSE)
# darma(z0, data.frame(a1,b0,c0), na.rm.cont = TRUE)
# darma(z0, data.frame(a0,b1,c0), na.rm.cat = FALSE, na.value.cat = "99")
# darma(z0, data.frame(a0,b1,c0), na.rm.cat = TRUE, na.value.cat = "99")
# darma(z1, data.frame(a0,b0,c0), na.rm.cat = FALSE)
# darma(z1, data.frame(a0,b0,c0), na.rm.cat = TRUE)
