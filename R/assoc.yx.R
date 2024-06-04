assoc.yx <- function(y, x, weights = NULL, xx = TRUE, correlation = "kendall",
                     na.rm.cat = FALSE, na.value.cat = "NAs", na.rm.cont = FALSE,
                     nperm = NULL, distrib = "asympt", dec = c(3,3)) {
  
  if(is.null(weights)) weights <- rep(1, length(y))
  if(any(is.na(weights))) stop("There are empty values in weights.")
  
  x <- as.data.frame(x)
  xnames <- names(x)
  xformats <- sapply(x,class)
  yformat <- class(y)
  
  x <- droplevels(x)
  
  res <- list()
  for(i in 1:ncol(x)) {
    if(yformat %in% c('numeric','integer') & xformats[i] %in% c('numeric','integer')) {
      z <- assoc.twocont(y, x[,i], weights = weights, na.rm = na.rm.cont, nperm = nperm, distrib = distrib)
      measure = correlation
      association = z[,correlation][1]
      permutation.pvalue = z[,correlation][2]
    }
    if(yformat %in% c('numeric','integer') & xformats[i]=='factor') {
      z <- assoc.catcont(x[,i], y, weights = weights,
                         na.rm.cat = na.rm.cat, na.value.cat = na.value.cat, na.rm.cont = na.rm.cont,
                         nperm = nperm, distrib = distrib)
      measure='Eta2'
      association = z$eta.squared
      permutation.pvalue = z$permutation.pvalue
    }
    if(yformat=='factor' & xformats[i] %in% c('numeric','integer')) {
      z <- assoc.catcont(y, x[,i], weights = weights,
                         na.rm.cat = na.rm.cat, na.value.cat = na.value.cat, na.rm.cont = na.rm.cont,
                         nperm = nperm, distrib = distrib)
      measure='Eta2'
      association = z$eta.squared
      permutation.pvalue = z$permutation.pvalue
    }
    if(yformat=='factor' & xformats[i]=='factor') {
      z <- assoc.twocat(x[,i], y, weights = weights, na.rm = na.rm.cat, na.value = na.value.cat, nperm = nperm, distrib = distrib)
      measure="Cramer V"
      association = z$global$cramer.v
      permutation.pvalue = z$global$permutation.pvalue
    }
    if(is.null(nperm)) permutation.pvalue <- NA
    res[[i]] <- data.frame(measure,association,permutation.pvalue, stringsAsFactors = F)
  }
  res <- do.call('rbind.data.frame',res)
  restot <- data.frame(variable=xnames,measure=res$measure,association=res$association,permutation.pvalue=res$permutation.pvalue)
  restot <- restot[order(restot$permutation.pvalue,restot$measure,-restot$association),]
  restot$measure <- gsub("kendall","Kendall tau",restot$measure)
  restot$measure <- gsub("spearman","Spearman rho",restot$measure)
  restot$measure <- gsub("pearson","Pearson rho",restot$measure)
  rownames(restot) <- NULL
  restot$association <- round(restot$association,dec[1])
  if(is.null(nperm)) {
    restot$permutation.pvalue <- NULL
  } else {
    restot$permutation.pvalue <- round(restot$permutation.pvalue,dec[2])
  }

  if(xx==TRUE) {
    combi <- utils::combn(xnames,2,simplify=F)
    res <- list()
    for(i in 1:length(combi)) {
      x1 <- x[,combi[[i]][1]]
      x2 <- x[,combi[[i]][2]]
      
      if(inherits(x1,c('numeric','integer')) & inherits(x2,c('numeric','integer'))) {
        z <- assoc.twocont(x1, x2, weights = weights, na.rm = na.rm.cont, nperm = nperm, distrib = distrib)
        measure = correlation
        association = z[,correlation][1]
        permutation.pvalue = z[,correlation][2]
      }
      if(inherits(x1,c('numeric','integer')) & inherits(x2,'factor')) {
        z <- assoc.catcont(x2, x1, weights = weights,
                           na.rm.cat = na.rm.cat, na.value.cat = na.value.cat, na.rm.cont = na.rm.cont,
                           nperm = nperm, distrib = distrib)
        measure='Eta2'
        association = z$eta.squared
        permutation.pvalue = z$permutation.pvalue
      }
      if(inherits(x1,'factor') & inherits(x2,c('numeric','integer'))) {
        z <- assoc.catcont(x1, x2, weights = weights,
                           na.rm.cat = na.rm.cat, na.value.cat = na.value.cat, na.rm.cont = na.rm.cont,
                           nperm = nperm, distrib = distrib)
        measure='Eta2'
        association = z$eta.squared
        permutation.pvalue = z$permutation.pvalue
      }
      if(inherits(x1,'factor') & inherits(x2,'factor')) {
        z <- assoc.twocat(x1, x2, weights = weights,
                          na.rm = na.rm.cat, na.value = na.value.cat,
                          nperm = nperm, distrib = distrib)
        measure="Cramer V"
        association = z$global$cramer.v
        permutation.pvalue = z$global$permutation.pvalue
      }
      if(is.null(nperm)) permutation.pvalue <- NA
      res[[i]] <- data.frame(measure,association,permutation.pvalue, stringsAsFactors = F)
    }
    res <- do.call('rbind.data.frame',res)
    noms <- do.call('rbind.data.frame',combi)
    restot2 <- data.frame(variable1=noms[,1],variable2=noms[,2],measure=res$measure,association=res$association,permutation.pvalue=res$permutation.pvalue,row.names=NULL)
    restot2 <- restot2[order(restot2$permutation.pvalue,restot2$measure,-restot2$association),]
    restot2$measure <- gsub("kendall","Kendall tau",restot2$measure)
    restot2$measure <- gsub("spearman","Spearman rho",restot2$measure)
    restot2$measure <- gsub("pearson","Pearson rho",restot2$measure)
    rownames(restot2) <- NULL
    restot2$association <- round(restot2$association,dec[1])
    if(is.null(nperm)) { 
      restot2$permutation.pvalue <- NULL
    } else {
      restot2$permutation.pvalue <- round(restot2$permutation.pvalue,dec[2])  
    }
  } else {
    restot2 <- NULL
  }

  return(list(YX=restot, XX=restot2))
}
