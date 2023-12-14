assoc.xx <- function(x, weights = NULL, correlation = "kendall",
                     na.rm.cat = FALSE, na.value.cat = "NA", na.rm.cont = FALSE,
                     nperm = NULL, distrib = "asympt", dec = c(3,3)) {
  
  if(is.null(weights)) weights <- rep(1, nrow(x))
  if(any(is.na(weights))) stop("There are empty values in weights.")
  
  x <- as.data.frame(x)
  xnames <- names(x)
  xformats <- sapply(x,class)

  x <- droplevels(x)
  
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
    restot <- data.frame(variable1=noms[,1],variable2=noms[,2],measure=res$measure,association=res$association,permutation.pvalue=res$permutation.pvalue,row.names=NULL)
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

  return(restot)
}


data(iris)
iris2 = iris
iris2$Species = factor(iris$Species == "versicolor")
assoc.xx(iris2, nperm=10)

# data(Movies)
# y0 = y1 = Movies$ArtHouse
# a0 = a1 = Movies$Budget
# b0 = b1 = Movies$Genre
# c0 = c1 = Movies$Country
# w0 = w1 = Movies$Critics
# w1[1:2] <- NA
# a1[3:4] <- NA
# b1[5:6] <- NA
# c1[7:8] <- NA
# 
# assoc.yx(y0, data.frame(a0,b0,c0), weights = w0)
# assoc.yx(y0, data.frame(a0,b0,c0), weights = w1)
# assoc.yx(y0, data.frame(a1,b0,c0), na.rm.cont = FALSE)
# assoc.yx(y0, data.frame(a1,b0,c0), na.rm.cont = TRUE)
# assoc.yx(y0, data.frame(a0,b1,c0), na.rm.cat = FALSE, na.value.cat = "99")
# assoc.yx(y0, data.frame(a0,b1,c0), na.rm.cat = TRUE, na.value.cat = "99")
# assoc.yx(y1, data.frame(a0,b0,c0), na.rm.cat = FALSE, na.value.cat = "99")
# assoc.yx(y1, data.frame(a0,b0,c0), na.rm.cat = TRUE, na.value.cat = "99")
