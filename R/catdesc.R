catdesc <- function(y, x, weights = NULL,
                    na.rm.cat = FALSE, na.value.cat = "NA", na.rm.cont = FALSE,
                    measure = "phi", limit = NULL, correlation = "kendall", robust = TRUE,
                    nperm = NULL, distrib = "asympt", digits = 2) {
  
  if(is.null(weights)) weights <- rep(1, length(y))
  if(any(is.na(weights))) stop("There are empty values in weights.")
  
  if(na.rm.cat==FALSE) {
    if(any(is.na(y))) {
      y <- factor(y, levels=c(levels(y), na.value.cat))
      y[is.na(y)] <- na.value.cat
      y <- factor(y)
    }
  }

  icat <- which(sapply(x,is.factor))
  xcat <- as.data.frame(x[,icat])
  names(xcat) <- names(x)[icat]
  icon <- which(sapply(x, function(x) is.numeric(x) | is.integer(x)))
  xcon <- as.data.frame(x[,icon])
  names(xcon) <- names(x)[icon]
  
  if(ncol(xcat)==0) {
    lcat <- lapply(levels(y), function(x) return(NULL))
    names(lcat) <- levels(y)
  }
  
  if(ncol(xcat)>0) {
    lcat <- list()
    for(i in 1:ncol(xcat)) {
      temp <- assoc.twocat(y, xcat[,i], weights = weights, na.rm = na.rm.cat, na.value = na.value.cat, nperm = nperm, distrib = "asympt")$gather
      temp$prop.x <- NULL
      temp$prop.y <- NULL
      temp$categories <- paste(names(xcat)[i],temp$var.y,sep='.')
      lcat[[i]] <- merge(temp, aggregate(prop~var.y, data=temp, sum), by="var.y")
    }
    lcat <- do.call("rbind.data.frame",lcat)
    lcat <- lcat[order(-lcat[,measure]),]
    if(!is.null(limit)) lcat <- lcat[abs(lcat[,measure])>=limit,]
    lcat$cprop <- round(100*lcat$cprop, 1)
    lcat$rprop <- round(100*lcat$rprop, 1)
    lcat$prop.y <- round(100*lcat$prop.y, 1)
    lcat[,measure] <- round(lcat[,measure], 3)
    splitvar <- lcat$var.x
    noms <- c("categories","freq","cprop","rprop","prop.y",measure)
    if(!is.null(nperm)) {
      lcat$pvalue <- round(lcat$perm.pval, 5)
      noms <- c(noms, "pvalue")
      }
    lcat <- lcat[, noms]
    names(lcat)[3:5] <- c("pct.y.in.x","pct.x.in.y","overall.pct.x")
    lcat <- split(lcat, splitvar)
    lcat <- lapply(lcat, function(x) {rownames(x) <- NULL ; return(x)})
  }

  if(ncol(xcon)==0) {
    lcon <- lapply(levels(y), function(x) return(NULL))
    names(lcon) <- levels(y)
  }
    
  if(ncol(xcon)>0) {
    lcon <- list()
    for(i in 1:ncol(xcon)) {
      asso <- assoc.catcont(y, xcon[,i], weights = weights,
                            na.rm.cat = na.rm.cat, na.value.cat = na.value.cat, na.rm.cont = na.rm.cont,
                            nperm = nperm, distrib = distrib)
      temp <- data.frame(correlation = asso$cor)
      if(!is.null(nperm)) temp <- data.frame(temp, pvalue = round(asso$cor.perm.pval, 5))
      temp$variables <- rep(names(xcon)[i],nrow(temp))
      temp$categories <- rownames(temp)
      if(robust==TRUE) {
        temp$median.x.in.ycat <- sapply(split(data.frame(xcon[,i],weights), y), function(x) weighted.quantile(x[,1],x[,2],probs=.5, na.rm = na.rm.cont))
        temp$median.x.global <- rep(weighted.quantile(xcon[,i], weights, na.rm = na.rm.cont, probs = .5), nrow(temp))
        temp$mad.x.in.ycat <- sapply(split(data.frame(xcon[,i],weights), y), function(x) weighted.mad(x[,1],x[,2], na.rm = na.rm.cont))
        temp$mad.x.global <- rep(weighted.mad(xcon[,i], weights, na.rm = na.rm.cont), nrow(temp))
      }
      if(robust==FALSE) {
        temp$median.x.in.ycat <- sapply(levels(y), function(x) weighted.mean(xcon[y[!is.na(y)]==x,i], weights[y[!is.na(y)]==x], na.rm = na.rm.cont))
        temp$median.x.global <- rep(weighted.mean(xcon[,i], weights, na.rm = na.rm.cont), nrow(temp))
        temp$mad.x.in.ycat <- sapply(levels(y), function(x) weighted.sd(xcon[y[!is.na(y)]==x,i], weights[y[!is.na(y)]==x], na.rm = na.rm.cont))
        temp$mad.x.global <- rep(weighted.sd(xcon[,i], weights, na.rm = na.rm.cont), nrow(temp))       
      }
      lcon[[i]] <- temp
    }
    lcon <- do.call("rbind.data.frame",lcon)
    lcon <- lcon[order(-lcon$correlation),]
    lcon$median.x.in.ycat <- round(lcon$median.x.in.ycat, digits)
    lcon$median.x.global <- round(lcon$median.x.global, digits)
    lcon$mad.x.in.ycat <- round(lcon$mad.x.in.ycat, digits)
    lcon$mad.x.global <- round(lcon$mad.x.global, digits)
    # lcon$cor <- round(lcon$cor,dec[6])
    splitvar <- lcon$categories
    noms <- c("variables", "median.x.in.ycat", "median.x.global", 
              "mad.x.in.ycat", "mad.x.global", "correlation")
    if(!is.null(nperm)) noms <- c(noms, "pvalue")
    lcon <- lcon[, noms]
    # lcon <- lcon[,c(2,4:7,1)]
    if(robust==FALSE) {
      names(lcon)[2:5] <- c("mean.in.category","overall.mean",
                            "sd.in.category","overall.sd")
    } else {
      names(lcon)[2:5] <- c("median.in.category","overall.median",
                            "mad.in.category","overall.mad")      
    }
    # rownames(lcon) <- NULL
    lcon <- split(lcon,splitvar)
    lcon <- lapply(lcon, function(x) {rownames(x) <- NULL ; return(x)})
  }
  
  bylevel <- list()
  for(i in levels(y)) {
    bylevel[[i]]$categories <- lcat[[i]]
    bylevel[[i]]$continuous.var <- lcon[[i]]
  }
  
  v <- assoc.yx(y, x, xx = FALSE, weights = weights, correlation = correlation,
                na.rm.cat = na.rm.cat, na.value.cat = na.value.cat, na.rm.cont = na.rm.cont,
                nperm = nperm, distrib = distrib, dec = c(3,5))$YX
  if(!is.null(nperm)) names(v)[4] <- "pvalue"
  
  res <- list(variables = v,
              bylevel = bylevel)
  return(res)
}
