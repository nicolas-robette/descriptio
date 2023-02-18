catdesc <- function(y, x, weights = NULL,
                    na.rm.cat = FALSE, na.value.cat = "NA", na.rm.cont = FALSE,
                    min.phi = NULL, robust = TRUE,
                    nperm = NULL, distrib = "asympt", dec = c(3,3,3,3,1,3)) {
  
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
      temp <- assoc.twocat(y, xcat[,i], weights = weights, na.rm = na.rm.cat, na.value = na.value.cat, nperm = NULL)$gather
      temp$prop.x <- NULL
      temp$prop.y <- NULL
      temp$categories <- paste(names(xcat)[i],temp$var.y,sep='.')
      lcat[[i]] <- merge(temp, aggregate(prop~var.y, data=temp, sum), by="var.y")
    }
    lcat <- do.call("rbind.data.frame",lcat)
    lcat <- lcat[order(-lcat$phi),]
    if(!is.null(min.phi)) lcat <- lcat[abs(lcat$phi)>=min.phi,]
    lcat$cprop <- round(lcat$cprop, dec[3])
    lcat$rprop <- round(lcat$rprop, dec[3])
    lcat$prop.y <- round(lcat$prop.y, dec[3])
    lcat$phi <- round(lcat$phi, dec[4])
    splitvar <- lcat$var.x
    lcat <- lcat[,c("categories","cprop","rprop","prop.y","phi")]
    names(lcat) <- c("categories","pct.ycat.in.xcat","pct.xcat.in.ycat","pct.xcat.global","phi")
    rownames(lcat) <- NULL
    lcat <- split(lcat, splitvar)
  }

  if(ncol(xcon)==0) {
    lcon <- lapply(levels(y), function(x) return(NULL))
    names(lcon) <- levels(y)
  }
    
  if(ncol(xcon)>0) {
    lcon <- list()
    for(i in 1:ncol(xcon)) {
      temp <- data.frame(cor = assoc.catcont(y, xcon[,i], weights = weights,
                                             na.rm.cat = na.rm.cat, na.value.cat = na.value.cat, na.rm.cont = na.rm.cont,
                                             nperm = NULL, digits = 9)$cor)
      temp$variables <- rep(names(xcon)[i],nrow(temp))
      temp$categories <- rownames(temp)
      if(robust==TRUE) {
        temp$median.x.in.ycat <- sapply(levels(y), function(x) weighted.quantile(xcon[y[!is.na(y)]==x,i], weights[y[!is.na(y)]==x], 
                                                                                 na.rm = na.rm.cont, probs = .5))
        temp$median.x.global <- rep(weighted.quantile(xcon[,i], weights, na.rm = na.rm.cont, probs = .5), nrow(temp))
        temp$mad.x.in.ycat <- sapply(levels(y), function(x) weighted.mad(xcon[y[!is.na(y)]==x,i], weights[y[!is.na(y)]==x], na.rm = na.rm.cont))
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
    lcon <- lcon[order(-lcon$cor),]
    lcon$median.x.in.ycat <- round(lcon$median.x.in.ycat, dec[5])
    lcon$median.x.global <- round(lcon$median.x.global, dec[5])
    lcon$mad.x.in.ycat <- round(lcon$mad.x.in.ycat, dec[5])
    lcon$mad.x.global <- round(lcon$mad.x.global, dec[5])
    lcon$cor <- round(lcon$cor,dec[6])
    splitvar <- lcon$categories
    lcon <- lcon[,c(2,4:7,1)]
    if(robust==FALSE) names(lcon) <- c("variables","mean.x.in.ycat","mean.x.global",
                                       "sd.x.in.ycat","sd.x.global","cor")
    rownames(lcon) <- NULL
    lcon <- split(lcon,splitvar)
  }
  
  bylevel <- list()
  for(i in levels(y)) {
    bylevel[[i]]$categories <- lcat[[i]]
    bylevel[[i]]$continuous.var <- lcon[[i]]
  }
  
  res <- list(variables = assoc.yx(y, x, weights = weights, 
                                   na.rm.cat = na.rm.cat, na.value.cat = na.value.cat, na.rm.cont = na.rm.cont,
                                   xx = FALSE, nperm = nperm, distrib = distrib, dec = dec[1:2])$YX,
              bylevel = bylevel)
  return(res)
}

# data(Movies)
# catdesc(Movies$ArtHouse, Movies[,c("Budget","Genre","Country")])

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
# y1[9:10] <- NA
# 
# catdesc(y0, data.frame(a0,b0,c0), weights = w0)
# catdesc(y0, data.frame(a0,b0,c0), weights = w1)
# catdesc(y0, data.frame(a1,b0,c0), na.rm.cont = FALSE)
# catdesc(y0, data.frame(a1,b0,c0), na.rm.cont = TRUE)
# catdesc(y0, data.frame(a0,b1,c0), na.rm.cat = FALSE, na.value.cat = "99")
# catdesc(y0, data.frame(a0,b1,c0), na.rm.cat = TRUE, na.value.cat = "99")
# catdesc(y1, data.frame(a0,b0,c0), na.rm.cat = FALSE, na.value.cat = "99")
# catdesc(y1, data.frame(a0,b0,c0), na.rm.cat = TRUE, na.value.cat = "99")
