phi.table <- function(x, y, weights = NULL, na.rm = FALSE, na.value = "NAs", digits = 3) {
  
  if(is.null(weights)) weights <- rep(1, length(x))
  if(any(is.na(weights))) stop("There are empty values in weights.")
  
  if(na.rm==FALSE) {
    if(any(is.na(x))) {
      x <- factor(x, levels=c(levels(x), na.value))
      x[is.na(x)] <- na.value
      # x <- factor(x)
    }
    if(any(is.na(y))) {
      y <- factor(y, levels=c(levels(y), na.value))
      y[is.na(y)] <- na.value
      # y <- factor(y)      
    }
  } else {
    complete <- !(is.na(x) | is.na(y))
    x <- x[complete]
    y <- y[complete]
    weights <- weights[complete]
  }
  
  if(!is.factor(x)) x <- factor(x)
  xdic <- dichot(x, out='numeric')
  if(!is.factor(y)) y <- factor(y)
  ydic <- dichot(y, out='numeric')
  tab <- matrix(nrow = ncol(xdic), ncol = ncol(ydic))
  for(i in 1:nrow(tab)) {
    for(j in 1:ncol(tab)) {
      tab[i,j] <- weighted.cor(xdic[,i], ydic[,j], weights = weights, method = "pearson")      
    }
  }
  tab[is.nan(tab)] <- 0
  tab <- as.table(tab)
  rownames(tab) <- levels(x)
  colnames(tab) <- levels(y)
  if(!is.null(digits)) tab <- round(tab,digits)
  return(tab)
}
