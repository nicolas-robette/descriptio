# phi.table(x0, y0, weights = w0, na.rm = FALSE)
# phi.table(x0, y0, weights = w0, na.rm = TRUE)
# phi.table(x0, y0, weights = w1, na.rm = FALSE)
# phi.table(x0, y0, weights = w1, na.rm = TRUE)
# phi.table(x1, y0, weights = w0, na.rm = FALSE)
# phi.table(x1, y0, weights = w0, na.rm = TRUE)

phi.table <- function(x, y, weights = NULL, na.rm = FALSE, na.value = "NA", digits = 3) {
  
  if(is.null(weights)) weights <- rep(1, length(x))
  if(any(is.na(weights))) stop("There are empty values in weights.")
  
  if(na.rm==FALSE) {
    x <- factor(x, levels=c(levels(x), na.value))
    x[is.na(x)] <- na.value
    x <- factor(x)
    y <- factor(y, levels=c(levels(y), na.value))
    y[is.na(y)] <- na.value
    y <- factor(y)
  } else {
    complete <- !(is.na(x) | is.na(y))
    x <- x[complete]
    y <- y[complete]
    weights <- weights[complete]
  }
  
  xdic <- dichot(factor(x),out='numeric')
  ydic <- dichot(factor(y),out='numeric')
  tab <- matrix(nrow = ncol(xdic), ncol = ncol(ydic))
  for(i in 1:nrow(tab)) {
    for(j in 1:ncol(tab)) {
      tab[i,j] <- weighted.cor(xdic[,i], ydic[,j], weights = weights, method = "pearson")      
    }
  }
  tab <- as.table(tab)
  rownames(tab) <- levels(factor(x))
  colnames(tab) <- levels(factor(y))
  if(!is.null(digits)) tab <- round(tab,digits)
  return(tab)
}
