phi.table <- function(x,y,weights=rep(1,length(x)),digits=3) {
  # tab <- cor(dichotom(factor(x),out='numeric'),
  #            dichotom(factor(y),out='numeric'),
  #            method='pearson',
  #            use='complete.obs')
  xdic <- dichot(factor(x),out='numeric')
  ydic <- dichot(factor(y),out='numeric')
  tab <- matrix(nrow = ncol(xdic), ncol = ncol(ydic))
  for(i in 1:nrow(tab)) {
    for(j in 1:ncol(tab)) {
      tab[i,j] <- weighted.cor(xdic[,i], ydic[,j], weights = weights, method = "pearson", remove_missing = TRUE)      
    }
  }
  tab <- as.table(tab)
  rownames(tab) <- levels(factor(x))
  colnames(tab) <- levels(factor(y))
  if(!is.null(digits)) tab <- round(tab,digits)
  return(tab)
}
