# x <- Movies$Genre
# y <- Movies$Country
# x[2:3] <- NA
# y[9:10] <- NA
# levels(x) <- c(levels(x), "Pouet pouet")
# weighted.table(x,y)

weighted.table <- function(x, y = NULL, weights = NULL, stat = "freq", mar = FALSE, 
                           na.rm = FALSE, na.value = "NAs", digits = 1) {
  
  if(is.null(weights)) weights <- rep(1, length(x))
  if(any(is.na(weights))) stop("There are empty values in weights.")
  
  if(na.rm==FALSE) {   # add na level
    if(any(is.na(x))) {
      x <- factor(x, levels=c(levels(x), na.value))
      x[is.na(x)] <- na.value
      # x <- factor(x)
    }
    if(!is.null(y)) {
      if(any(is.na(y))) {
        y <- factor(y, levels=c(levels(y), na.value))
        y[is.na(y)] <- na.value
        # y <- factor(y)
      }
    }
    X <- x
    Y <- y
    W <- weights
  } else {   # remove obs with na
    if(!is.null(y)) {
      complete <- !(is.na(x) | is.na(y))
      Y <- y[complete]
    } else {
      complete <- !is.na(x)
    }
    X <- x[complete]
    W <- weights[complete]   
  }

  if(!is.null(y)) {
    t <- tapply(W, list(X,Y), sum)
    tab <- as.table(t)
    tab[is.na(tab)] <- 0
    if(mar) tab <- addmargins(tab)
    if(stat=="prop") {
      tab <- 100*prop.table(tab)
      if(mar) tab <- 4*tab
    }
    if(stat=="rprop") {
      tab <- 100*apply(tab, 2, function(x) x/rowSums(tab))
      if(mar) tab <- 2*tab
    }
    if(stat=="cprop") {
      tab <- t(100*apply(tab, 1, function(x) x/colSums(tab)))
      if(mar) tab <- 2*tab}
  } else {
    t <- tapply(W, list(X), sum)
    tab <- as.table(t)
    tab[is.na(tab)] <- 0
    if(mar) tab <- addmargins(tab)
    if(stat=="prop") {
      tab <- 100*prop.table(tab)
      if(mar) tab <- 2*tab
    }
  }

  # tab[is.na(tab)] <- 0
  
  if(!is.null(digits)) tab <- round(tab, digits)
  
  return(tab)
  }
