pem.table <- function(x, y, weights = NULL, sort = FALSE, na.rm = FALSE, na.value = "NA", digits = 1) {

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
  
  cont <- stats::xtabs(data = data.frame(x, y, weights), weights~x+y)
  tota <- colSums(cont)
  totb <- rowSums(cont)
  total <- sum(cont)
  theo <- matrix(nrow=nrow(cont),ncol=ncol(cont))
  for(i in 1:nrow(cont)) { 
    for(j in 1:ncol(cont)) theo[i,j] <- tota[j]*totb[i]/total
    }
  ecart <- cont-theo
  max <- matrix(nrow=nrow(cont),ncol=ncol(cont))
  emax <- matrix(nrow=nrow(cont),ncol=ncol(cont))
  pem <- matrix(nrow=nrow(cont),ncol=ncol(cont))
  for(i in 1:nrow(cont)) { for(j in 1:ncol(cont)) {
    if(ecart[i,j]>=0) max[i,j] <- min(tota[j],totb[i])
    if(ecart[i,j]<0 & tota[j]<=(total-totb[i])) max[i,j] <- 0
    if(ecart[i,j]<0 & tota[j]>(total-totb[i])) max[i,j] <- tota[j]+totb[i]-total
    emax[i,j] <- max[i,j] - theo[i,j]
    pem[i,j] <- ifelse(ecart[i,j]>=0,ecart[i,j]/emax[i,j]*100,0-ecart[i,j]/emax[i,j]*100)
    }}
  dimnames(pem) <- dimnames(cont)
  if(isFALSE(sort)) {
    z <- cont
  } else {
    temp <- suppressWarnings(MASS::corresp(cont,nf=1))
    z <- cont[order(temp$rscore),order(temp$cscore)]
  }
  tota <- colSums(z)
  totb <- rowSums(z)
  maxc <- matrix(0,nrow=nrow(z),ncol=ncol(z))
  i <- 1; j <- 1
  repeat {
    m <- min(tota[j],totb[i])
    maxc[i,j] <- m
    tota[j] <- tota[j] - m
    totb[i] <- totb[i] - m
    if(sum(tota)+sum(totb)<0.000001) break
    if(tota[j]==0) j <- j+1
    if(totb[i]==0) i <- i+1
  }
  
  if(isTRUE(sort)) {
    pemg <- (sum(ecart)+sum(abs(ecart)))/(sum(maxc-theo[order(temp$rscore),order(temp$cscore)])+sum(abs(maxc-theo[order(temp$rscore),order(temp$cscore)])))
  } else {
    pemg <- (sum(ecart)+sum(abs(ecart)))/(sum(maxc-theo)+sum(abs(maxc-theo)))
  }
  pemg <- 100*pemg
  
  pem <- as.table(pem)

  if(!is.null(digits)) pem <- round(pem,digits)
  if(!is.null(digits)) pemg <- round(pemg,digits)
  
  PEM <- list(peml=pem, pemg=pemg)
  return(PEM)
}
