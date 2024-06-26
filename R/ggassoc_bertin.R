ggassoc_bertin <- function(data, mapping, prop.width = FALSE, sort = "none", 
                           add.gray = FALSE, add.rprop = FALSE,
                           na.rm = FALSE, na.value = "NAs") {
  
  xVal <- rlang::eval_tidy(mapping$x, data)
  yVal <- rlang::eval_tidy(mapping$y, data)
  wVal <- rlang::eval_tidy(mapping$weight, data)
  xName <- rlang::as_name(mapping$x)
  yName <- rlang::as_name(mapping$y)
  # wName <- rlang::as_name(mapping$weight)
  
  if(is.null(wVal)) wVal <- rep(1, length(xVal))
  if(any(is.na(wVal))) stop("There are empty values in weights.")
  
  if(na.rm==FALSE) {
    xVal <- factor(xVal, levels=c(levels(xVal), na.value))
    xVal[is.na(xVal)] <- na.value
    xVal <- factor(xVal)
    yVal <- factor(yVal, levels=c(levels(yVal), na.value))
    yVal[is.na(yVal)] <- na.value
    yVal <- factor(yVal)
  } else {
    complete <- !(is.na(xVal) | is.na(yVal))
    xVal <- xVal[complete]
    yVal <- yVal[complete]
    wVal <- wVal[complete]
  }
  
  if(sort!="none") {
    temp <- MASS::corresp(~xVal+yVal,nf=1)
    if(sort %in% c("x","both")) xVal <- factor(xVal, levels=names(sort(temp$rscore)))
    if(sort %in% c("y","both")) yVal <- factor(yVal, levels=names(sort(temp$cscore)))
  }
  
  wVal <- wVal*length(wVal)/sum(wVal)
  
  res0 <- assoc.twocat(x = xVal, y = yVal, weights = wVal, na.rm = TRUE)
  res <- res0$gather
  
  res1 <- res
  res1$co <- rep("A", nrow(res))
  res1$h <- ifelse(res1$rprop > res1$prop.y, 0, res1$rprop)
  
  res2 <- res
  res2$co <- rep("B", nrow(res))
  res2$h <- ifelse(res2$rprop > res2$prop.y, res2$prop.y, 0)
  
  res3 <- res
  res3$co <- rep("C", nrow(res))
  res3$h <- ifelse(res3$rprop > res3$prop.y, res3$rprop-res3$prop.y, 0)
  
  restot <- rbind(res1,res2,res3)
  restot$co <- factor(restot$co, levels=c("C","B","A"))
  
  if(prop.width) { 
    restot$wi <- restot$prop.x
  } else {
    restot$wi <- rep(1,nrow(restot))
  }
  
  if(add.gray) {
    cols <- c("black","gray","white")
  } else {
    cols <- c("black","black","white")
  }

  p <- ggplot2::ggplot(restot, ggplot2::aes(x = 1, y = .data$h, fill = .data$co, width = .data$wi)) +
    ggplot2::geom_col(col = "black") +
    ggplot2::scale_fill_manual(values = cols) +
    ggplot2::facet_grid(var.y ~ var.x, scales = "free_x", space = "free") +
    ggplot2::xlab(xName) +
    ggplot2::ylab(yName) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none",
                   panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank(),
                   strip.text.y.right = ggplot2::element_text(angle = 0, hjust = 0))
  
  if(add.rprop) p <- p +
    ggplot2::geom_text(data = restot[restot$co=="A",], 
                       ggplot2::aes(y = .data$rprop, label = round(100*.data$rprop,1)),
                       size = ggplot2::rel(2), vjust = -0.5)
  p
}


# ggassoc_bertin(Movies, aes(Country, Genre), na.rm= FALSE)
# ggassoc_bertin(Movies, aes(Country, Genre), na.rm= TRUE)
# ggassoc_bertin(Movies, aes(x = Country, y = Genre, weight = Critics), na.rm= FALSE)
# ggassoc_bertin(Movies, aes(x = Country, y = Genre, weight = Critics), na.rm= TRUE)
# 
# MoviesNA <- Movies
# MoviesNA$CountryNA <- MoviesNA$Country
# MoviesNA$GenreNA <- MoviesNA$Genre
# MoviesNA$CriticsNA <- MoviesNA$Critics
# MoviesNA$CountryNA[c(1,3,5)] <- NA
# MoviesNA$GenreNA[c(2,4,6)] <- NA
# MoviesNA$CriticsNA[7] <- NA
# 
# ggassoc_bertin(MoviesNA, aes(CountryNA, Genre), na.rm= FALSE)
# ggassoc_bertin(MoviesNA, aes(CountryNA, Genre), na.rm= TRUE)
# ggassoc_bertin(MoviesNA, aes(x = Country, y = Genre, weight = CriticsNA), na.rm= FALSE)
# ggassoc_bertin(MoviesNA, aes(x = Country, y = Genre, weight = CriticsNA), na.rm= TRUE)


# ggassoc_bertin(Movies, ggplot2::aes(x = Country, y = Genre), sort = "both", prop.width = TRUE)
# ggassoc_bertin(Movies, ggplot2::aes(x = Country, y = Genre), sort = "both", prop.width = TRUE, add.rprop = TRUE)
# ggassoc_bertin(Movies, ggplot2::aes(x = Country, y = Genre), sort = "both", prop.width = TRUE, ncol = 3)
# ggassoc_bertin(Movies, ggplot2::aes(x = Country, y = Genre), sort = "both", prop.width = TRUE, ncol = 3, add.rprop = TRUE)
# ggassoc_bertin(Movies, ggplot2::aes(x = Country, y = Genre, weight = Critics), sort = "both", prop.width = TRUE)

