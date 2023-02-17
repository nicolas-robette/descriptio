ggassoc_boxplot <- function(data, mapping,
                            na.rm.cat = FALSE, na.value.cat = "NA", na.rm.cont = FALSE,
                            axes.labs = TRUE, ticks.labs = TRUE, text.size = 3,
                            box = TRUE, notch = FALSE, violin = TRUE) {

  xVal <- rlang::eval_tidy(mapping$x, data)
  yVal <- rlang::eval_tidy(mapping$y, data)
  wVal <- rlang::eval_tidy(mapping$weight, data)
  xName <- rlang::as_name(mapping$x)
  yName <- rlang::as_name(mapping$y)
  
  if(is.null(wVal)) {
    mapping$weight <- ggplot2::aes_string(weight = sprintf("rep(1, length(%s))",xName))$weight
    wVal <- rep(1, nrow(data))
  }
  if(any(is.na(wVal))) stop("There are empty values in weights.")
  
  if(is.numeric(yVal)) {
    contVal <- yVal
    contName <- yName
    catVal <- xVal
    catName <- xName
  } else {
    contVal <- xVal
    contName <- xName
    catVal <- yVal
    catName <- yName
  }
  
  if(na.rm.cont) {
    complete <- !is.na(contVal)
    catVal <- factor(catVal[complete])
    contVal <- contVal[complete]
    wVal <- wVal[complete]
  } else {
    if(any(is.na(contVal))) stop("There are empty values in the continuous variable. \nPlease consider transforming your data (filtering, recoding, imputation, etc.) or set na.rm.cont to TRUE.")
  }
  
  if(na.rm.cat==FALSE) {
    catVal <- factor(catVal, levels=c(levels(catVal), na.value.cat))
    catVal[is.na(catVal)] <- na.value.cat
    catVal <- factor(catVal)
  } else {
    complete <- !is.na(catVal)
    catVal <- factor(catVal[complete])
    contVal <- contVal[complete]
    wVal <- wVal[complete]
  }
  
  assoc <- assoc.catcont(catVal, contVal, weights = wVal, na.rm.cat = TRUE, na.rm.cont = TRUE)$eta.squared
  
  newdata <- data.frame(catVal, contVal, wVal)
  newdata$catVal <- factor(newdata$catVal, levels=rev(levels(newdata$catVal)))
  
  p <- ggplot2::ggplot(newdata, ggplot2::aes(x=.data$catVal, y=.data$contVal, weight=.data$wVal))
  
  if(violin) p <- p + ggplot2::geom_violin(scale="count", alpha=.6, color="grey")
  
  if(box) p <- p + ggplot2::geom_boxplot(varwidth=TRUE, notch=notch, fill="grey", alpha=.6, outlier.size=0.5, outlier.alpha=0.2)
  
  p <- p + 
          ggplot2::theme_minimal() +
          ggplot2::theme( legend.position="none",
                          panel.grid.major = ggplot2::element_blank(),
                          panel.grid.minor=ggplot2::element_blank(),
                          panel.border = ggplot2::element_rect(
                            linetype = "solid",
                            color = "grey",
                            fill = "transparent"))
  
  if(!is.null(text.size)) p <- p + ggplot2::annotate(geom="label", label=paste0("eta2 = ",round(assoc,3)), x=-Inf, y=Inf, size=text.size,
                                                     hjust=0, vjust=1, label.size=NA, fill="white", alpha=.5)
  if(axes.labs) {
    p <- p + ggplot2::xlab(catName) + ggplot2::ylab(contName)
  } else {
    p <- p + ggplot2::xlab(NULL) + ggplot2::ylab(NULL)
  }
  if(!ticks.labs) p <- p + ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                                          axis.text.x = ggplot2::element_blank())
  p
}

# data(Movies)
# ggassoc_boxplot(Movies, aes(x = Budget, y = Genre))
# ggassoc_boxplot(Movies, aes(x = Budget, y = Genre, weight = Critics))
# 
# MoviesNA <- Movies
# MoviesNA$BudgetNA <- MoviesNA$Budget
# MoviesNA$GenreNA <- MoviesNA$Genre
# MoviesNA$CriticsNA <- MoviesNA$Critics
# MoviesNA$BudgetNA[c(1,3,5)] <- NA
# MoviesNA$GenreNA[c(2,4,6)] <- NA
# MoviesNA$CriticsNA[7] <- NA
# 
# ggassoc_boxplot(MoviesNA, aes(x = Budget, y = Genre, weight = CriticsNA))
# ggassoc_boxplot(MoviesNA, aes(x = Budget, y = Genre, weight = CriticsNA))
# ggassoc_boxplot(MoviesNA, aes(x = BudgetNA, y = Genre, weight = Critics), na.rm.cont = TRUE)
# ggassoc_boxplot(MoviesNA, aes(x = BudgetNA, y = Genre, weight = Critics), na.rm.cont = FALSE)
# ggassoc_boxplot(MoviesNA, aes(x = Budget, y = GenreNA, weight = Critics), na.rm.cat = TRUE)
# ggassoc_boxplot(MoviesNA, aes(x = Budget, y = GenreNA, weight = Critics), na.rm.cat = FALSE)
