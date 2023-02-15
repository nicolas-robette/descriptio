ggassoc_phiplot <- function(data, mapping, measure = "phi", limit = NULL, sort = "none", na.rm = FALSE, na.value = "NA") {
  
  xVal <- rlang::eval_tidy(mapping$x, data)
  yVal <- rlang::eval_tidy(mapping$y, data)
  wVal <- rlang::eval_tidy(mapping$weight, data)
  xName <- rlang::as_name(mapping$x)
  yName <- rlang::as_name(mapping$y)
  # wName <- rlang::as_name(mapping$weight)
  
  if(is.null(wVal)) mapping$weight <- ggplot2::aes_string(weight = sprintf("rep(1, length(%s))",xName))$weight
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
    data <- data[complete,]
  }
  
  if(sort!="none") {
    temp <- MASS::corresp(~xVal+yVal,nf=1)
    if(sort %in% c("x","both")) xVal <- factor(xVal, levels=names(sort(temp$rscore)))
    if(sort %in% c("y","both")) yVal <- factor(yVal, levels=names(sort(temp$cscore)))
  }
  
  data[,xName] <- xVal
  data[,yName] <- yVal
  
  mapping$xmin <- ggplot2::aes_string(xmin = "as.numeric(after_stat(x)) - after_stat(prop.x)")$xmin
  mapping$xmax <- ggplot2::aes_string(xmax = "as.numeric(after_stat(x)) + after_stat(prop.x)")$xmax
  mapping$ymin <- ggplot2::aes_string(ymin = "as.numeric(after_stat(y))")$ymin
  if(is.null(limit)) {
    mapping$ymax <- ggplot2::aes_string(ymax = sprintf("as.numeric(after_stat(y)) + after_stat(%s)*0.5/(1.1*max(abs(after_stat(%s))))", measure, measure))$ymax
  } else {
    mapping$ymax <- ggplot2::aes_string(ymax = sprintf("as.numeric(after_stat(y)) + after_stat(%s)*0.5/limit", measure))$ymax
  }
  mapping$fill <- ggplot2::aes_string(fill = sprintf("factor(sign(after_stat(%s)))", measure))$fill

  ggplot(data, mapping) +
    ggplot2::geom_rect(stat = "twocat", col = "black", linewidth = 0.2) +
    ggplot2::scale_fill_manual(values = c("white","black"), guide = "none") +
    ggplot2::xlab(xName) +
    ggplot2::ylab(yName) +
    # ggplot2::scale_x_continuous(breaks = 1:nlevels(xVal), labels = levels(xVal)) +
    # ggplot2::scale_y_continuous(breaks = 1:nlevels(yVal), labels = levels(yVal)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid.major.x = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_text(vjust = 5))
}


# ggassoc_phiplot(Movies, aes(Country, Genre), na.rm= FALSE)
# ggassoc_phiplot(Movies, aes(Country, Genre), na.rm= TRUE)
# ggassoc_phiplot(Movies, aes(x = Country, y = Genre, weight = Critics), na.rm= FALSE)
# ggassoc_phiplot(Movies, aes(x = Country, y = Genre, weight = Critics), na.rm= TRUE)
# 
# MoviesNA <- Movies
# MoviesNA$CountryNA <- MoviesNA$Country
# MoviesNA$GenreNA <- MoviesNA$Genre
# MoviesNA$CriticsNA <- MoviesNA$Critics
# MoviesNA$CountryNA[c(1,3,5)] <- NA
# MoviesNA$GenreNA[c(2,4,6)] <- NA
# MoviesNA$CriticsNA[7] <- NA
# 
# ggassoc_phiplot(MoviesNA, aes(CountryNA, Genre), na.rm= FALSE)
# ggassoc_phiplot(MoviesNA, aes(CountryNA, Genre), na.rm= TRUE)
# ggassoc_phiplot(MoviesNA, aes(x = Country, y = Genre, weight = CriticsNA), na.rm= FALSE)
# ggassoc_phiplot(MoviesNA, aes(x = Country, y = Genre, weight = CriticsNA), na.rm= TRUE)



# ggassoc_phiplot(Taste, aes(Age, Educ))
# ggassoc_phiplot(Taste, aes(Age, Educ)) + facet_wrap(~ Gender)
