ggassoc_chiasmogram <- function(data, mapping, measure = "phi",
                                limits = NULL, sort = "none",
                                na.rm = FALSE, na.value = "NAs",
                                colors = NULL, direction = 1) {

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
  
  breaks1 <- as.data.frame(weighted.table(xVal, weights = wVal, mar = FALSE, na.rm = TRUE))
  names(breaks1) <- c("xVal", "Freq")
  breaks1$cumw <- cumsum(breaks1$Freq)
  breaks1$pos1 <- .5 * (breaks1$cumw + lag1(breaks1$cumw, default = 0))
  breaks1$prop1 = round(100*breaks1$Freq/sum(breaks1$Freq),1)
  breaks1$var.xb = paste(breaks1$prop1, breaks1$xVal, sep = " - ")
  names(breaks1)[1] <- "var.x"
  names(breaks1)[2] <- "freq1"
  breaks1$cumw <- NULL
  
  breaks2 <- as.data.frame(weighted.table(yVal, weights = wVal, mar = FALSE, na.rm = TRUE))
  names(breaks2) <- c("yVal", "Freq")
  breaks2$cumw <- cumsum(breaks2$Freq)
  breaks2$pos2 <- .5 * (breaks2$cumw + lag1(breaks2$cumw, default = 0))
  breaks2$prop2 = round(100*breaks2$Freq/sum(breaks2$Freq),1)
  breaks2$var.yb = paste(breaks2$prop2, breaks2$yVal, sep = " - ")
  names(breaks2)[1] <- "var.y"
  names(breaks2)[2] <- "freq2"
  breaks2$cumw <- NULL
  
  df <- assoc.twocat(x = xVal, y = yVal, weights = wVal, na.rm = TRUE)$gather
  df <- merge(df, breaks1, by = "var.x")
  df <- merge(df, breaks2, by = "var.y")

  df$asso <- df[,measure]
  
  if(is.null(colors)) colors <- c("#009392FF","#39B185FF","#9CCB86FF","#E9E29CFF","#EEB479FF","#E88471FF","#CF597EFF")  # rcartocolor::Temps
  if(direction==-1) colors <- rev(colors)
  
  ggplot2::ggplot(df, ggplot2::aes(x = .data$pos1, y = .data$pos2)) +
    ggplot2::geom_tile(aes(width = .data$freq1,
                           height = .data$freq2,
                           fill = .data$asso),
                       color = 'black') +
    ggplot2::scale_x_continuous(breaks = breaks1$pos1, labels = breaks1$var.xb, expand = c(0, 0.1), position = "top") +
    ggplot2::scale_y_continuous(breaks = breaks2$pos2, labels = breaks2$var.yb, expand = c(0, 0.1), position = "right") +
    ggplot2::scale_fill_gradientn(colours = colors, limits = limits, name = measure) +
    ggplot2::xlab(xName) +
    ggplot2::ylab(yName) +
    ggplot2::theme(axis.text.x = element_text(angle = 45, vjust = 0, hjust = 0),
                   legend.position = "bottom",
                   legend.key.size = unit(1, 'cm'))
}


# ggassoc_chiasmogram(Movies, aes(Country, Genre), na.rm= FALSE)
# ggassoc_chiasmogram(Movies, aes(Country, Genre), na.rm= TRUE)
# ggassoc_chiasmogram(Movies, aes(x = Country, y = Genre, weight = Critics), na.rm= FALSE)
# ggassoc_chiasmogram(Movies, aes(x = Country, y = Genre, weight = Critics), na.rm= TRUE)
# 
# MoviesNA <- Movies
# MoviesNA$CountryNA <- MoviesNA$Country
# MoviesNA$GenreNA <- MoviesNA$Genre
# MoviesNA$CriticsNA <- MoviesNA$Critics
# MoviesNA$CountryNA[c(1,3,5)] <- NA
# MoviesNA$GenreNA[c(2,4,6)] <- NA
# MoviesNA$CriticsNA[7] <- NA
# 
# ggassoc_chiasmogram(MoviesNA, aes(CountryNA, Genre), na.rm= FALSE)
# ggassoc_chiasmogram(MoviesNA, aes(CountryNA, Genre), na.rm= TRUE)
# ggassoc_chiasmogram(MoviesNA, aes(x = Country, y = Genre, weight = CriticsNA), na.rm= FALSE)
# ggassoc_chiasmogram(MoviesNA, aes(x = Country, y = Genre, weight = CriticsNA), na.rm= TRUE)



# data(Movies)
# ggassoc_chiasmogram(Movies, aes(Country,Genre), sort = "both", measure = "phi")
# ggassoc_chiasmogram(Movies, aes(Country,Genre,weight = Critics), sort = "both", measure = "phi")
# ggassoc_chiasmogram(Movies, aes(Country,Genre), sort = "both", measure = "phi", limits = c(-0.5, 0.5))