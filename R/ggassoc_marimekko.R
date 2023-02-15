ggassoc_marimekko <- function(data, mapping, type = "classic",
                              measure = "phi", limits = NULL, 
                              na.rm = FALSE, na.value = "NA",
                              palette = NULL, colors = NULL, direction = 1,
                              linecolor = "gray60", linewidth = 0.1,
                              sort = "none", legend = "right") {
  
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
  
  levels(xVal) <- paste0(1:nlevels(xVal), levels(xVal))
  levels(yVal) <- paste0(1:nlevels(yVal), levels(yVal))
  
  if(is.null(colors)) colors <- c("#009392FF","#39B185FF","#9CCB86FF","#E9E29CFF","#EEB479FF","#E88471FF","#CF597EFF")  # rcartocolor::Temps
  if(direction==-1) colors <- rev(colors)
  
  if(is.null(palette)) 
    if(nlevels(yVal)<=10) {
      palette <- c("#4E79A7FF","#F28E2BFF","#E15759FF","#59A14FFF","#EDC948FF",
                   "#B07AA1FF","#FF9DA7FF","#9C755FFF","#BAB0ACFF","#76B7B2FF")  # ggthemes::Tableau_10
    } else {
      palette <- c("#4E79A7FF","#A0CBE8FF","#F28E2BFF","#FFBE7DFF","#59A14FFF",
                   "#8CD17DFF","#B6992DFF","#F1CE63FF","#499894FF","#86BCB6FF",
                   "#E15759FF","#FF9D9AFF","#79706EFF","#BAB0ACFF","#D37295FF",
                   "#FABFD2FF","#B07AA1FF","#D4A6C8FF","#9D7660FF","#D7B5A6FF")  # ggthemes::Tableau_20
    }
  if(length(palette) < nlevels(yVal)) stop("The number of colors in palette should be equal or higher to the number of levels in y")
  palette <- palette[1:nlevels(yVal)]
  if(direction==-1) palette <- rev(palette)
  
  res <- assoc.twocat(x = xVal, y = yVal, weights = wVal, na.rm = TRUE)$gather
  res <- res[order(rev(res$var.y), res$var.x),]
  res$x.center <- c(0, cumsum(res$prop.x)[1:nlevels(xVal) -1]) + res$prop.x / 2
  
  res$measure <- res[,measure]
  res$association <- factor(sign(res$measure), labels = c("repulsion","attraction"))
  
  labs.x <- res[res$var.y==levels(res$var.y)[1],]
  labs.y <- res[res$var.x==levels(res$var.x)[1],]
  labs.y$y.center <- cumsum(labs.y$rprop) - 0.5*labs.y$rprop
  labs.y2 <- res[res$var.x==levels(res$var.x)[nlevels(res$var.x)],]
  labs.y2$y.center <- cumsum(labs.y2$rprop) - 0.5*labs.y2$rprop
  
  if (type=="classic") {
    p <- 
      ggplot2::ggplot(res, ggplot2::aes(x = .data$x.center, y = .data$rprop, width = .data$prop.x, fill = .data$var.y)) +
        ggplot2::geom_bar(stat = "identity", col = linecolor, linewidth = linewidth) +
        ggplot2::scale_fill_discrete(type = palette, labels = sub(".","",levels(yVal))) +
        ggplot2::labs(x = xName, y = yName, fill = yName)
  } else if (type=="shades") {
    p <- 
      ggplot2::ggplot(res, ggplot2::aes(x = .data$x.center, y = .data$rprop, width = .data$prop.x, fill = .data$measure)) +
        ggplot2::geom_bar(stat = "identity", col = linecolor, linewidth = linewidth) +
        ggplot2::scale_fill_gradientn(colours = colors, limits = limits) +
        ggplot2::geom_text(data = labs.y2, ggplot2::aes(label = sub(".","",as.character(.data$var.y)), y = .data$y.center, x = 1.05), size = ggplot2::rel(3), vjust = "top", angle = -90) +
        ggplot2::labs(x = xName, y = yName, fill = measure)
  } else if (type=="patterns") {
    p <- 
      ggplot2::ggplot(res, ggplot2::aes(x = .data$x.center, y = .data$rprop, width = .data$prop.x)) +
        ggpattern::geom_bar_pattern(stat = "identity", 
                                    ggplot2::aes(fill = .data$var.y, 
                                                 pattern = .data$association,
                                                 pattern_spacing = -abs(.data$measure)),
                                    pattern_colour = "black", pattern_fill = "black", pattern_alpha = 0.2,
                                    col = linecolor, linewidth = linewidth) +
        ggpattern::scale_pattern_spacing_continuous() +
        ggplot2::scale_fill_discrete(type = palette, labels = sub(".","",levels(yVal))) +
        ggplot2::labs(x = xName, y = yName) +
        ggplot2::guides(fill = "none", pattern_spacing = "none")
  } 
  
  p <- p +
    ggplot2::scale_x_continuous(labels = NULL) +
    ggplot2::scale_y_continuous(labels = NULL) +
    geom_text(data = labs.x, aes(label = sub(".","",as.character(.data$var.x)), x = .data$x.center, y = -0.05), size = ggplot2::rel(3), vjust = "inward") +
    geom_text(data = labs.y, aes(label = sub(".","",as.character(.data$var.y)), y = .data$y.center, x = -0.05), size = ggplot2::rel(3), vjust = "top", angle = 90) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = legend,
                   panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank())
    
  return(p)
}


# ggassoc_marimekko(Movies, aes(Country, Genre), na.rm= FALSE)
# ggassoc_marimekko(Movies, aes(Country, Genre), na.rm= TRUE)
# ggassoc_marimekko(Movies, aes(x = Country, y = Genre, weight = Critics), na.rm= FALSE)
# ggassoc_marimekko(Movies, aes(x = Country, y = Genre, weight = Critics), na.rm= TRUE)
# 
# MoviesNA <- Movies
# MoviesNA$CountryNA <- MoviesNA$Country
# MoviesNA$GenreNA <- MoviesNA$Genre
# MoviesNA$CriticsNA <- MoviesNA$Critics
# MoviesNA$CountryNA[c(1,3,5)] <- NA
# MoviesNA$GenreNA[c(2,4,6)] <- NA
# MoviesNA$CriticsNA[7] <- NA
# 
# ggassoc_marimekko(MoviesNA, aes(CountryNA, Genre), na.rm= FALSE)
# ggassoc_marimekko(MoviesNA, aes(CountryNA, Genre), na.rm= TRUE)
# ggassoc_marimekko(MoviesNA, aes(x = Country, y = Genre, weight = CriticsNA), na.rm= FALSE)
# ggassoc_marimekko(MoviesNA, aes(x = Country, y = Genre, weight = CriticsNA), na.rm= TRUE)



# ggassoc_marimekko(Movies, aes(Genre, Country), type = "classic", direction = -1)
# ggassoc_marimekko(Movies, aes(Genre, Country), type = "shades", sort = "both", colors = c("pink","white","purple"), limits = c(-0.5,0.5))
# ggassoc_marimekko(Movies, aes(Genre, Country), type = "patterns", sort = "both")
# ggassoc_marimekko(Movies, aes(Genre, Country), type = "patterns", limits = c(-0.9,0.9), sort = "both")
# ggassoc_marimekko(Movies, aes(x = Genre, y = Country, weight = Critics), type = "classic", direction = -1)
