ggassoc_crosstab <- function(data, mapping,
                             size = "freq", max.size = 20, 
                             measure = "phi", limits = NULL, sort = "none",
                             na.rm = FALSE, na.value = "NA",
                             colors = NULL, direction = 1, legend = "right") {
  
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
  
  if(is.null(colors)) colors <- c("#009392FF","#39B185FF","#9CCB86FF","#E9E29CFF","#EEB479FF","#E88471FF","#CF597EFF")  # rcartocolor::Temps
  if(direction==-1) colors <- rev(colors)
  
  mapping$colour <- ggplot2::aes_string(colour = sprintf("ggplot2::after_stat(%s)", measure))$colour
  mapping$size <- ggplot2::aes_string(size = sprintf("sqrt(ggplot2::after_stat(%s))", size))$size
  
  geom_point_args <- list()
  geom_point_args$stat <- "twocat"
  geom_point_args$shape <- "square"
  
  ggplot2::ggplot(data, mapping) +
    do.call(geom_point, geom_point_args) +
    ggplot2::scale_size(guide = "none", range = c(0, max.size)) +
    ggplot2::scale_colour_gradientn(colours = colors, limits = limits, name = measure) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = legend,
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(vjust = 5),
      # axis.text.y = ggplot2::element_text(hjust = 2.5),
      # axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 0)),
      # axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 5)),
      legend.key.size = ggplot2::unit(0.5, 'cm'))
}

# ggassoc_crosstab(Movies, aes(Country, Genre), na.rm= FALSE)
# ggassoc_crosstab(Movies, aes(Country, Genre), na.rm= TRUE)
# ggassoc_crosstab(Movies, aes(x = Country, y = Genre, weight = Critics), na.rm= FALSE)
# ggassoc_crosstab(Movies, aes(x = Country, y = Genre, weight = Critics), na.rm= TRUE)
# 
# MoviesNA <- Movies
# MoviesNA$CountryNA <- MoviesNA$Country
# MoviesNA$GenreNA <- MoviesNA$Genre
# MoviesNA$CriticsNA <- MoviesNA$Critics
# MoviesNA$CountryNA[c(1,3,5)] <- NA
# MoviesNA$GenreNA[c(2,4,6)] <- NA
# MoviesNA$CriticsNA[7] <- NA
# 
# ggassoc_crosstab(MoviesNA, aes(CountryNA, Genre), na.rm= FALSE)
# ggassoc_crosstab(MoviesNA, aes(CountryNA, Genre), na.rm= TRUE)
# ggassoc_crosstab(MoviesNA, aes(x = Country, y = Genre, weight = CriticsNA), na.rm= FALSE)
# ggassoc_crosstab(MoviesNA, aes(x = Country, y = Genre, weight = CriticsNA), na.rm= TRUE)


# ggassoc_crosstab(Taste, aes(Age, Educ), max.size = 20)
# ggassoc_crosstab(Taste, aes(Age, Educ), max.size = 20) + facet_wrap(~ Gender)

