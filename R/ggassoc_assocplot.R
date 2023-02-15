ggassoc_assocplot <- function(data, mapping, measure = "std.residuals", limits = NULL, sort = "none",
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
  
  mapping$xmin <- ggplot2::aes_string(xmin = "as.numeric(after_stat(x)) - sqrt(after_stat(expected))*0.45/sqrt(max(abs(after_stat(expected))))")$xmin
  mapping$xmax <- ggplot2::aes_string(xmax = "as.numeric(after_stat(x)) + sqrt(after_stat(expected))*0.45/sqrt(max(abs(after_stat(expected))))")$xmax
  mapping$ymin <- ggplot2::aes_string(ymin = "as.numeric(after_stat(y))")$ymin
  mapping$ymax <- ggplot2::aes_string(ymax = sprintf("as.numeric(after_stat(y)) + after_stat(%s)*0.5/(1.1*max(abs(after_stat(%s))))", measure, measure))$ymax
  mapping$fill <- ggplot2::aes_string(fill = sprintf("after_stat(%s)", measure))$fill
  
  ggplot2::ggplot(data, mapping) +
    ggplot2::geom_rect(stat = "twocat") +
    ggplot2::scale_fill_gradientn(colours = colors, limits = limits, name = measure) +
    ggplot2::xlab(xName) +
    ggplot2::ylab(yName) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = legend,
                   panel.grid.major.x = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_text(vjust = 5))
}

# ggassoc_assocplot(Movies, aes(Country, Genre), na.rm= FALSE)
# ggassoc_assocplot(Movies, aes(Country, Genre), na.rm= TRUE)
# ggassoc_assocplot(Movies, aes(x = Country, y = Genre, weight = Critics), na.rm= FALSE)
# ggassoc_assocplot(Movies, aes(x = Country, y = Genre, weight = Critics), na.rm= TRUE)
# 
# MoviesNA <- Movies
# MoviesNA$CountryNA <- MoviesNA$Country
# MoviesNA$GenreNA <- MoviesNA$Genre
# MoviesNA$CriticsNA <- MoviesNA$Critics
# MoviesNA$CountryNA[c(1,3,5)] <- NA
# MoviesNA$GenreNA[c(2,4,6)] <- NA
# MoviesNA$CriticsNA[7] <- NA
# 
# ggassoc_assocplot(MoviesNA, aes(CountryNA, Genre), na.rm= FALSE)
# ggassoc_assocplot(MoviesNA, aes(CountryNA, Genre), na.rm= TRUE)
# ggassoc_assocplot(MoviesNA, aes(x = Country, y = Genre, weight = CriticsNA), na.rm= FALSE)
# ggassoc_assocplot(MoviesNA, aes(x = Country, y = Genre, weight = CriticsNA), na.rm= TRUE)


# ===========================================
# remplacement de aes_string (deprecated) ? 
# str(mapping)
# str(mapping$x)
# mapping$ymin <- aes(ymin = !!sym("as.numeric(after_stat(y))"))$ymin
# ===========================================


# ===========================================
# paletteer_d("rcartocolor::Temps")
# paletteer_d("rcartocolor::Bold")
# paletteer_d("ggthemes::Tableau_10")
# paletteer_d("ggthemes::Tableau_20")
# paletteer_d("khroma::bright")
# paletteer_d("rcartocolor::Temps") %>% str()
# paletteer::paletteer_c("viridis::plasma", 200)
# paletteer::paletteer_dynamic("cartography::blue.pal", 20)
# 
# palettes_dynamic_names
# palettes_c_names
# palettes_d_names
# palettes_d_names %>% filter(package == "ggthemes")
# ===========================================