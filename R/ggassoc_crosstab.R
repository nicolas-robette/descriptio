ggassoc_crosstab <- function(data, mapping,
                             size = "freq", max.size = 20, 
                             measure = "phi", limit = NULL, sort = "none",
                             palette = "PRGn", direction = 1, legend = "right") {
  
  mapping$colour <- ggplot2::aes_string(colour = sprintf("ggplot2::after_stat(%s)", measure))$colour
  mapping$size <- ggplot2::aes_string(size = sprintf("sqrt(ggplot2::after_stat(%s))", size))$size
  
  if(!is.null(limit)) limit <- c(-limit, limit)
  
  xVal <- rlang::eval_tidy(mapping$x, data)
  yVal <- rlang::eval_tidy(mapping$y, data)
  xName <- rlang::as_name(mapping$x)
  yName <- rlang::as_name(mapping$y)
  
  # data <- data[!is.na(data[,xName]) & !is.na(data[,yName]),]
  # data[,xName] <- factor(data[,xName])
  # data[,yName] <- factor(data[,yName])
  
  if(sort!="none") {
    temp <- MASS::corresp(~xVal+yVal,nf=1)
    if(sort %in% c("x","both")) data[,xName] <- factor(data[,xName], levels=names(sort(temp$rscore)))
    if(sort %in% c("y","both")) data[,yName] <- factor(data[,yName], levels=names(sort(temp$cscore)))
  }
  
  geom_point_args <- list()
  geom_point_args$stat <- "twocat"
  geom_point_args$shape <- "square"
  
  ggplot2::ggplot(data, mapping) +
    # ggplot2::geom_point(stat = "twocat",
    #                     shape = "square") +
    do.call(geom_point, geom_point_args) +
    # stat_twocat(geom = "point", 
    #                    shape = "square",
    #                    keep.zero.cells = TRUE) +
    ggplot2::scale_size(guide = "none", range = c(0, max.size)) +
    ggplot2::scale_color_distiller(palette = palette, direction = direction, limit = limit) +
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

# ggassoc_crosstab(Taste, aes(Age, Educ), max.size = 20)
# ggassoc_crosstab(Taste, aes(Age, Educ), max.size = 20) + facet_wrap(~ Gender)

