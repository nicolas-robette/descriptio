# data = Movies
# mapping = aes(Genre, Country)

ggassoc_marimekko <- function(data, mapping, type = "classic",
                              measure = "phi", limit = NULL, 
                              palette = NULL, direction = 1,
                              linecolor = "gray60", linewidth = 0.1,
                              sort = "none", legend = "right") {
  
  if(is.null(palette)) {
    if(type %in% c("classic", "patterns")) {
      palette <- "khroma::bright"
    } else if(type=="shades") {
      palette <- "PRGn"
    }
  }
  
  xVal <- rlang::eval_tidy(mapping$x, data)
  yVal <- rlang::eval_tidy(mapping$y, data)
  xName <- rlang::as_name(mapping$x)
  yName <- rlang::as_name(mapping$y)

  if(sort!="none") {
    temp <- MASS::corresp(~xVal+yVal,nf=1)
    if(sort %in% c("x","both")) xVal <- factor(xVal, levels=names(sort(temp$rscore)))
    if(sort %in% c("y","both")) yVal <- factor(yVal, levels=names(sort(temp$cscore)))
  }
  
  levels(xVal) <- paste0(1:nlevels(xVal), levels(xVal))
  levels(yVal) <- paste0(1:nlevels(yVal), levels(yVal))
  
  if(!is.null(limit)) limit <- c(-limit, limit)
  
  res <- assoc.twocat(xVal, yVal)$gather
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
        paletteer::scale_fill_paletteer_d(palette = palette, labels = sub(".","",levels(yVal))) +
        ggplot2::labs(x = xName, y = yName, fill = yName)
  } else if (type=="shades") {
    p <- 
      ggplot2::ggplot(res, ggplot2::aes(x = .data$x.center, y = .data$rprop, width = .data$prop.x, fill = .data$measure)) +
        ggplot2::geom_bar(stat = "identity", col = linecolor, linewidth = linewidth) +
        ggplot2::scale_fill_distiller(palette = palette, direction = direction, limits = limit) +
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
        ggpattern::scale_pattern_spacing_continuous(limits = limit) +
        paletteer::scale_fill_paletteer_d(palette = palette, labels = sub(".","",levels(yVal))) +
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

  # ggplot2::ggplot(res, ggplot2::aes(x = x.center, y = rprop, width = prop.x, fill = var.y)) +
  #   ggplot2::geom_bar(stat = "identity", col = linecolor, linewidth = linewidth) +
  #   paletteer::scale_fill_paletteer_d(palette = palette, labels = sub(".","",levels(yVal))) +
  #   # ggplot2::scale_x_continuous(labels = NULL) +
  #   # ggplot2::scale_y_continuous(labels = NULL) +
  #   # geom_text(data = labs.x, aes(label = sub(".","",as.character(var.x)), x = x.center, y = -0.05), size = ggplot2::rel(3), vjust = "inward") +
  #   # geom_text(data = labs.y, aes(label = sub(".","",as.character(var.y)), y = y.center, x = -0.05), size = ggplot2::rel(3), vjust = "top", angle = 90) +
  #   # geom_text(data = labs.y2, aes(label = sub(".","",as.character(var.y)), y = y.center, x = 1.05), size = ggplot2::rel(3), vjust = "top", angle = -90) +
  #   ggplot2::labs(x = xName, y = yName, fill = yName) +
  #   # ggplot2::theme_minimal() +
  #   # ggplot2::theme(legend.position = legend,
  #   #                panel.grid.major = ggplot2::element_blank(),
  #   #                panel.grid.minor = ggplot2::element_blank())
  #   
  # ggplot2::ggplot(res, ggplot2::aes(x = x.center, y = rprop, width = prop.x, fill = measure)) +
  #   ggplot2::geom_bar(stat = "identity", col = linecolor, linewidth = linewidth) +
  #   ggplot2::scale_fill_distiller(palette = palette, direction = direction, limits = limit) +
  #   # ggplot2::scale_x_continuous(labels = NULL) +
  #   # ggplot2::scale_y_continuous(labels = NULL) +
  #   # ggplot2::geom_text(data = labs.x, ggplot2::aes(label = sub(".","",as.character(var.x)), x = x.center, y = -0.05), size = ggplot2::rel(3), vjust = "inward") +
  #   # ggplot2::geom_text(data = labs.y, ggplot2::aes(label = sub(".","",as.character(var.y)), y = y.center, x = -0.05), size = ggplot2::rel(3), vjust = "top", angle = 90) +
  #   ggplot2::geom_text(data = labs.y2, ggplot2::aes(label = sub(".","",as.character(var.y)), y = y.center, x = 1.05), size = ggplot2::rel(3), vjust = "top", angle = -90) +
  #   ggplot2::labs(x = xName, y = yName, fill = measure) +
  #   # ggplot2::theme_minimal() +
  #   # ggplot2::theme(legend.position = legend,
  #   #                panel.grid.major = ggplot2::element_blank(),
  #   #                panel.grid.minor = ggplot2::element_blank())
  #   
  # ggplot2::ggplot(res, ggplot2::aes(x = x.center, y = rprop, width = prop.x)) +
  #   ggpattern::geom_bar_pattern(stat = "identity", 
  #                               ggplot2::aes(fill = var.y, 
  #                                            pattern = association,
  #                                            pattern_spacing = -abs(measure)),
  #                               pattern_colour = "black", pattern_fill = "black", pattern_alpha = 0.2,
  #                               col = linecolor, linewidth = linewidth) +
  #   paletteer::scale_fill_paletteer_d(palette = palette, labels = sub(".","",levels(yVal))) +
  #   # ggplot2::scale_x_continuous(labels = NULL) +
  #   # ggplot2::scale_y_continuous(labels = NULL) +
  #   # ggplot2::geom_text(data = labs.x, ggplot2::aes(label = sub(".","",as.character(var.x)), x = x.center, y = -0.05), size = ggplot2::rel(3), vjust = "inward") +
  #   ggplot2::geom_text(data = labs.y, ggplot2::aes(label = sub(".","",as.character(var.y)), y = y.center, x = -0.05), size = ggplot2::rel(3), vjust = "top", angle = 90) +
  #   ggplot2::geom_text(data = labs.y2, ggplot2::aes(label = sub(".","",as.character(var.y)), y = y.center, x = 1.05), size = ggplot2::rel(3), vjust = "top", angle = -90) +
  #   ggplot2::labs(x = xName, y = yName) +
  #   # ggplot2::theme_minimal() +
  #   # ggplot2::theme(legend.position = legend,
  #   #                panel.grid.major = ggplot2::element_blank(),
  #   #                panel.grid.minor = ggplot2::element_blank()) +
  #   ggplot2::guides(fill = "none", pattern_spacing = "none")
  
}

# ggassoc_marimekko(Movies, aes(Genre, Country), type = "classic")
# ggassoc_marimekko(Movies, aes(Genre, Country), type = "shades")
# ggassoc_marimekko(Movies, aes(Genre, Country), type = "patterns")
# ggassoc_marimekko(Movies, aes(Genre, Country), type = "patterns", limit = 0.9)
