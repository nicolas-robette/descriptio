ggassoc_phiplot <- function(data, mapping, measure = "phi", limit = NULL, sort = "none") {
  
  xVal <- rlang::eval_tidy(mapping$x, data)
  yVal <- rlang::eval_tidy(mapping$y, data)
  xName <- rlang::as_name(mapping$x)
  yName <- rlang::as_name(mapping$y)
  
  mapping$xmin <- ggplot2::aes_string(xmin = "as.numeric(after_stat(x)) - after_stat(prop.x)")$xmin
  mapping$xmax <- ggplot2::aes_string(xmax = "as.numeric(after_stat(x)) + after_stat(prop.x)")$xmax
  mapping$ymin <- ggplot2::aes_string(ymin = "as.numeric(after_stat(y))")$ymin
  if(is.null(limit)) {
    mapping$ymax <- ggplot2::aes_string(ymax = sprintf("as.numeric(after_stat(y)) + after_stat(%s)*0.5/(1.1*max(abs(after_stat(%s))))", measure, measure))$ymax
  } else {
    mapping$ymax <- ggplot2::aes_string(ymax = sprintf("as.numeric(after_stat(y)) + after_stat(%s)*0.5/limit", measure))$ymax
  }
  mapping$fill <- ggplot2::aes_string(fill = sprintf("factor(sign(after_stat(%s)))", measure))$fill
  
  if(sort!="none") {
    temp <- MASS::corresp(~xVal+yVal,nf=1)
    if(sort %in% c("x","both")) data[,xName] <- factor(data[,xName], levels=names(sort(temp$rscore)))
    if(sort %in% c("y","both")) data[,yName] <- factor(data[,yName], levels=names(sort(temp$cscore)))
  }
  
  ggplot(data, mapping) +
    geom_rect(stat = "twocat", col = "black", linewidth = 0.2) +
    ggplot2::scale_fill_manual(values = c("white","black"), guide = "none") +
    ggplot2::xlab(xName) +
    ggplot2::ylab(yName) +
    # ggplot2::scale_x_continuous(breaks = 1:nlevels(xVal), labels = levels(xVal)) +
    # ggplot2::scale_y_continuous(breaks = 1:nlevels(yVal), labels = levels(yVal)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid.major.x = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_text(vjust = 5))
}

# ggassoc_phiplot(Taste, aes(Age, Educ))
# ggassoc_phiplot(Taste, aes(Age, Educ)) + facet_wrap(~ Gender)
