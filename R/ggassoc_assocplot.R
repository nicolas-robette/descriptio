ggassoc_assocplot <- function(data, mapping, measure = "std.residuals", limit = NULL,
                              sort="none", palette = "PRGn", direction = 1, legend = "right") {
  
  xVal <- rlang::eval_tidy(mapping$x, data)
  yVal <- rlang::eval_tidy(mapping$y, data)
  xName <- rlang::as_name(mapping$x)
  yName <- rlang::as_name(mapping$y)
  
  if(sort!="none") {
    temp <- MASS::corresp(~xVal+yVal,nf=1)
    if(sort %in% c("x","both")) xVal <- factor(xVal, levels=names(sort(temp$rscore)))
    if(sort %in% c("y","both")) yVal <- factor(yVal, levels=names(sort(temp$cscore)))
  }
  
  if(!is.null(limit)) limit <- c(-limit, limit)
  
  mapping$xmin <- ggplot2::aes_string(xmin = "as.numeric(after_stat(x)) - sqrt(after_stat(expected))*0.45/sqrt(max(abs(after_stat(expected))))")$xmin
  mapping$xmax <- ggplot2::aes_string(xmax = "as.numeric(after_stat(x)) + sqrt(after_stat(expected))*0.45/sqrt(max(abs(after_stat(expected))))")$xmax
  mapping$ymin <- ggplot2::aes_string(ymin = "as.numeric(after_stat(y))")$ymin
  mapping$ymax <- ggplot2::aes_string(ymax = sprintf("as.numeric(after_stat(y)) + after_stat(%s)*0.5/(1.1*max(abs(after_stat(%s))))", measure, measure))$ymax
  mapping$fill <- ggplot2::aes_string(fill = sprintf("after_stat(%s)", measure))$fill
  
  ggplot2::ggplot(data, mapping) +
    geom_rect(stat = "twocat") +
    ggplot2::scale_fill_distiller(palette = palette, direction = direction, limits = limit) +
    ggplot2::xlab(xName) +
    ggplot2::ylab(yName) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = legend,
                   panel.grid.major.x = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_text(vjust = 5))
}

# ggassoc_assocplot(Taste, aes(Age, Educ))
# ggassoc_assocplot(Taste, aes(Age, Educ)) + facet_wrap(~ Gender)