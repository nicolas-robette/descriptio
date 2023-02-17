ggassoc_scatter <- function(data, mapping, na.rm = FALSE, axes.labs = TRUE, ticks.labs = TRUE, text.size = 3) {

  xVal <- rlang::eval_tidy(mapping$x, data)
  yVal <- rlang::eval_tidy(mapping$y, data)
  wVal <- rlang::eval_tidy(mapping$weight, data)
  xName <- rlang::as_name(mapping$x)
  yName <- rlang::as_name(mapping$y)
  
  if(is.null(wVal)) mapping$weight <- ggplot2::aes_string(weight = sprintf("rep(1, length(%s))",xName))$weight
  if(any(is.na(wVal))) stop("There are empty values in weights.")
  
  assoc <- weighted.cor(xVal, yVal, weights = wVal, method = "kendall", na.rm = na.rm)
  
  p <- ggplot2::ggplot(data, mapping) +
          ggplot2::geom_point(alpha=.8, size=rel(0.5)) +
          ggplot2::geom_smooth(method="gam", se=FALSE, size=rel(.7)) + 
          ggplot2::theme_minimal() +
          ggplot2::theme(legend.position="none",
                         panel.grid.major=ggplot2::element_blank(),
                         panel.grid.minor=ggplot2::element_blank(),
                         panel.border = ggplot2::element_rect(
                           linetype = "solid",
                           color = "grey",
                           fill = "transparent"))
  
  if(!is.null(text.size)) p <- p + ggplot2::annotate(geom="label", label=paste0("tau = ",round(assoc,3)), x=-Inf, y=Inf, size=text.size,
                                                     hjust=0, vjust=1, label.size=NA, fill="white", alpha=.5)
  if(axes.labs) {
    p <- p + ggplot2::xlab(xName) + ggplot2::ylab(yName)
  } else {
    p <- p + ggplot2::xlab(NULL) + ggplot2::ylab(NULL)
  }
  if(!ticks.labs) p <- p + ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                                          axis.text.x = ggplot2::element_blank())
  p
}

# data(Movies)
# ggassoc_scatter(Movies, aes(x = Budget, y = BoxOffice), na.rm = TRUE)
# ggassoc_scatter(Movies, aes(x = Budget, y = BoxOffice), na.rm = FALSE)
# ggassoc_scatter(Movies, aes(x = Budget, y = BoxOffice, weight = Critics), na.rm = TRUE)
# ggassoc_scatter(Movies, aes(x = Budget, y = BoxOffice, weight = Critics), na.rm = FALSE)
# 
# MoviesNA <- Movies
# MoviesNA$BudgetNA <- MoviesNA$Budget
# MoviesNA$CriticsNA <- MoviesNA$Critics
# MoviesNA$BudgetNA[c(1,3,5)] <- NA
# MoviesNA$CriticsNA[7] <- NA
# 
# ggassoc_scatter(MoviesNA, aes(x = Budget, y = BoxOffice, weight = CriticsNA), na.rm = TRUE)
# ggassoc_scatter(MoviesNA, aes(x = Budget, y = BoxOffice, weight = CriticsNA), na.rm = FALSE)
# ggassoc_scatter(MoviesNA, aes(x = BudgetNA, y = BoxOffice, weight = Critics), na.rm = TRUE)
# ggassoc_scatter(MoviesNA, aes(x = BudgetNA, y = BoxOffice, weight = Critics), na.rm = FALSE)
