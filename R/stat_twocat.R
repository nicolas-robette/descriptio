StatTwocat <- ggplot2::ggproto(
  "StatTwocat",
  ggplot2::Stat,
  required_aes = c("x", "y"),
  default_aes = ggplot2::aes(weight = 1),
  
  # setup_params = function(data, params) {
  #   params
  # },
  # extra_params = c("na.rm"),
  
  compute_panel = function(self, data, scales) {
    
    if (is.null(data$weight)) {
      data$weight <- rep(1, nrow(data))
    }
    
    # compute crosstabulation statistics
    panel <- with(data, assoc.twocat(x, y, weights = weight)$gather)
    names(panel)[1:2] <- c("y","x")

    # to handle the fact that ggplot2 could transform factors into integers
    # before computation of the statistic
    if (is.numeric(data$x)) panel$x <- as.numeric(panel$x)
    if (is.numeric(data$y)) panel$y <- as.numeric(panel$y)
    
    # keeping first value of other aesthetics in data
    # panel <- merge(
    #   panel,
    #   # dplyr::select(data, -dplyr::all_of("PANEL")),
    #   data,
    #   by = c("x", "y"),
    #   all.x = TRUE
    # )
    
    # dups <- duplicated(panel[,c("x","y")])
    # panel <- panel[!dups,]
    # panel <- panel %>% dplyr::distinct(.data$x, .data$y, .keep_all = TRUE)
    
    # if (!keep.zero.cells) {
    #   panel <- panel[panel$freq != 0, ]
    # }
    
    panel
  }
)


stat_twocat <- function(mapping = NULL, 
                        data = NULL,
                        geom = "point",
                        position = "identity",
                        ...,
                        show.legend = NA,
                        inherit.aes = TRUE) {

  layer(
    data = data,
    mapping = mapping,
    stat = StatTwocat,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes
  )
}
