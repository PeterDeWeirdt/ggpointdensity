#' @import ggplot2


count_neighbors <- function(x, y, r2, xy) {
  .Call("count_neighbors_", x, y, r2, xy, "ggpointdensity")
}

#' Implementation of count_neighbors in R. Not actually used, just for clarity
count_neighbors_r <- function(x, y, r2, xy) {
  yx <- 1 / xy
  sapply(1:length(x), function(i) {
    sum((yx * (x[i] - x) ^ 2) + (xy * (y[i] - y) ^ 2) < r2)
  })
}

# From Kamil Slowikowski: https://slowkow.com/notes/ggplot2-color-by-density/
# Get density of points in 2 dimensions.
# @param x A numeric vector.
# @param y A numeric vector.
# @param n Create a square n by n grid to compute density.
# @return The density within each square.
get_density <- function(x, y, ...) {
  dens <- MASS::kde2d(x, y, ...)
  ix <- findInterval(x, dens$x)
  iy <- findInterval(y, dens$y)
  ii <- cbind(ix, iy)
  return(dens$z[ii])
}

StatPointkde <- ggproto("StatPointkde", Stat,
                        default_aes = aes(color = stat(density)),
                        required_aes = c("x", "y"),

                        compute_group = function(data, scales, adjust = 1) {
                          adjust <- ggplot2:::dual_param(adjust, list(x = 1, y = 1))

                          # get kde around each point.
                          # Increase n for higher resolution
                          data$density <- get_density(
                            data$x, data$y, n = 300)

                          data
                        }
)

stat_pointkde <- function(mapping = NULL,
                          data = NULL,
                          geom = "point",
                          position = "identity",
                          ...,
                          adjust = 1,
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatPointkde,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      adjust = 1,
      na.rm = na.rm,
      ...
    )
  )
}

stat_pointdensity <- function(mapping = NULL,
                              data = NULL,
                              geom = "point",
                              position = "identity",
                              ...,
                              adjust = 1,
                              na.rm = FALSE,
                              show.legend = NA,
                              inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatPointdensity,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      adjust = 1,
      na.rm = na.rm,
      ...
    )
  )
}

StatPointdensity <- ggproto("StatPointdensity", Stat,
                            default_aes = aes(color = stat(n_neighbors)),
                            required_aes = c("x", "y"),

                            compute_group = function(data, scales, adjust = 1) {
                              adjust <- ggplot2:::dual_param(adjust, list(x = 1, y = 1))

                              # find an appropriate bandwidth (radius), pretty ad-hoc:
                              xrange <- diff(scales$x$get_limits()) * adjust$x
                              yrange <- diff(scales$y$get_limits()) * adjust$y
                              r2 <- (xrange + yrange) / 70

                              # since x and y may be on different scales, we need a
                              # factor to weight x and y distances accordingly:
                              xy <- xrange / yrange

                              # counting the number of neighbors around each point,
                              # this will be used to color the points
                              data$n_neighbors <- count_neighbors(
                                data$x, data$y, r2 = r2, xy = xy)

                              data
                            }
)

geom_pointdensity <- function(mapping = NULL,
                              data = NULL,
                              stat = "pointdensity",
                              position = "identity",
                              ...,
                              na.rm = FALSE,
                              show.legend = NA,
                              inherit.aes = TRUE) {

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = ggplot2::GeomPoint,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}
