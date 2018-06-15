# color to use for the data in plot.eff
col.data <- "grey70"

#' Plotting tools for effectiveness distributions
#'
#' Plot the density, distribution and quantile functions of an effectiveness distribution. Function
#' \code{plot} plots all three functions in the same graphics device.
#'
#' @param x the effectiveness distribution to plot.
#' @param plot.data logical: whether to plot the data used to fit the distribution, if any.
#' @param ... other arguments to be passed to graphical functions.
#' @seealso \code{\link{plot.eff.cont}} and \code{\link{plot.eff.disc}} for more details.
#' @export
plot.eff <- function(x, ..., plot.data = TRUE) {
  prevpar <- graphics::par(mfrow = c(1, 3))

  dplot(x, plot.data = plot.data, ...)
  pplot(x, plot.data = plot.data, ...)
  qplot(x, plot.data = plot.data, ...)

  graphics::par(prevpar) # reset previous par
}
#' @rdname plot.eff
#' @export
dplot <- function(x, ..., plot.data = TRUE) {
  UseMethod("dplot", x)
}
#' @rdname plot.eff
#' @export
pplot <- function(x, ..., plot.data = TRUE) {
  UseMethod("pplot", x)
}
#' @rdname plot.eff
#' @export
qplot <- function(x, ..., plot.data = TRUE) {
  UseMethod("qplot", x)
}
