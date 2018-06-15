#' Plotting tools for Continuous effectiveness distributions
#'
#' Plot the density, distribution and quantile functions of a continuous effectiveness distribution.
#'
#' @param x the effectiveness distribution to plot.
#' @param plot.data logical: whether to plot the data used to fit the distribution, if any.
#' @param subdivisions number of equidistant points at which to evaluate the distribution to plot.
#' @param xlab the title for the x axis.
#' @param ylab the title for the y axis.
#' @param main the overall title for the plot.
#' @param ... arguments to be passed to \code{\link[graphics]{lines}}.
#' @seealso \code{\link{plot.eff.disc}} for discrete distributions.
#' @name plot.eff.cont
NULL

#' @rdname plot.eff.cont
#' @export
dplot.eff.cont <- function(x, ..., plot.data = TRUE, subdivisions = 200,
                           xlab = "x", ylab = "f(x)", main = "density") {
  .eff <- x
  if(plot.data && is.null(.eff$data)) {
    warning("no data to plot.")
    plot.data <- FALSE
  }

  x <- seq(1e-4, 1-1e-4, length.out = subdivisions)
  y <- deff(x, .eff)
  ylim = c(0, max(y))
  if(plot.data) {
    h <- graphics::hist(.eff$data, plot = FALSE)
    ylim = range(ylim, h$density)
  }

  graphics::plot(NA, xlab = xlab, ylab = ylab, main = main, xlim = 0:1, ylim = ylim, ...)
  graphics::abline(h = 0, col = col.data, lty = 2)

  if(plot.data) {
    graphics::hist(.eff$data, probability = TRUE, border = col.data, add = TRUE, axes = FALSE)
    graphics::rug(mean(.eff$data), side = 1, col = col.data)
  }

  graphics::lines(x, y, type = "l", ...)
  graphics::rug(.eff$mean, side = 1, ...)
}
#' @rdname plot.eff.cont
#' @export
pplot.eff.cont <- function(x, ..., plot.data = TRUE, subdivisions = 200,
                           xlab = "q", ylab = "F(q)", main = "distribution") {
  .eff <- x
  if(plot.data && is.null(.eff$data)) {
    warning("no data to plot.")
    plot.data <- FALSE
  }

  x <- seq(1e-4, 1-1e-4, length.out = subdivisions)
  y <- peff(x, .eff)

  graphics::plot(NA, xlab = xlab, ylab = ylab, main = main, xlim = 0:1, ylim = 0:1, ...)
  graphics::abline(h = 0:1, col = col.data, lty = 2)

  if(plot.data) {
    graphics::lines(stats::ecdf(.eff$data), col = col.data, pch = 19, cex = 1, col.01line = NA)
    graphics::rug(mean(.eff$data), side = 1, col = col.data)
  }

  graphics::lines(x, y, ...)
  graphics::rug(.eff$mean, side = 1, ...)
}
#' @rdname plot.eff.cont
#' @export
qplot.eff.cont <- function(x, ..., plot.data = TRUE, subdivisions = 200,
                           xlab = "p", ylab = expression(F^-1*(p)), main = "quantile") {
  .eff <- x
  if(plot.data && is.null(.eff$data)) {
    warning("no data to plot.")
    plot.data <- FALSE
  }

  x <- seq(0, 1, length.out = subdivisions)
  y <- qeff(x, .eff)

  graphics::plot(NA, xlab = xlab, ylab = ylab, main = main, xlim = 0:1, ylim = 0:1, ...)
  graphics::abline(v = 0:1, col = col.data, lty = 2)

  if(plot.data) {
    x_data <- sort(unique(.eff$data))
    y_data <- stats::ecdf(.eff$data)(x_data)
    qfun <- stats::stepfun(y_data[-length(y_data)], x_data, right = TRUE, ties = "ordered")
    graphics::lines(qfun, col = col.data, pch = 19, cex = 1, verticals = FALSE)
    graphics::rug(mean(.eff$data), side = 2, col = col.data)
  }

  graphics::lines(x, y, ...)
  graphics::rug(.eff$mean, side = 2, ...)
}
