#' Plotting tools for Discrete effectiveness distributions
#'
#' Plot the density, distribution and quantile functions of a discrete effectiveness distribution.
#'
#' @param x the effectiveness distribution to plot.
#' @param plot.data logical: whether to plot the data used to fit the distribution, if any.
#' @param xlab the title for the x axis.
#' @param ylab the title for the y axis.
#' @param main the overall title for the plot.
#' @param ... arguments to be passed to \code{\link[graphics]{lines}}.
#' @seealso \code{\link{plot.eff.cont}} for continuous distributions.
#' @name plot.eff.disc
NULL

#' @rdname plot.eff.disc
#' @export
dplot.eff.disc <- function(x, ..., plot.data = TRUE,
                           xlab = "x", ylab = "f(x)", main = "mass") {
  .eff <- x
  if(plot.data && is.null(.eff$data)) {
    warning("no data to plot.")
    plot.data <- FALSE
  }

  x <- .eff$support
  y <- deff(x, .eff)
  ylim = c(0, max(y))
  if(plot.data) {
    y_data <- as.numeric(table(factor(.eff$data, levels = .eff$support))) / length(.eff$data)
    ylim = range(ylim, y_data)
  }

  graphics::plot(NA, xlab = xlab, ylab = ylab, main = main, xlim = 0:1, ylim = ylim, ...)
  graphics::abline(h = 0, col = col.data, lty = 2)

  if(plot.data) {
    graphics::lines(x, y_data, type = "b", col = col.data, pch = 19)
    graphics::rug(mean(.eff$data), side = 1, col = col.data)
  }

  graphics::lines(x, y, type = "b", pch = 19, ...)
  graphics::rug(.eff$mean, side = 1, ...)
}
#' @rdname plot.eff.disc
#' @export
pplot.eff.disc <- function(x, ..., plot.data = TRUE,
                           xlab = "q", ylab = "F(q)", main = "distribution") {
  .eff <- x
  if(plot.data && is.null(.eff$data)) {
    warning("no data to plot.")
    plot.data <- FALSE
  }

  x <- .eff$support
  y <- peff(x, .eff)
  pfun <- stats::stepfun(x, c(0, y), ties = "ordered")

  graphics::plot(NA, xlab = xlab, ylab = ylab, main = main, xlim = 0:1, ylim = 0:1, ...)
  graphics::abline(h = 0:1, col = col.data, lty = 2)

  if(plot.data) {
    graphics::lines(stats::ecdf(.eff$data), col = col.data, pch = 19, cex = 1, col.01line = NA)
    graphics::rug(mean(.eff$data), side = 1, col = col.data)
  }

  graphics::lines(pfun, verticals = FALSE, pch = 19, cex = 1, ...)
  graphics::rug(.eff$mean, side = 1, ...)
}
#' @rdname plot.eff.disc
#' @export
qplot.eff.disc <- function(x, ..., plot.data = TRUE,
                           xlab = "p", ylab = expression(F^-1*(p)), main = "quantile") {
  .eff <- x
  if(plot.data && is.null(.eff$data)) {
    warning("no data to plot.")
    plot.data <- FALSE
  }

  x <- .eff$support
  y <- peff(x, .eff)
  qfun <- stats::stepfun(y[-length(y)], x, right = TRUE, ties = "ordered")

  graphics::plot(NA, xlab = xlab, ylab = ylab, main = main, xlim = 0:1, ylim = 0:1, ...)
  graphics::abline(v = 0:1, col = col.data, lty = 2)

  if(plot.data) {
    x_data <- sort(unique(.eff$data))
    y_data <- stats::ecdf(.eff$data)(x_data)
    qfun_data <- stats::stepfun(y_data[-length(y_data)], x_data, right = TRUE, ties = "ordered")
    graphics::lines(qfun_data, col = col.data, pch = 19, cex = 1, verticals = FALSE)
    graphics::rug(mean(.eff$data), side = 1, col = col.data)
  }

  graphics::lines(qfun, pch = 19, cex = 1, verticals = FALSE, ...)
  graphics::rug(.eff$mean, side = 2, ...)
}
