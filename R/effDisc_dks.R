#' Discrete Effectiveness as Discrete Kernel-smoothed Distribution.
#'
#' Fits a Beta-Binomial distribution, to the given sample of scores and support points.
#'
#' @param x a sample of effectiveness scores between 0 and 1.
#' @param support the support of the distribution.
#' @param mult a constant to multiply the initially selected bandwidth.
#' @return an object of class \code{eff.disc.dks}, which inherits from
#'   \code{\link[=eff.disc-class]{eff.disc}}.
#' @seealso \code{\link{deff}}, \code{\link{peff}}, \code{\link{qeff}} and \code{\link{reff}}.
#' @references M.C. Wang and J.V. Ryzing (1981). A Class of Smooth Estimators for Discrete
#'   Distributions. Biometrika, 68, 301-309.
#' @examples
#' e <- effDisc_dks(web2010p20[,1], seq(0,1,.05))
#' c(e$mean, e$var)
#' plot(e, plot.data = TRUE)
#' e2 <- effDisc_dks(web2010p20[,1], seq(0,1,.05), mult = 2)
#' c(e2$mean, e2$var)
#' plot(e2, plot.data = TRUE)
#' @export
effDisc_dks <- function(x, support, mult = 1) {
  support <- sort(support)

  x_i <- matchTol(x, support)
  support_i <- seq_along(support)
  bw <- np::npudensbw(ordered(x_i, support_i), bwmethod = "cv.ls", okertype = "wangvanryzin")
  bw$bw <- bw$bw * mult
  bw$bandwidth$x <- bw$bandwidth$x * mult

  if (bw$bw > 1)
    stop("(multiplied) bandwidth too large, use a smaller multiplier")

  d <- stats::predict(np::npudens(bw), se.fit = FALSE, as.ordered(support_i))
  d <- d / sum(d)
  p <- cumsum(d)
  df <- mean((1 - bw$bw) / d[x_i])

  e <- effDisc_new(p, support, df, x)
  e$model <- list(type = paste0("dks(", mult, ")"), bw = bw, mult = mult)
  class(e) <- c("eff.disc.dks", class(e))
  e
}
