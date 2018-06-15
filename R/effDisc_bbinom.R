#' Discrete Effectiveness as Beta-Binomial Distribution.
#'
#' Fits a discrete kernel-smoothed distribution, to the given sample of scores and support points.
#'
#' @param x a sample of effectiveness scores between 0 and 1.
#' @param support the support of the distribution.
#' @return an object of class \code{eff.disc.bbinom}, which inherits from
#'   \code{\link[=eff.disc-class]{eff.disc}}.
#' @seealso \code{\link{deff}}, \code{\link{peff}}, \code{\link{qeff}} and \code{\link{reff}}.
#' @examples
#' e <- effDisc_bbinom(web2010p20[,1], seq(0,1,.05))
#' c(e$mean, e$var)
#' plot(e, plot.data = TRUE)
#' @export
effDisc_bbinom <- function(x, support) {
  support <- sort(support)

  n <- length(support)-1 # number of trials in binomial
  x_i <- matchTol(x, support) -1 # number of successes (0-based)
  support_i <- 0:n # integer support for the binomial

  # estimate parameters numerically, from initial values
  mu_0 <- mean(x_i)
  sigma2_0 <- stats::var(x_i)
  shape1 <- (n*mu_0 - sigma2_0) / (n*(sigma2_0/mu_0 - mu_0 -1) + mu_0)
  shape2 <- (n - mu_0)*(n - sigma2_0/mu_0) / (n*(sigma2_0/mu_0 - mu_0 -1) + mu_0)

  fit <- MASS::fitdistr(x_i, densfun = extraDistr::dbbinom,
                        start = list(alpha = shape1, beta = shape2),
                        lower = list(alpha = 1, beta = 1),
                        size = n)
  shape1 <- unname(fit$estimate[1])
  shape2 <- unname(fit$estimate[2])

  p <- extraDistr::pbbinom(support_i, n, shape1, shape2)
  e <- effDisc_new(p, support, 2, x)
  e$model <- list(type = "bbinom", n = n, shape1 = shape1, shape2 = shape2)
  class(e) <- c("eff.disc.bbinom", class(e))
  e
}