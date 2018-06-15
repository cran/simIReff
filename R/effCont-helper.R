#' Helper functions for continuous effectiveness distributions
#'
#' These are functions to help in the creation and use of continuous effectiveness distributions.
#'
#' \code{cap} caps (censor) a variable from below and above.
#'
#' \code{effContMean} computes the expected value of a distribution by numerical integration of the
#' given quantile function.
#'
#' \code{effContVar} computes the variance of a distribution by numerical integration of the given
#' quantile function.
#'
#' \code{effContTrun} computes the density, distribution and quantile functions of the distribution
#' resulting from truncating a given distribution between 0 and 1.
#'
#' @param x a sample of effectiveness scores.
#' @param xmin lowest value to cap scores.
#' @param xmax highest value to cap scores.
#' @param dfun a density function.
#' @param pfun a distribution function.
#' @param qfun a quantile function.
#' @param mu the expected value of the distribution (see \code{effContMean}).
#' @param abs.tol absolute accuracy requested, passed to \code{\link{integrate}}.
#' @param subdivisions the maximum number of subintervals, passed to \code{\link{integrate}}.
#' @param ... additional arguments passed to other functions, if any.
#' @return \code{cap}: the original vector, but censored.
#'
#'   \code{effContMean}: the estimate of the expected value.
#'
#'   \code{effContVar}: the estimate of the variance.
#'
#'   \code{effContTrunc}: a list with components:
#'   \tabular{ll}{
#'     \code{td} \tab the truncated density function. \cr
#'     \code{tp} \tab the truncated distribution function. \cr
#'     \code{tq} \tab the truncated quantile function.
#'   }
#' @seealso \code{\link[=eff.cont-class]{eff.cont}}.
#' @examples
#' cap(c(0, .5, 1))
#'
#' effContMean(function(p) qnorm(p, mean = 4))
#' effContMean(function(p) qbeta(p, 1, 2))
#'
#' effContVar(function(p) qnorm(p, mean = 2, sd = 4), 2)
#' effContVar(function(p) qbeta(p, 1, 2), 1/3)
#'
#' tr <- effContTrunc(dnorm, pnorm, qnorm, mean = .8, sd = .3)
#' x01 <- seq(0, 1, .01)
#' plot(x01, tr$d(x01), type = "l")
#' plot(x01, tr$p(x01), type = "l")
#' plot(x01, tr$q(x01), type = "l")
#' @name effCont-helper
NULL

#' @rdname effCont-helper
#' @export
cap <- function(x, xmin = 1e-6, xmax = 1-xmin) {
  pmin(xmax, pmax(xmin, x))
}

#' @rdname effCont-helper
#' @export
effContMean <- function(qfun, abs.tol = 1e-6, subdivisions = 500) {
  stats::integrate(qfun, lower = 0, upper = 1, abs.tol = abs.tol, subdivisions = subdivisions)$value
}

#' @rdname effCont-helper
#' @export
effContVar <- function(qfun, mu, abs.tol = 1e-6, subdivisions = 500) {
  stats::integrate(function(x) qfun(x)^2, lower = 0, upper = 1,
            abs.tol = abs.tol, subdivisions = subdivisions)$value - mu^2
}

#' @rdname effCont-helper
#' @export
effContTrunc <- function(dfun, pfun, qfun, ...) {
  F01 <- pfun(c(0,1), ...) # distribution at endpoints 0 and 1

  td <- function(x) { # truncated density
    y <- dfun(x, ...) / (F01[2] - F01[1])
    y[x<0 | x>1] <- 0
    y
  }
  tp <- function(q) { # truncated distribution
    y <- (pfun(q, ...) - F01[1]) / (F01[2] - F01[1])
    cap(y, 0, 1)
  }
  tq <- function(p) { # truncated quantile
    y <- qfun(p*(F01[2] - F01[1]) + F01[1], ...)
    cap(y, 0, 1)
  }

  list(d = td, p = tp, q = tq)
}
