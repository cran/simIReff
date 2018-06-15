#' Discrete Effectiveness Distributions
#'
#' Families to model effectiveness distributions with discrete support. Currently implemented
#' families are:
#' \tabular{rl}{
#'   \code{\link{effDisc_bbinom}} \tab Beta-Binomial \cr
#'   \code{\link{effDisc_dks}} \tab Kernel-smoothed with Discrete kernel.
#' }
#' @seealso \code{\link{effDiscFit}} to fit discrete distributions, and
#'   \code{\link[=eff.disc-class]{eff.disc}} for the S3 class. For continuous distributions, see
#'   \code{\link{effCont}}.
#' @name effDisc
NULL

#' Class \code{eff.disc}
#'
#' This is the base S3 class for all discrete effectiveness distributions, which is itself a
#' subclass of \code{eff}. Function \code{effDisc_new} is the constructor of the class.
#'
#' A new distribution family is expected to build new objects through this constructor. Default
#' implementations are readily available for methods \code{\link{deff}}, \code{\link{peff}},
#' \code{\link{qeff}} and \code{\link{reff}}.
#'
#' @param p the values of the distribution function at the support points.
#' @param support the support of the distribution.
#' @param df the effective degrees of freedom of the distribution.
#' @param x the sample of effectiveness scores used to fit the distribution. Defaults to
#'   \code{NULL}.
#' @return an object of class \code{eff.disc}, with the following components:
#' \tabular{rl}{
#'   \code{mean} \tab the expected value. \cr
#'   \code{var} \tab the variance. \cr
#'   \code{df} \tab the degrees of freedom (effective number of parameters) for
#'     \link[=effSelect]{model selection}. \cr
#'   \code{support} \tab the support of the distribution. \cr
#'   \code{data} \tab the sample data used to fit the distribution, or \code{NULL} if none. \cr
#'   \code{model} \tab a list with the family-specific data. \cr
#' }
#' @seealso \code{\link{effDisc}} for a list of currently implemented distribution families,
#'   \code{\link{effDiscFit}} to fit distributions, and \code{\link{effDisc-helper}} for helper
#'   functions.
#'
#'   For continuous distributions, see \code{\link[=eff.cont-class]{eff.cont}}.
#' @name eff.disc-class
effDisc_new <- function(p, support, df, x = NULL) {
  d <- c(p[1], diff(p))
  dfun <- function(x) {
    i <- matchTol(x, support)
    return(d[i])
  }
  pfun <- stats::stepfun(support, c(0, p), ties = "ordered")
  qfun <- stats::stepfun(p[-length(p)], support, right = TRUE, ties = "ordered")
  # qfun <- Vectorize(function(x) {
  #   i <- which(p >= x)[1]
  #   return(support[i])
  # })

  E <- sum(support * dfun(support))
  Var <- sum(support^2 * dfun(support)) - E^2

  e <- eff_new(E, Var, df, x)
  e$support <- support
  e$dfun <- dfun
  e$pfun <- pfun
  e$qfun <- qfun
  class(e) <- c("eff.disc", class(e))
  e
}

#' @export
deff.eff.disc <- function(x, .eff) {
  .eff$dfun(x)
}
#' @export
peff.eff.disc <- function(q, .eff) {
  .eff$pfun(q)
}
#' @export
qeff.eff.disc <- function(p, .eff) {
  .eff$qfun(p)
}
#' @export
reff.eff.disc <- function(n, .eff) {
  u <- stats::runif(n)
  .eff$qfun(u)
}

