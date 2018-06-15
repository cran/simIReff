eff_new <- function(mean, var, df, x = NULL) {
  e <- structure(list(mean = mean, var = var, df = df), class = "eff")
  if(!is.null(x))
    e$data <- x
  e
}

#' Effectiveness Distributions
#'
#' Density, distribution function, quantile function and random generation for an effectiveness
#' distribution.
#'
#' @param x,q vector of quantiles.
#' @param p vector of probabilities.
#' @param n number of observations.
#' @param .eff the \code{eff} object representing the effectiveness distribution.
#' @return \code{deff} gives the density, \code{peff} gives the distribution function, \code{qeff}
#'   gives the quantile function, and \code{reff} generates random variates.
#' @seealso \code{\link{effCont}} for continuous distributions, and \code{\link{effDisc}} for
#'   discrete distributions.
#' @examples
#' # sample distribution from AP scores
#' e <- effCont_beta(web2010ap[,1])
#' # pdf integrates to 1
#' integrate(deff, lower = 0, upper = 1, .eff = e)
#' # qeff (quantile) is the inverse of peff (cumulative)
#' qeff(peff(.2, e), e)
#' # random generation of 100 scores
#' r <- reff(100, e)
#' @name eff
NULL

#' @rdname eff
#' @export
deff <- function(x, .eff){
  UseMethod("deff", .eff)
}
#' @rdname eff
#' @export
peff <- function(q, .eff) {
  UseMethod("peff", .eff)
}
#' @rdname eff
#' @export
qeff <- function(p, .eff) {
  UseMethod("qeff", .eff)
}
#' @rdname eff
#' @export
reff <- function(n, .eff) {
  UseMethod("reff", .eff)
}
