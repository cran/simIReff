#' Fit Vine copula models to matrices of effectiveness scores
#'
#' Fitting of and simulation from a copula model.
#'
#' @param n number of observations to simulate.
#' @param .effcop the \code{effcop} object representing the copula model (see \code{effcopFit}).
#' @param x a matrix or data frame of effectiveness scores to estimate dependence.
#' @param eff a list of effectiveness distributions to use for the margins.
#' @param ... other parameters for \code{\link[rvinecopulib]{vinecop}}, such as \code{family_set},
#'   \code{selcrit}, \code{trunc_lvl} and \code{cores}.
#' @return \code{effcopFit}: an object of class \code{effcop}, with the following components:
#'
#'   \tabular{rl}{
#'     \code{data} \tab the matrix of effectiveness scores used to fit the copula. \cr
#'     \code{pobs} \tab the matrix of pseudo-observations computed from \code{data}. This is stored
#'       because pseudo-observations are calculated breaking ties randomly
#'       (see \code{\link[rvinecopulib]{pseudo_obs}}). \cr
#'     \code{margins} \tab the list of marginal effectiveness distributions. \cr
#'     \code{cop} \tab the underlying copulas fitted with \code{\link[rvinecopulib]{vinecop}}.
#'   }
#' These components may be altered to gain specific simulation capacity, such as
#'   \link[=effTransform]{systems with the same expected value}.
#'
#' \code{reffcop}: a matrix of random scores.
#' @seealso \code{\link{effCont}} and \code{\link{effDisc}} for available distributions for the
#'   margins. See package \code{\link[rvinecopulib]{rvinecopulib}} for details on fitting
#'   the copulas.
#' @examples
#' \donttest{
#' ## Automatically build a gaussian copula to many systems
#' d <- web2010p20[,1:20] # sample P@20 data from 20 systems
#' effs <- effDiscFitAndSelect(d, support("p20")) # fit and select margins
#' cop <- effcopFit(d, effs, family_set = "gaussian") # fit copula
#' y <- reffcop(1000, cop) # simulate new 1000 topics
#'
#' # compare observed vs. expected mean
#' E <- sapply(effs, function(e) e$mean)
#' E.hat <- colMeans(y)
#' plot(E, E.hat)
#' abline(0:1)
#'
#' # compare observed vs. expected variance
#' Var <- sapply(effs, function(e) e$var)
#' Var.hat <- apply(y, 2, var)
#' plot(Var, Var.hat)
#' abline(0:1)
#' }
#' @name effcop
#' @export
effcopFit <- function(x, eff, ...) {
  pobs <- rvinecopulib::pseudo_obs(x, ties_method = "random")

  vine <- rvinecopulib::vinecop(pobs, ...)
  cop <- list(data = x, pobs = pobs, margins = eff, copula = vine)
  class(cop) <- "effcop"
  cop
}

#' @rdname effcop
#' @export
reffcop <- function(n, .effcop) {
  r <- rvinecopulib::rvinecop(n, .effcop$copula)
  y <- sapply(1:ncol(r), function(i) qeff(r[,i], .effcop$margins[[i]]))
}
