#' Continuous Effectiveness as Truncated Gaussian Kernel-smoothed Distribution.
#'
#' Fits a kernel-smoothed distribution to the given sample of scores, truncated between 0 and 1, and
#' using a gaussian kernel.
#'
#' @param x a sample of effectiveness scores between 0 and 1.
#' @return an object of class \code{eff.cont.nks}, which inherits from
#'   \code{\link[=eff.cont-class]{eff.cont}}.
#' @seealso \code{\link{deff}}, \code{\link{peff}}, \code{\link{qeff}} and \code{\link{reff}}.
#' @examples
#' e <- effCont_nks(web2010ap[,1])
#' c(e$mean, e$var)
#' plot(e, plot.data = TRUE)
#' @export
effCont_nks <- function(x) {
  # estimate
  k <- ks::kde(x)
  tk <- effContTrunc(ks::dkde, ks::pkde, ks::qkde, fhat = k)
  df <- mean(stats::dnorm(0, sd = k$h) / ks::dkde(x, fhat = k))

  E <- effContMean(tk$q) # expected value
  Var <- effContVar(tk$q, E) # variance

  # prepare eff object and return
  e <- effCont_new(E, Var, df, x)
  e$model <- list(type = "nks", kde = k, d = tk$d, p = tk$p, q = tk$q)
  class(e) <- c("eff.cont.nks", class(e))
  e
}

#' @export
deff.eff.cont.nks <- function(x, .eff) {
  .eff$model$d(x)
}
#' @export
peff.eff.cont.nks <- function(q, .eff) {
  .eff$model$p(q)
}
#' @export
qeff.eff.cont.nks <- function(p, .eff) {
  .eff$model$q(p)
}
#' @export
reff.eff.cont.nks <- function(n, .eff) {
  r <- stats::runif(n)
  .eff$model$q(r)
}
