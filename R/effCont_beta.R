#' Continuous Effectiveness as Beta Distribution.
#'
#' Fits a Beta distribution to the given sample of scores.
#'
#' @param x a sample of effectiveness scores between 0 and 1.
#' @return an object of class \code{eff.cont.beta}, which inherits from
#'   \code{\link[=eff.cont-class]{eff.cont}}.
#' @seealso \code{\link{deff}}, \code{\link{peff}}, \code{\link{qeff}} and \code{\link{reff}}.
#' @examples
#' e <- effCont_beta(web2010ap[,1])
#' c(e$mean, e$var)
#' plot(e, plot.data = TRUE)
#' @export
effCont_beta <- function(x) {
  x_cap <- cap(x)

  # estimate parameters numerically, from initial values
  mu_0 <- mean(x_cap)
  sigma2_0 <- stats::var(x_cap)
  shape1 <- mu_0 * (mu_0 * (1-mu_0) / sigma2_0 -1)
  shape2 <- (mu_0) * (mu_0 * (1-mu_0) / sigma2_0 -1)

  fit <- MASS::fitdistr(x_cap, densfun = "beta",
                        start = list(shape1 = shape1, shape2 = shape2),
                        lower = list(shape1 = 1, shape2 = 1))
  shape1 <- unname(fit$estimate[1])
  shape2 <- unname(fit$estimate[2])

  E <- shape1 / (shape1 + shape2) # expected value
  Var <- shape1 * shape2 / (shape1 + shape2)^2 / (shape1 + shape2 + 1) # variance

  # prepare eff object and return
  e <- effCont_new(E, Var, 2, x)
  e$model <- list(type = "beta", shape1 = shape1, shape2 = shape2)
  class(e) <- c("eff.cont.beta", class(e))
  e
}

#' @export
deff.eff.cont.beta <- function(x, .eff) {
  x <- cap(x)
  stats::dbeta(x, .eff$model$shape1, .eff$model$shape2)
}
#' @export
peff.eff.cont.beta <- function(q, .eff) {
  stats::pbeta(q, .eff$model$shape1, .eff$model$shape2)
}
#' @export
qeff.eff.cont.beta <- function(p, .eff) {
  stats::qbeta(p, .eff$model$shape1, .eff$model$shape2)
}
#' @export
reff.eff.cont.beta <- function(n, .eff) {
  stats::rbeta(n, .eff$model$shape1, .eff$model$shape2)
}

