# effCont_mixbeta <- function(x, k = 2) {
#   x <- cap(x)
#
#   mix <- betareg::betamix(x~1|1, k = k,  +)
#
#   coefs <- matrix(coef(mix), ncol = 2)
#   beta_mu <- plogis(coefs[,1])
#   beta_phi <- exp(coefs[,2])
#
#   beta_w <- prior(mix$flexmix)
#   beta_shape1 <- beta_mu * beta_phi
#   beta_shape2 <- (1-beta_mu) * beta_phi
#
#   f <- function(xx, fbeta) { # wrapper function to sum over components
#     out <- rep(0, length(xx))
#     for(i in seq_along(beta_shape1))
#       out <- out + beta_w[i] * fbeta(xx, beta_shape1[i], beta_shape2[i])
#     return(out)
#   }
#
#   ddist <- function(x) f(capX(x), dbeta)
#   pdist <- function(x) f(x, pbeta)
#   # We need to approximate the quantile function numerically through the cdf
#   x01 <- seq(0,1, length.out = subdivisions)
#   x01 <- pdist(x01) # interpolate at F(0...1) rather than at 0...1, so it is smooth
#   qdist <- approxfun(x01, sapply(x01, function(p) uniroot(function(x) pdist(x)-p, 0:1)$root),
#                      yleft = NaN, yright = NaN)
#
#   dist <- list(d = ddist, p = pdist, q = qdist)
#   dist$model <- list(type = "mixbeta", prettytype = paste0("mixbeta_", length(beta_w)),
#                      mix = mix, k = length(beta_w),
#                      df = 3*length(beta_w) -1)
#   dist$support <- NULL
#   dist$mu <- sum(beta_w * beta_shape1 / (beta_shape1 + beta_shape2))
#
#   class(dist) <- "effdist"
#   return(dist)
# }
#
# deff.effCont_mixbeta <- function(x, eff) {
#   x <- cap(x)
#   dbeta(x, eff$model$shape1, eff$model$shape2) # TODO cap?
# }
# peff.effCont_mixbeta <- function(q, eff) {
#   pbeta(q, eff$model$shape1, eff$model$shape2) # TODO cap?
# }
# qeff.effCont_mixbeta <- function(p, eff) {
#   qbeta(p, eff$model$shape1, eff$model$shape2)
# }
# reff.effCont_mixbeta <- function(n, eff) {
#   rbeta(n, eff$model$shape1, eff$model$shape2)
# }
#
