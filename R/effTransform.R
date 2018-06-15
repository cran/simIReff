#' Transform effectiveness distributions towards a expected value
#'
#' Transforms the given effectiveness distribution such that its expected value matches a predefined
#' value. For details, please refer to section 3.4 of (Urbano and Nagler, 2018).
#'
#' \code{effTransformAll} does the same but for a list of distributions and target means.
#'
#' @param eff the distribution to transform.
#' @param effs the list of distributions to transform.
#' @param mean the target expected value to transform to. If missing, defaults to the mean in the
#'   data used to fit \code{eff}, if any.
#' @param means the vector of target expected values to transform to. If missing, defaults to the
#'   means in the data used to fit \code{effs}, if any.
#' @param abs.tol the absolute tolerance of the transformation.
#' @param silent logical: should the report of error messages be suppressed?
#' @return an effectiveness distribution of class \code{eff.cont.trans} or \code{eff.disc.trans},
#'   depending on the type of distribution.
#' @references J. Urbano and T. Nagler. (2018). Stochastic Simulation of Test Collections:
#'   Evaluation Scores. ACM SIGIR.
#' @examples
#' e <- effCont_beta(web2010ap[,1])
#' e2 <- effTransform(e, 0.12)
#' c(e$mean, e2$mean)
#' plot(e)
#' plot(e2)
#'
#' \donttest{
#' # transform a list of distributions to the observed means
#' ee <- effContFitAndSelect(web2010ap[,1:5])
#' ee2 <- effTransformAll(ee)
#' obsmeans <- colMeans(web2010ap[,1:5])
#' sapply(ee, function(e)e$mean) - obsmeans
#' sapply(ee2, function(e)e$mean) - obsmeans
#' }
#' @export
effTransform <- function(eff, mean, abs.tol = 1e-5) {
  if(missing(mean))
    mean <- base::mean(eff$data)


  opt <- stats::optim(par = c(1, 1),
                      fn = function(par) {
                        teff <- effTransform_inner(eff, par[1], par[2])
                        return((teff$mean - mean)^2)
                      },
                      method = "L-BFGS-B",
                      lower = c(1, 1),
                      control = list(factr = abs.tol^2 / .Machine$double.eps))

  if(opt$convergence != 0)
    stop("No successful transformation")
  effTransform_inner(eff, opt$par[1], opt$par[2])
}

#' @rdname effTransform
#' @export
effTransformAll <- function(effs, means, abs.tol = 1e-5, silent = TRUE) {
  if(missing(means))
    means <- sapply(effs, function(e) mean(e$data))

  means <- means + rep(0, length(effs))
  teffs <- as.list(rep(NA, length(effs)))

  for(i in seq_along(effs)) {
    e <- try(effTransform(effs[[i]], mean = means[i], abs.tol), silent = silent)

    if(!inherits(e, "try-error"))
      teffs[[i]] <- e
  }

  teffs
}

effTransform_inner <- function(eff, par1, par2) {
  if(inherits(eff, "eff.cont")) { # continuous

    dfun <- function(x) {
      #x <- cap(x)
      stats::dbeta(peff(x, eff), par1, par2) * deff(x, eff)
    }
    pfun <- function(q) {
      stats::pbeta(peff(q, eff), par1, par2)
    }
    qfun <- function(p) {
      qeff(stats::qbeta(p, par1, par2), eff)
    }

    E <- effContMean(qfun)
    Var <- effContVar(qfun, E)

    teff <- effCont_new(E, Var, eff$df, eff$data)
    teff$model <- list(type = paste0("t(", eff$model$type, ")"),
                       original = eff, par1 = par1, par2 = par2,
                       dfun = dfun, pfun = pfun, qfun = qfun)

    class(teff) <- c("eff.cont.trans", class(teff))
    teff
  } else { # discrete
    # compute the new cdf values, and from there the full object
    p <- stats::pbeta(peff(eff$support, eff), par1, par2)
    p <- cummax(p) # ensure it's monotonically increasing

    teff <- effDisc_new(p, eff$support, eff$df, eff$data)
    teff$model <- list(type = paste0("t(", eff$model$type, ")"),
                       original = eff, par1 = par1, par2 = par2)

    class(teff) <- c("eff.disc.trans", class(teff))
    teff
  }
}

#' @export
deff.eff.cont.trans <- function(x, .eff) {
  .eff$model$dfun(x)
}
#' @export
peff.eff.cont.trans <- function(q, .eff) {
  .eff$model$pfun(q)
}
#' @export
qeff.eff.cont.trans <- function(p, .eff) {
  .eff$model$qfun(p)
}
#' @export
reff.eff.cont.trans <- function(n, .eff) {
  u <- stats::runif(n)
  .eff$model$qfun(u)
}
