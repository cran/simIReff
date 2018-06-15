#' Model Selection for Effectiveness Distributions
#'
#' Functions to compute the log-likelihood, the Akaike Information Criterion, and the Bayesian
#' Information Criterion for an effectiveness distribution. \code{effSelect} and
#' \code{which.effSelect} are helper function for automatic selection from a given list of
#' candidates.
#'
#' @param object an effectiveness distribution.
#' @param effs the list of candidate distributions to select from.
#' @param method selection method. One of \code{"AIC"} (default), \code{"BIC"}, or \code{"logLik"}.
#' @param ... other parameters to the selection function.
#' @return the selected disttribution (\code{effSelect}), or its index within \code{effs}
#'   (\code{which.effSelect}).
#' @seealso \code{\link{logLik}}, \code{\link{AIC}}, \code{\link{BIC}} for details on model
#'   selection.
#'
#'   See \code{\link{effFitAndSelect}} to fit and select automatically.
#' @examples
#' ee <- effContFit(web2010ap[,5])
#' e <- effSelect(ee, method = "BIC")
#' e2 <- ee[[which.effSelect(ee, method = "BIC")]] # same as e
#'
#' logLik(e)
#' AIC(e, k=4)
#' BIC(e)
#' @export
effSelect <- function(effs, method = "AIC", ...) {
  i <- which.effSelect(effs, method, ...)
  effs[[i]]
}

#' @export
#' @rdname effSelect
which.effSelect <- function(effs, method = "AIC", ...) {
  method <- match.arg(method, c("AIC", "BIC", "logLik"))
  FUN <- get(method)

  scores <- sapply(effs, FUN, ...)
  i <- ifelse(method == "logLik", which.max(scores), which.min(scores))
  structure(i, score = scores[i])
}

#' @rdname effSelect
#' @export
logLik.eff <- function(object, ...) {
  stopifnot(!is.null(object$data))

  ll <- sum(log(deff(object$data, object)))
  structure(ll, class = "logLik", df = object$df, nobs = length(object$data))
}