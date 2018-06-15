#' Fit Effectiveness Distributions
#'
#' Attempts to fit the distribution families listed in \code{\link{effCont}} or
#' \code{\link{effDisc}}. In the discrete case, the \code{\link[=effDisc_dks]{dks}} distribution is
#' fitted with multipliers 1, 2, 5 and 10. Failure to fit any distribution family results in an
#' error.
#'
#' @param x a sample of effectiveness scores between 0 and 1.
#' @param support the support of the distribution (see \code{\link{support}}).
#' @param silent logical: should the report of error messages be suppressed?
#' @return a list of \code{\link[=eff.cont-class]{eff.cont}} objects fitted to the given data.
#' @seealso \code{\link{effCont}} and \code{\link{effDisc}} for the available distribution families.
#'
#'   See \code{\link{effSelect}} for model selection, and \code{\link{effFitAndSelect}} to fit and
#'   select automatically.
#' @examples
#' e <- effContFit(web2010ap[,1])
#' str(e, 1)
#' sapply(e, plot, plot.data = TRUE)
#'
#' e <- effDiscFit(web2010p20[,1], seq(0,1,.05))
#' str(e, 1)
#' sapply(e, plot, plot.data = TRUE)
#' @export
#' @name effFit
effContFit <- function(x, silent = TRUE) {
  effs <- as.list(rep(NA, 4))

  try(effs[[1]] <- effCont_norm(x), silent = silent)
  try(effs[[2]] <- effCont_beta(x), silent = silent)
  try(effs[[3]] <- effCont_nks(x), silent = silent)
  try(effs[[4]] <- effCont_bks(x), silent = silent)

  effs <- effs[!is.na(effs)]
  if(length(effs) == 0)
    stop("Unable to fit a distribution")
  effs
}

#' @export
#' @rdname effFit
effDiscFit <- function(x, support, silent = TRUE) {
  effs <- as.list(rep(NA, 5))

  try(effs[[1]] <- effDisc_bbinom(x, support), silent = silent)
  try(effs[[2]] <- effDisc_dks(x, support), silent = silent)
  try(effs[[3]] <- effDisc_dks(x, support, mult = 2), silent = silent)
  try(effs[[4]] <- effDisc_dks(x, support, mult = 5), silent = silent)
  try(effs[[5]] <- effDisc_dks(x, support, mult = 10), silent = silent)

  effs <- effs[!is.na(effs)]
  if(length(effs) == 0)
    stop("Unable to fit a distribution")
  effs
}
