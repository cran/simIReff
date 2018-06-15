#' Automatic Fitting and Selection of Effectiveness Distributions
#'
#' @param x a sample of effectiveness scores between 0 and 1, or a matrix or data frame of
#'   topic-by-system scores.
#' @param support the support of the distribution (see \code{\link{support}}).
#' @param method selection method. See \code{\link{effSelect}}.
#' @param silent logical: should the report of error messages be suppressed?
#' @return if \code{x} is a vector, the selected disttribution. If \code{x} is a matrix or data
#'   frame, a list of the selected distributions.
#' @seealso \code{\link{effFit}} and \code{\link{effSelect}}.
#' @examples
#' e <- effContFitAndSelect(web2010ap[,1], method = "logLik")
#' c(e$mean, e$var)
#' e2 <- effContFitAndSelect(web2010ap[,2], method = "logLik")
#' c(e2$mean, e2$var)
#'
#' ee <- effContFitAndSelect(web2010ap[,1:2], method = "logLik")
#' sapply(ee, function(e) c(e$mean, e$var)) # same as above
#' @export
#' @name effFitAndSelect
effContFitAndSelect <- function(x, method = "AIC", silent = TRUE){
  if(is.matrix(x) || is.data.frame(x)) {
    apply(x, 2, effContFitAndSelect, method = method, silent = silent)
  } else {
    effs <- effContFit(x, silent)
    effSelect(effs, method)
  }
}

#' @export
#' @rdname effFitAndSelect
effDiscFitAndSelect <- function(x, support, method = "AIC", silent = TRUE) {
  if(is.matrix(x) || is.data.frame(x)) {
    apply(x, 2, effDiscFitAndSelect, support = support, method = method, silent = silent)
  } else {
    effs <- effDiscFit(x, support, silent)
    effSelect(effs, method)
  }
}
