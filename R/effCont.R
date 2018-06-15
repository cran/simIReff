#' Continuous Effectiveness Distributions
#'
#' Families to model effectiveness distributions with continuous support. Currently implemented
#' families are:
#' \tabular{rl}{
#'   \code{\link{effCont_norm}} \tab Truncated Normal. \cr
#'   \code{\link{effCont_beta}} \tab Beta. \cr
#'   \code{\link{effCont_nks}} \tab Truncated Kernel-smoothed with Gaussian kernel. \cr
#'   \code{\link{effCont_bks}} \tab Kernel-smoothed with Beta kernel.
#' }
#' @seealso \code{\link{effContFit}} to fit continuous distributions, and
#'   \code{\link[=eff.cont-class]{eff.cont}} for the S3 class.
#'
#'   For discrete distributions, see \code{\link{effDisc}}.
#' @name effCont
NULL

#' Class \code{eff.cont}
#'
#' This is the base S3 class for all continuous effectiveness distributions, which is itself a
#' subclass of \code{eff}. Function \code{effCont_new} is the constructor of the class.
#'
#' A new distribution family is expected to build new objects through this constructor, and they
#' must implement methods \code{\link{deff}}, \code{\link{peff}}, \code{\link{qeff}} and
#' \code{\link{reff}}.
#'
#' @param mean the expected value of the distibution.
#' @param var the variance of the distribution.
#' @param df the effective degrees of freedom of the distribution.
#' @param x the sample of effectiveness scores used to fit the distribution. Defaults to
#'   \code{NULL}.
#' @return an object of class \code{eff.cont}, with the following components:
#' \tabular{rl}{
#'   \code{mean} \tab the expected value. \cr
#'   \code{var} \tab the variance. \cr
#'   \code{df} \tab the degrees of freedom (effective number of parameters) for
#'     \link[=effSelect]{model selection}. \cr
#'   \code{data} \tab the sample data used to fit the distribution, or \code{NULL} if none. \cr
#'   \code{model} \tab a list with the family-specific data. \cr
#' }
#' @seealso \code{\link{effCont}} for a list of currently implemented distribution families,
#'   \code{\link{effContFit}} to fit distributions, and \code{\link{effCont-helper}} for helper
#'   functions.
#'
#'   For discrete distributions, see \code{\link[=eff.disc-class]{eff.disc}}.
#' @name eff.cont-class
effCont_new <- function(mean, var, df, x = NULL) {
  e <- eff_new(mean, var, df, x)
  class(e) <- c("eff.cont", class(e))
  e
}
