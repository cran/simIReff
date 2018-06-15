#' Helper functions for discrete effectiveness distributions
#'
#' These are functions to help in the creation and use of discrete effectiveness distributions.
#'
#' \code{matchTol} returns a vector of the positions of matches of \code{x} in the vector of
#' possible support values, within tolerance (see \code{\link[base]{match}}). This is helpful when
#' data are loaded from disk and possibly rounded or truncated.
#'
#' \code{support} obtains the discrete support defined by an effectiveness measure given its name.
#' Current measures are Reciprocal Rank (\code{"RR"}), and Precision at k (\code{"P@k"} or
#' \code{"Pk"}, where \code{k} is the cutoff, eg. \code{"P@10"} or \code{"P10"}).
#'
#' @param x a vector of effectiveness scores.
#' @param support the support of the distribution.
#' @param tol tolerance for matching.
#' @param measure the case insensitive name of the effectiveness measure. See Details.
#' @param runLength the maximum number of documents retrieved for a query (defautls to 1000).
#' @return \code{matchTol}: an integer vector giving the position in the support of the match if
#'   there is a match, otherwise \code{NA}.
#'
#'   \code{support}: the support of the distribution of scores defined by the measure.
#' @seealso \code{\link[=eff.disc-class]{eff.disc}}.
#' @examples
#' support("rr")
#' support("rr", runLength = 10)
#' support("p@10")
#' support("p20")
#'
#' (i <- matchTol(c(.1, .4, .41, .40001), support("p10")))
#' support("p10")[i]
#' @name effDisc-helper
NULL

#' @rdname effDisc-helper
#' @export
matchTol <- Vectorize(function(x, support, tol = 1e-4) {
  err <- abs(support - x)
  i <- which.min(err)
  if(err[i] <= tol)
    return(i)
  else
    return(NA)
}, vectorize.args = "x", SIMPLIFY = TRUE)

#' @rdname effDisc-helper
#' @export
support <- function(measure, runLength = 1000) {
  stopifnot(is.character(measure) && length(measure)==1)

  measure <- tolower(measure)

  s <- if(measure == "rr") { # reciprocal rank
    c(0, 1/runLength:1)
  }else if(grepl("^p@?(\\d+)$", measure)) { # precision at k
    cutoff <- as.numeric(gsub("^p@?(\\d+)$", "\\1", measure))
    0:cutoff / cutoff
  }else stop("invalid measure")

  s
}