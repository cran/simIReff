% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.cont.R
\name{plot.eff.cont}
\alias{plot.eff.cont}
\alias{dplot.eff.cont}
\alias{pplot.eff.cont}
\alias{qplot.eff.cont}
\title{Plotting tools for Continuous effectiveness distributions}
\usage{
\method{dplot}{eff.cont}(x, ..., plot.data = TRUE, subdivisions = 200,
  xlab = "x", ylab = "f(x)", main = "density")

\method{pplot}{eff.cont}(x, ..., plot.data = TRUE, subdivisions = 200,
  xlab = "q", ylab = "F(q)", main = "distribution")

\method{qplot}{eff.cont}(x, ..., plot.data = TRUE, subdivisions = 200,
  xlab = "p", ylab = expression(F^-1 * (p)), main = "quantile")
}
\arguments{
\item{x}{the effectiveness distribution to plot.}

\item{...}{arguments to be passed to \code{\link[graphics]{lines}}.}

\item{plot.data}{logical: whether to plot the data used to fit the distribution, if any.}

\item{subdivisions}{number of equidistant points at which to evaluate the distribution to plot.}

\item{xlab}{the title for the x axis.}

\item{ylab}{the title for the y axis.}

\item{main}{the overall title for the plot.}
}
\description{
Plot the density, distribution and quantile functions of a continuous effectiveness distribution.
}
\seealso{
\code{\link{plot.eff.disc}} for discrete distributions.
}
