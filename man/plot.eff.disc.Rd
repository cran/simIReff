% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.disc.R
\name{plot.eff.disc}
\alias{plot.eff.disc}
\alias{dplot.eff.disc}
\alias{pplot.eff.disc}
\alias{qplot.eff.disc}
\title{Plotting tools for Discrete effectiveness distributions}
\usage{
\method{dplot}{eff.disc}(x, ..., plot.data = TRUE, xlab = "x",
  ylab = "f(x)", main = "mass")

\method{pplot}{eff.disc}(x, ..., plot.data = TRUE, xlab = "q",
  ylab = "F(q)", main = "distribution")

\method{qplot}{eff.disc}(x, ..., plot.data = TRUE, xlab = "p",
  ylab = expression(F^-1 * (p)), main = "quantile")
}
\arguments{
\item{x}{the effectiveness distribution to plot.}

\item{...}{arguments to be passed to \code{\link[graphics]{lines}}.}

\item{plot.data}{logical: whether to plot the data used to fit the distribution, if any.}

\item{xlab}{the title for the x axis.}

\item{ylab}{the title for the y axis.}

\item{main}{the overall title for the plot.}
}
\description{
Plot the density, distribution and quantile functions of a discrete effectiveness distribution.
}
\seealso{
\code{\link{plot.eff.cont}} for continuous distributions.
}
