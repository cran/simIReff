% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simIReff.R
\docType{package}
\name{simIReff-package}
\alias{simIReff}
\alias{simIReff-package}
\title{\code{simIReff}: Stochastic Simulation for Information Retrieval Evaluation: Effectiveness Scores}
\description{
Provides tools for the stochastic simulation of effectiveness scores to mitigate data-related
limitations of Information Retrieval evaluation research. These tools include:
\itemize{
  \item Fitting of continuous and discrete distributions to model system effectiveness.
  \item Plotting of effectiveness distributions.
  \item Selection of distributions best fitting to given data.
  \item Transformation of distributions towards a prespecified expected value.
  \item Proxy to fitting of copula models based on these distributions.
  \item Simulation of new evaluation data from these distributions and copula models.
}
}
\examples{
\donttest{
## Fit a marginal AP distribution and simulate new data
x <- web2010ap[,10] # sample AP scores of a system
e <- effContFitAndSelect(x, method = "BIC") # fit and select based on log-likelihood
plot(e) # plot pdf, cdf and quantile function
e$mean # expected value
y <- reff(50, e) # simulation of 50 new topics

## Transform the distribution to have a pre-specified expected value
e2 <- effTransform(e, mean = .14) # transform for expected value of .14
plot(e2)
e2$mean # check the result

## Build a copula model of two systems
d <- web2010ap[,2:3] # sample AP scores
e1 <- effCont_norm(d[,1]) # force the first margin to follow a truncated gaussian
e2 <- effCont_bks(d[,2]) # force the second margin to follow a beta kernel-smoothed
cop <- effcopFit(d, list(e1, e2)) # copula
y <- reffcop(1000, cop) # simulation of 1000 new topics
c(e1$mean, e2$mean) # expected means
colMeans(y) # observed means

## Modify the model to both systems have the same distribution
cop2 <- cop # copy the model
cop2$margins[[2]] <- e1 # modify 2nd margin
y <- reffcop(1000, cop2) # simulation of 1000 new topics
colMeans(y) # observed means

## Automatically build a gaussian copula to many systems
d <- web2010p20[,1:20] # sample P@20 data from 20 systems
effs <- effDiscFitAndSelect(d, support("p20")) # fit and select margins
cop <- effcopFit(d, effs, family_set = "gaussian") # fit copula
y <- reffcop(1000, cop) # simulate new 1000 topics

# compare observed vs. expected mean
E <- sapply(effs, function(e) e$mean)
E.hat <- colMeans(y)
plot(E, E.hat)
abline(0:1)

# compare observed vs. expected variance
Var <- sapply(effs, function(e) e$var)
Var.hat <- apply(y, 2, var)
plot(Var, Var.hat)
abline(0:1)

# compare distributions
o <- order(colMeans(d))
boxplot(d[,o])
points(colMeans(d)[o], col = "red", pch = 4) # plot means
boxplot(y[,o])
points(colMeans(y)[o], col = "red", pch = 4) # plot means
}
}
\references{
J. Urbano and T. Nagler. (2018). Stochastic Simulation of Test Collections:
  Evaluation Scores. ACM SIGIR.
}
\seealso{
Useful links:
\itemize{
  \item \url{https://github.com/julian-urbano/simIReff/}
  \item Report bugs at \url{https://github.com/julian-urbano/simIReff/issues}
}

}
\author{
\strong{Maintainer}: Julián Urbano \email{urbano.julian@gmail.com}

Other contributors:
\itemize{
  \item Thomas Nagler \email{thomas.nagler@tum.de} [contributor]
}

}
