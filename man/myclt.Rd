% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/myclt.R
\name{myclt}
\alias{myclt}
\title{Central Limit Theorem Function}
\usage{
myclt(n, iter, a = 0, b = 5)
}
\arguments{
\item{n}{Represent the number of y's used in each sample}

\item{iter}{Represents the number of columns i.e. the number of samples}

\item{a}{parameter of uniform distribution}

\item{b}{second parameter of uniform distribution}
}
\value{
summation of the y's over iter samples with n y's per sample. The higher the value of n, the closer the resulting distribution will be to a normal distribution
}
\description{
Takes in n, iter, a, and b and returns a summation of the y's in iter as a matrix and also returns a histogram of the summations. Under the CLT, the distribution represented by the histogram should be normal as the number of samples of the population, being uniform with paramters n*iter, a, and b; become increasingly large.
}
\examples{
w=myclt(n=50,iter=10000,a=5,b=10)
}
