% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/myboot2.R
\name{myboot2}
\alias{myboot2}
\title{Title}
\usage{
myboot2(iter = 10000, x, fun = "mean", alpha = 0.05, cx = 1.5, ...)
}
\arguments{
\item{iter}{The Number of times that the re-sampled sample is, well, re-sampled again and again and again}

\item{x}{An array holding the values in the sample}

\item{fun}{The function which is being used to create a statistic. For example, sd(), var(), mean(), etc...}

\item{alpha}{The percent to subtract off of 100% when defining the confidence level of the confidence intervals being created}

\item{cx}{A parameter that that adjusts the text such that it modifies cex}

\item{...}{This just means that we can add parameters to whereever this shows up (the histogram), i.e. we can add things like histogram labels}
}
\value{
A lot! it always returns a list of parameters and a histogram with a confidence interval and point estimate plotted onto it.
}
\description{
Returns a bootstrap of a sample i.e. re-samples a sample iter times; returns histogram with Confidence intervals of mean, variance, or whatever you enter for fun = "", and then return the parameter estimate for the bootstrapped interval.
}
\examples{
myboot(x = rchisq(20,3), fun = "mean")
}
