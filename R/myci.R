#' myci
#'
#' @description Creates confidence intervals for the mean of a population given the sample and confidence level passed in
#'
#' @param x an array containing a random sample of a population
#' @param alpha the probability of a Type 1 error used to create the confidence level
#'
#' @return A list containing (lower bound, upper bound) of the CI for the population mean with confidence level (1-alpha)100%
#' @export
#'
#' @examples myci(x = rnorm(n=20, mean = 5, sd = 2), alpha = .05)
myci = function(x, alpha = .05){
  n = length(x)
  xbar = mean(x)
  s = sd(x)
  t = qt(1- alpha/2, n-1)
  mp = c(-1,1) #Plus and minus creates the lower and upper CIs
  mu = xbar + mp * t * s/ sqrt(n)
  return (list(ci = mu))
}
