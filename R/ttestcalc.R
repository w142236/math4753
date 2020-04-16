#' t.test.calc()
#'
#' @description RAR statistic that deterines whether the t-value of the sample passed in and returns the t-value and the t-value intervals
#'
#' @param x a vector containing a sample
#' @param alpha the probability of the type I error
#' @param mu the population mean
#'
#' @return returns R A R being the interval with which the t-value is expected to lie between.
#' @export
#'
#' @examples x = rnorm(n = 30, mean = 5, sd =10);ttestcalc(x, alpha = .05, mu = 5)
ttestcalc = function(x, alpha, mu){
   n = length(x)
   t = (mean(x) - mu)/(sd(x)/sqrt(n))
   t_negAlphaOver2 = qt(alpha/2, n-1)
   t_AlphaOver2 = qt(1-alpha/2, n-1)
   return(list(t = t, RLeft = t_negAlphaOver2, RRight = t_AlphaOver2))
}
