
#' myncurve
#'
#' @param mu The mean of the normal probability distribution curve. Essentially defines the center of the curve of the distribution of the probability.
#' @param sigma The standard deviation of the normal probability distribution curve.
#'
#' @return a curve of the normal distribution (defined mean and standard deviation)
#' @export
#'
#' @examples
#' myncurve(mu = 2, sigma = 1)
myncurve = function(mu, sigma){
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma))

}
