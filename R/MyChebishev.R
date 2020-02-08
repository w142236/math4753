#' Chebishev function
#'
#' This takes in a number of standard deviations, k, and returns the minimum number of points within that many standard deviations for any distribution
#'
#' @param k
#'
#' @return a percentage of points, n, representing that, for any distribution, the percentage of points within k standard deviations of the mean is greater than or equal to this percentage
#' @export
#'
#' @examples
#' k = 2; chebishev(k)
chebishev = function(k){
  n = 1 - 1/k^2
  n
}
