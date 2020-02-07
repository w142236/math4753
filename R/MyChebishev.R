#' Chebishev function
#'
#' This takes in a number of standard deviations, k, and returns the minimum number of points within that many standard deviations for any distribution
#'
#' @param k
#'
#' @return a number of points, n, representing that, for any distribution, there are this many or a greater number of points within k standard deviations
#' @export
#'
#' @examples
#' k = 2; chebishev(k)
chebishev = function(k){
  n = 1 - 1/k^2
  n
}
