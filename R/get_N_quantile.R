# ' Get the quantile that the true value occurs at in the estimated distribution of that value
#'
#' Assumes a log-Normal distribution for the quantity in question.
#' @param N the true value
#' @param Nhat the estimated value
#' @param cvN the estimated coefficient of variation
#' @return the quantile value
#' @author David L Miller
#' @export
get_N_quantile <- function(N, Nhat, cvN){

  meantrans <- function(m, cv) log(m)-0.5*log(cv^2+1)
  setrans <- function(cv) sqrt(log(cv^2+1))

  plnorm(N, meantrans(Nhat, cvN), setrans(cvN))
}

