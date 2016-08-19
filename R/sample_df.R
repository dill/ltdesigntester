#' Sample data using a detection function
#'
#' @param df \code{DSsim} \code{Detectability} object defining the detection function
#' @param data \code{vector} of distances
sample_df <- function(df, distances){

  if(df@key.function=="hn"){
    g <- function(x, sigma, b){
      exp(-x^2/(2*sigma^2))
    }
  }else if(df@key.function=="hr"){
    g <- function(x, sigma, b){
      1-exp(-(x/sigma)^(-b))
    }
  }else{
    stop("Detection function not \"hr\" or \"hn\"")
  }

  as.logical(rbinom(length(distances), 1,
                    g(distances, sigma=df@scale.param, b=df@shape.param)))
}

