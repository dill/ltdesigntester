#' Make DSsim data usable by dsm
#'
#' Get the simulated data and put it into a format that \code{dsm} can use. The consists of creating the three tables used by \code{\link{dsm}}.
#'
#' @param survey a simulated survey
#' @return a \code{list} with three \code{data.frame}s: \code{obs} the observation table, \code{segs} the segment table and \code{dist} the distance data.
#' @export
#' @author David L Miller
dsmify <- function(survey){

  # distance data
  dist.data <- DSsim::get.distance.data(survey)
  dist.data$Sample.Label <- dist.data$transect.ID

  # assemble the sent data
  segs <- survey@transects@sampler.info
  segs$x <- apply(segs, 1, function(x) mean(as.numeric(x[c(2,4)])))
  segs$y <- apply(segs, 1, function(x) mean(as.numeric(x[c(3,5)])))
  segs$Sample.Label <- segs$ID
  segs$Effort <- segs$length
  # assemble observation table
  obs <- survey@obs.table@obs.table
  obs <- merge(obs, dist.data, by="object")
  obs$Sample.Label <- obs$Sample.Label.x
  obs$Sample.Label.y <- obs$Sample.Label.x <- NULL
  obs$size <- 1

  return(list(obs=obs, segs=segs, dist=dist.data))
}
