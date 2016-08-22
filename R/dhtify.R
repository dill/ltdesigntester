#' Make DSsim data usable by dht
#'
#' Get the simulated data and put it into a format that \code{dht} can use. The consists of creating the three tables used by \code{\link{dht}}.
#'
#' @param survey a simulated survey
#' @return a \code{list} with three \code{data.frame}s: \code{obs} the observation table, \code{segs} the segment table and \code{dist} the distance data.
#' @export
#' @author David L Miller
#' @importFrom plyr ddply
dhtify <- function(dsm_data, survey, transect_id){

  # assemble the sample table
  segs <- survey@transects@sampler.info
  segs$Region.Label <- segs$region
  segs$region <- NULL
  sh_segs <- segs
  if(is.null(transect_id)){
    segs$Sample.Label <- segs$ID
    segs$Effort <- segs$length
  }else{
    segs$Sample.Label <- transect_id
    segs <- ddply(segs, .(Sample.Label), summarize,
                  Effort=sum(length), Region.Label=unique(Region.Label))
  }

  # assemble observation table
  obs <- dsm_data$obs
  sh_segs$tr <- transect_id
  sh_segs$Sample.Label <- sh_segs$ID
  sh_segs <- sh_segs[,c("Sample.Label", "tr", "Region.Label")]
  obs <- merge(obs, sh_segs, by="Sample.Label")
  obs$Sample.Label <- obs$tr
  obs$tr <- NULL

  obs$size <- 1

  return(list(region=survey@region.table@region.table, sample=segs, obs=obs))
}
