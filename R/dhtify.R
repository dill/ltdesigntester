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
  if(is.null(transect_id)){
    segs$Sample.Label <- segs$ID
    segs$Effort <- segs$length
  }else{
    segs[, c("start.X", "start.Y", "end.X", "end.Y", "ID", "d7.length")] <- NULL
    segs$Sample.Label <- transect_id
    segs_eff <- ddply(segs, .(Sample.Label), summarize, Effort=sum(length))
    segs$Effort <- segs_eff$Effort
    segs$length <- NULL
    segs <- unique(segs)
  }
  segs$Region.Label <- segs$region
  segs$region <- NULL

  # assemble observation table
  obs <- dsm_data$obs#survey@obs.table@obs.table
  sh_segs <- survey@transects@sampler.info
  sh_segs$tr <- transect_id
  sh_segs$Sample.Label <- sh_segs$ID
  sh_segs <- sh_segs[,c("Sample.Label", "tr", "region")]
  sh_segs$Region.Label <- sh_segs$region
  sh_segs$region <- NULL
  obs <- merge(obs, sh_segs, by="Sample.Label")
  obs$Sample.Label <- obs$tr
  obs$tr <- NULL

  obs$size <- 1

  return(list(region=survey@region.table@region.table, sample=segs, obs=obs))
}
