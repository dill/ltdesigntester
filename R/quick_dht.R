#' Get a quick Horvitz-Thompson estimate of N and CV(N)
#' @export
#' @inheritParams quick_dht_strat
#' @param data the result of calling \code{\link{dsmify}}
#' @return vector of length 2 with abundance and CV
quick_dht <- function(df, data){
  # kill the sample label, it's wrong!
  df$ddf$data$Sample.Label <- NULL
  aa <- dht(df$ddf, data$region, data$sample, data$obs)
  return(as.vector(aa$individuals$N[,c("Estimate","cv")][1,]))
}

#' Get a quick stratified Horvitz-Thompson estimate of N and CV(N)
#'
#' @export
#' @param df detection function
#' @param dht_data the result of calling \code{\link{dhtify}}
#' @param stratification a stratification scheme (at the moment, only points along the \code{x} axis are allowed)
#' @param xlims vector of length 2 with limits of the region in the \code{x} direction
#' @param ylims vector of length 2 with limits of the region in the \code{y} direction
#' @return vector of length 2 with abundance and CV
quick_dht_strat <- function(df, dht_data, stratification, xlims, ylims){

  # setup the region
  region <- dht_data$region
  region <- region[rep(seq(nrow(region)),length(stratification)+1),]
  region$Region.Label <- 1:(length(stratification)+1)
  region$Area <- diff(c(xlims[1], stratification, xlims[2]))*diff(ylims)

  # merge the sample table onto the transect locations to
  # get the end of the transects to do stratificationification
  samplet <- dht_data$sample

  # build the stratificationum data
  begin_stratum <- c(xlims[1], stratification)
  end_stratum   <- c(stratification, xlims[2])
  region.labs <- rep(NA, nrow(samplet))
  for(i_strat in 1:length(begin_stratum)){
    ind <- samplet$x_end <= end_stratum[i_strat] &
           samplet$x_start >= begin_stratum[i_strat]
    region.labs[ind] <- i_strat
  }
  samplet$Region.Label <- region.labs

  obst <- dht_data$obs
  obst$Region.Label <- NULL
  obst <- merge(obst, samplet[,c("Region.Label","Sample.Label")],
                by="Sample.Label")


  # kill the sample label, it's wrong!
  df$ddf$data$Sample.Label <- NULL

  # 2-4-6-8, it's time for us to estimate
  aa <- dht(df$ddf, region, samplet, obst)
  return(aa$individuals$N[,c("Estimate","cv")][nrow(aa$individuals$N),])
}

