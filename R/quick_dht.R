# Get a quick Horvitz-Thompson estimate of N and CV(N)
#' @export
quick_dht <- function(df, data){
  # kill the sample label, it's wrong!
  df$ddf$data$Sample.Label <- NULL
  aa <- dht(df$ddf, data$region, data$sample, data$obs)
  return(as.vector(aa$individuals$N[,c("Estimate","cv")][1,]))
}

# with stratification about x=1.5
#' @export
quick_dht_strat <- function(df, data, strat){

  # setup the region
  region <- data$region
  region <- region[rep(seq(nrow(region)),length(strat)+1),]
  region$Region.Label <- 1:(length(strat)+1)
  region$Area <- diff(c(0, strat, 3))

  # merge the sample table onto the transect locations to
  # get the end of the transects to do stratification
  samplet <- merge(data$sample.table,
                   survey_res@transects@sampler.info,
                   by.x="Sample.Label",by.y="ID")

  # build the stratum data
  begin_stratum <- c(0,strat)
  end_stratum   <- c(strat,3)
  region.labs <- rep(NA, nrow(samplet))
  for(i_strat in 1:length(begin_stratum)){
    ind <- samplet$end.X <= end_stratum[i_strat] &
           samplet$end.X > begin_stratum[i_strat]
    region.labs[ind] <- i_strat
  }
  samplet$Region.Label <- region.labs

  obst <- survey_res@obs.table@obs.table
  obst$Region.Label <- NULL
  obst <- merge(obst, samplet[,c("Region.Label","Sample.Label")],
                by="Sample.Label")
  aa <- dht(df$ddf, region,
            samplet, obst)
  return(aa$individuals$N[,c("Estimate","cv")][nrow(aa$individuals$N),])
}

