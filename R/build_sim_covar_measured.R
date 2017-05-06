#' Set up a simulation with a measured covariate
#'
#' This sets up a simulation where we have a covariate we already measured. For each segment we have a value for that covariate, we give this function a detection function for each covariate.
#'
#' \code{survey_spec_list} has as many elements as there are levels of the covariate. Truncation from \code{survey_spec_list[[1]]} will be used.
#'
#' @param survey_spec_list \code{list} of two simulation specs (results from \code{\link{build_sim}}
#' @param cov_values covariate values, vector with same length as number of segments
#' @return a \code{list} with three \code{data.frame}s: \code{obs} the observation table, \code{segs} the segment table and \code{dist} the distance data.
#' @author David L Miller
#' @export
build_sim_covar_measured <- function(survey_spec_list, cov_values, survey){

  # get the master spec
  survey_spec <- survey_spec_list[[length(survey_spec_list)]]

  # get "survey results" -- just using this to generate population
  # locations
#  survey <- DSsim::create.survey.results(survey_spec, dht.tables=TRUE)

  # calculate the distances to all possible (<truncation)
  # (this step actually gets carried out in the above call but the
  #  detection function then gets applied, so we need to get the distances
  #  again. Diving into create.survey.results might yield something faster)
  all_dat <- calc.poss.detect.dists.lines(survey@population,
                                          survey@transects,
                                          survey_spec@detectability@truncation)

  # get the data into the right format for dsm (only want seg table)
  dat1 <- dsmify(survey)

  # make the weather covariate per segment
  segs <- dat1$segs
  segs$weather <- cov_values

  # now build the distance/observation data
  all_dat$Sample.Label <- all_dat$transect.ID
  all_dat$transect.ID <- NULL
  all_dat <- merge(all_dat, segs[,c("weather","Sample.Label")],
                   by="Sample.Label")

  # make the detection function samples
  samp <- matrix(NA, ncol=length(survey_spec_list), nrow=nrow(all_dat))
  for(i in seq_along(survey_spec_list)){
    samp[, i] <-  sample_df(survey_spec_list[[i]]@detectability,
                            all_dat$distance)
    samp[, i][all_dat$weather != names(survey_spec_list)[i]] <- FALSE
  }

  # check plots
  #par(mfrow=c(1,2))
  #plot(survey@population@population[,c("x","y")],asp=1,pch=19, col="grey")
  #for(i in seq_along(survey_spec_list)){
  #  points(all_dat[samp[, i],][,c("x","y")],pch=19,col="green")
  #}

  # build the new combined data frame
  new_dat <- all_dat[apply(samp, 1, any), ]

  # deal with duplicates
  dupes <- duplicated(new_dat$object)
  if(any(dupes)){
    tt <- table(new_dat$object)[table(new_dat$object)>1]
    for(i in seq_along(names(tt))){
      allo <- new_dat$object == as.numeric(names(tt))[i]
      saver <- new_dat[allo, ,drop=FALSE][sample(1:sum(allo), 1), ,drop=FALSE]
      new_dat <- new_dat[-which(allo), ]
      new_dat <- rbind(new_dat, saver)
    }
  }

  # check histograms of distances
  #par(mfrow=c(1,length(survey_spec_list)+1))
  #hist(new_dat$distance, main="combined")
  #for(i in seq_along(survey_spec_list)){
  #  hist(new_dat$distance[new_dat$weather==i],
  #       main=paste0("weather= ",i))
  #}

  #par(mfrow=c(1,2))
  #plot(survey@population@population[,c("x","y")],asp=1,pch=19, col="grey")
  #points(all_dat[samp_1,][,c("x","y")],pch="g",col="green")
  #points(all_dat[samp_2,][,c("x","y")],pch="b",col="blue")
  #plot(survey@population@population[,c("x","y")],asp=1,pch=19, col="grey")
  #points(new_dat[new_dat$weather==0,][,c("x","y")],pch="0",col="green")
  #points(new_dat[new_dat$weather==1,][,c("x","y")],pch="1",col="blue")

  # build the observation table
  obs <- new_dat[, c("object", "Sample.Label", "distance")]
  obs$size <- 1


  return(list(obs=obs, segs=segs, dist=new_dat))
}


