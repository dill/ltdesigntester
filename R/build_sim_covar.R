#' Set up a simulation with a 2-level covariate
#'
#' This sets up a simulation where we have a 2-level covariate, which we'll call "weather". This covariate will effect detectability and varies in space from left to right across the study area. The way in which it varies is controlled by a logistic function.
#'
#' \code{survey_spec_list[[1]]} should only have two elements. Truncation from \code{survey_spec_list[[1]]} will be used.
#'
#' @param survey_spec_list \code{list} of two simulation specs (results from \code{\link{build_sim}}
#' @param logit_scale the scale of the logit use to define the covariate
#' @param logit_location the location of the logit use to define the covariate
#' @return a \code{list} with three \code{data.frame}s: \code{obs} the observation table, \code{segs} the segment table and \code{dist} the distance data.
#' @author David L Miller
#' @export
build_sim_covar <- function(survey_spec_list, logit_scale, logit_location){

  if(length(survey_spec_list)!=2){
    stop("survey_spec_list doesn't have exactly two elements")
  }

  # get the master spec
  survey_spec <- survey_spec_list[[1]]

  # get "survey results" -- just using this to generate population
  # locations
  survey <- DSsim::create.survey.results(survey_spec, dht.tables=TRUE)

  # calculate the distances to all possible (<truncation)
  all_dat <- calc.poss.detect.dists.lines(survey@population,
                                          survey@transects,
                                          survey_spec@detectability@truncation)

  # get the data into the right format for dsm (only want seg table)
  dat1 <- dsmify(survey)

  # make the weather covariate per segment
  segs <- dat1$segs
  segs$weather <- rbinom(nrow(segs), 1, plogis(segs$x, scale=logit_scale,
                                               location=logit_location))
  segs$weather <- as.factor(segs$weather)

  # now build the distance/observation data
  all_dat$Sample.Label <- all_dat$transect.ID
  all_dat$transect.ID <- NULL
  all_dat <- merge(all_dat, segs[,c("weather","Sample.Label")],
                   by="Sample.Label")
  # make the detection function samples
  samp_1 <- sample_df(survey_spec_list[[1]]@detectability, all_dat$distance)
  samp_2 <- sample_df(survey_spec_list[[2]]@detectability, all_dat$distance)

  # check plots
  #plot(survey@population@population[,c("x","y")],asp=1,pch=19, col="grey")
  #points(all_dat[samp_1,][,c("x","y")],pch=19,col="green")
  #points(all_dat[samp_2,][,c("x","y")],pch=19,col="blue")
  #points(all_dat[samp_1 & samp_2,][,c("x","y")],pch=19,col="red")

  # build the new combined data frame
  new_dat <- all_dat[samp_1 | samp_2,]
  new_dat <- new_dat[!duplicated(new_dat$object),]

  # check histograms of distances
  #par(mfrow=c(1,3))
  #hist(new_dat$distance, main="combined")
  #hist(new_dat$distance[new_dat$weather==0], main="weather=0, good weather")
  #hist(new_dat$distance[new_dat$weather==1], main="weather=1, bad weather")

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


