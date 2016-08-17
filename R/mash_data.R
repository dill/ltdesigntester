#' Mash together two simulated surveys
#'
#' @param survey1 simulated survey object (output from \code{\link{create.survey.results}})
#' @param survey2 simulated survey object (output from \code{\link{create.survey.results}})
#' @param scale parameter for the \code{plogis}
#' @param location parameter for the \code{plogis}
#' @return a \code{list} with three \code{data.frame}s: \code{obs} the observation table, \code{segs} the segment table and \code{dist} the distance data.
#' @author David L Miller
#' @importFrom stats plogis rbinom
mash_data <- function(survey1, survey2, scale, location){

  # get the data into shape
  dat1 <- dsmify(survey1)
  dat2 <- dsmify(survey2)

  segs <- dat1$segs

  labs <- 1:nrow(dat1$segs)
  pp <- plogis(labs, scale=scale, location=location)

  picker <- as.logical(rbinom(length(labs), 1, pp))
  ones <- (1:nrow(segs))[picker]
  twos <- (1:nrow(segs))[!picker]

  # get the data for each bit
  obs <- rbind(dat1$obs[dat1$obs$Sample.Label %in% ones,],
               dat2$obs[dat2$obs$Sample.Label %in% twos,])

  dist <- rbind(dat1$dist[dat1$dist$Sample.Label %in% ones,],
                dat2$dist[dat2$dist$Sample.Label %in% twos,])
  # add in the covariate
  dist$weather <- c(rep(0,sum(dat1$dist$Sample.Label %in% ones)),
                    rep(1,sum(dat2$dist$Sample.Label %in% twos)))
  # remove duplicate observations, perfering data from "survey1"
  dist <- dist[!duplicated(dist$object),]

  # add in the weather covariate to the segments
  segs$weather <- 0
  segs$weather[segs$Sample.Label %in% twos] <- 1


  return(list(dist=dist, obs=obs, segs=segs))
}

