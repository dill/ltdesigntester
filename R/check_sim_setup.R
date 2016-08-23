#' Check simulation setup graphically
#'
#' Show some plots to check if a survey has been setup properly. Produces an image plot of the underlying uncertainty, the deteection function, a realisation of a survey showing which object were observed and a histogram of observed distances.
#'
#' @author David L Miller, based on code from Laura Marshall.
#' @param simsetup a simulation object, i.e., the result of running \code{\link{build_sim}}.
#' @return just plots.
#' @export
#' @importFrom grDevices heat.colors
#' @importFrom graphics hist image par
#' @import methods
#' @import DSsim
# @importMethodsFrom DSsim plot
# @importClassesFrom DSsim Survey.Results
check_sim_setup <- function(simsetup) {
  # small function to plot the simulation setup input: simsetup - object
  # created by make.simulation() output: 4-panel plot of study region with
  # popn, study region with transects study region with detected objects,

  # simulate the survey process of detection
  eg.survey <- DSsim::create.survey.results(simsetup)
  # grab the distance data
  dist.data <- DSsim::get.distance.data(eg.survey)

  opar <- par(mfrow = c(2, 2))

  # theoretical section
  # this is uber slow:
  #plot(simsetup@population.description@density,
  #     plot.units = "m", style="blocks",
  #     density.col=heat.colors(50))
  den_obj <- simsetup@population.description@density@density.surface[[1]]
  xdim <- attr(den_obj, "out.attrs")$dim[1]
  ydim <- attr(den_obj, "out.attrs")$dim[2]
  image(z = matrix(den_obj$density, xdim, ydim),
        x = seq(min(den_obj$x), max(den_obj$x), len=xdim),
        y = seq(min(den_obj$y), max(den_obj$y), len=ydim),
        xlab="x", ylab="y",
        col=heat.colors(50), asp=1)

  # plot the detection function
  plot_df(simsetup@detectability)

  # what actually happened
  #plot(eg.survey)
  plot_survey_results(eg.survey)
  # histogram of detection distances
  hist(dist.data$distance, xlab = "Distance", main = "Distance Data")

  # print some stuff
  cat("Observations:", nrow(dist.data), "\n")

  par(opar)

  invisible(NULL)
}
