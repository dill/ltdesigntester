#' @export
#' @importFrom graphics points segments
plot_survey_results <- function(x, ...){

  plot(x@region@coords[[1]][[1]], type="l", asp=1)
  segments(x0=x@transects@sampler.info$start.X,
           x1=x@transects@sampler.info$end.X,
           y0=x@transects@sampler.info$start.Y,
           y1=x@transects@sampler.info$end.Y)
  points(x@population@population[,c("x","y")], pch=19, cex=0.4, col="grey")
  points(x@ddf.data@ddf.dat[,c("x","y")], pch=19, cex=0.4, col="red")
}
