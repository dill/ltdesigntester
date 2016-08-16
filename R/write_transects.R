#' Write out a transect as a shapefile
#'
#' Given a \code{data.frame} with the locations of transect legs in it, create a shapefile with those transects in it.
#'
#' @param transect should have the following columns: \code{x}, \code{y} the coordinates of endpoints and \code{leg} survey leg associated with this transect.
#' @param file filename (folder name) to write to
#' @return nothing, just writes the file
#' @author David L Miller
#' @export
# this is a completely Byzantine process
write_transects <- function(transect, file){

  llist <- list()

  Sample.Label <- c()

  # make each leg a different Line object
  for(this_leg in unique(transect$leg)){

    # get the transect bits for this leg
    tr <- transect[transect$leg==this_leg,]


    for(i in 1:(nrow(tr)-1)){
      this_label <- paste0(this_leg, "-", i)
      ll <- sp::Line(tr[,c("x","y")][i:(i+1),])
      llist <- c(llist, sp::Lines(ll, ID=this_label))
      Sample.Label <- c(Sample.Label, this_label)
    }
  }


  ll <- sp::SpatialLines(llist)

  dat <- data.frame(Sample.Label=Sample.Label)
  rownames(dat) <- Sample.Label

  ll <- sp::SpatialLinesDataFrame(ll, data=dat)

  rgdal::writeOGR(ll, file, "data", "ESRI Shapefile" )

  invisible(NULL)
}
