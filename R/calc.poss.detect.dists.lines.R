#' @importFrom graphics points
#' @importFrom rgeos gBuffer gIntersects gDistance
#' @importFrom sp SpatialPointsDataFrame SpatialPoints Lines Line SpatialLinesDataFrame SpatialLines coordinates
calc.poss.detect.dists.lines <- function(population, survey, perp.truncation, plot = FALSE){
  transects <- survey@sampler.info
  individuals <- population@population

  # are points inside the lines?
  # make individuals points
  ind_sp <- SpatialPointsDataFrame(SpatialPoints(individuals[,c("x","y")]),
                                   individuals)
  # make lines lines with width
  l <- list()
  for(ii in 1:nrow(transects)){
    l[[ii]] <- matrix(as.numeric(transects[ii,c("start.X","end.X",
                                    "start.Y","end.Y")]), ncol=2)
    l[[ii]] <- Lines(list(Line(l[[ii]])), ID=transects$ID[ii])
  }
  spl <- SpatialLinesDataFrame(SpatialLines(l), transects)

  # make a truncation distance buffer around the line
  spl_b <- gBuffer(spl, width=perp.truncation, capStyle="SQUARE")

  # which points are within the buffer
  #available_obs <- gIntersection(spl_b, ind_sp)
  available_ind <- which(gIntersects(ind_sp, spl_b, byid=TRUE))

  # distances between points and lines
  dists <- c()

  for(tr in 1:nrow(transects)){
    this_line <- transects[tr, c("start.X", "start.Y", "end.X", "end.Y")]

    # use Devin's code to get perp distances
    ddists <- dist2line(individuals[,c("x","y")][available_ind, ,drop=FALSE],
                       this_line)$distance
    ddists <- abs(ddists)
    #ddists <- ddists[ddists <= perp.truncation]

    # if there is a distance, add it to the data
    if(length(ddists)==0) next
    td <- data.frame(object = individuals$object[available_ind],
                     transect.ID = rep(tr, length(ddists)),
                     distance = ddists,
                     available = rep(TRUE, length(ddists)))
    td <- td[td$distance <= perp.truncation, , drop=FALSE]

    # if td is > 1 row, randomly pick a segment that it comes from
    # otherwise we get duplicates
    td <- td[sample(1:nrow(td), 1), ]
    dists <- rbind(dists, td)
  }

  # merge distance data back onto the individuals we started with
  # to pick up the other columns
  poss.detect.dists <- merge(dists, individuals, by="object")

  # plot if desired
  if(plot){
    transect.IDs <- sort(unique(poss.detect.dists$transect.ID))
    for(i in seq(along = transect.IDs)){
      points(poss.detect.dists$x[poss.detect.dists$transect.ID == transect.IDs[i]], poss.detect.dists$y[poss.detect.dists$transect.ID == transect.IDs[i]], col = i, pch = 20)
      points(poss.detect.dists$x[poss.detect.dists$transect.ID == transect.IDs[i]], poss.detect.dists$y[poss.detect.dists$transect.ID == transect.IDs[i]], col = i)
    }
  }
  return(poss.detect.dists)
}

dist2line=function(object.ppp, line.ends)
################################################################################
# Calculates perpendicular distance of a point process contained within a strip
# to center line of the strip they are contained in.  It also computes the position
# on the line. This is the inverse of the offset.points function.
#
# Arguments:
#
#   object.ppp - point process for observations in a strip
#   line.ends  - ends of line x0,y0,x1,y1
#
# Value:  list with elements
#    distVals  - vector of perpendicular distances
#    projection- dataframe of projected locations on the line
#
# Devin Johnson
# March 4 2008
################################################################################
{
  x.bar = object.ppp$x
  y.bar = object.ppp$y
  x0 = as.double(line.ends[1])
  y0 = as.double(line.ends[2])
  x1 = as.double(line.ends[3])
  y1 = as.double(line.ends[4])
  d = sum((x0-x1)^2 + (y0-y1)^2)
  u.bar = ((x.bar-x0)*(x1-x0) + (y.bar-y0)*(y1-y0))/d
  p.x = x0+u.bar*(x1-x0)
  p.y = y0+u.bar*(y1-y0)
  distVals = sqrt((x.bar-p.x)^2 + (y.bar-p.y)^2)
  return(list(distance=distVals, projection=data.frame(x=p.x,y=p.y)))
}


