#' Create a survey object for simulations
#'
#' Build a survey object using machinery from \code{DSsim}. Specify a design, region, population size, resolution and a density surface to generate the relevant simulation settings.
#'
#' @param design_path the path to the design shapefile
#' @param dsurf density surface
#' @param n_grid number of cells in each direction in \code{dsurf} (assumed square)
#' @param n_grid_x number of cells in \code{x} direction in \code{dsurf} (if non-square)
#' @param n_grid_y number of cells in \code{y} direction in \code{dsurf} (if non-square)
#' @param n_pop true population size
#' @param region_path path to region shapefile
#' @param df detection function \code{list} (elements: \code{key}, \code{scale}, \code{shape}, \code{truncation})
#' @param n_sim number of simulations to generate
#' @return simulation object ready to be run with \code{create.survey.results}.
#' @author David L Miller
#' @export
#' @importFrom shapefiles read.shapefile
build_sim <- function(design_path, region_path, dsurf, df, n_pop=500, n_sim=1,
                      n_grid=NULL, n_grid_x=NULL, n_grid_y=NULL){

  if(is.null(n_grid_x) | is.null(n_grid_y)){
    n_grid_x <- n_grid_y <- n_grid
  }

  # get the study region
  region.shapefile <- shapefiles::read.shapefile(region_path)
  region <- DSsim::make.region(region.name = "Survey Region", units = "m",
                               shapefile = region.shapefile)

  # populate the region with the density
  if(length(region@coords) > 1){
    stop("You're using a region with multiple polygons? Everything will break. E-mail Dave for support!")
  }
  x.space <- diff(range(region@coords[[1]][[1]][,1]))/n_grid_x
  y.space <- diff(range(region@coords[[1]][[1]][,1]))/n_grid_y
  pop.density <- DSsim::make.density(region=region, density.surface=list(dsurf),
                                     x.space = x.space, y.space = y.space)

  pop.description <- DSsim::make.population.description(region.obj=region,
                                density.obj=pop.density, N=n_pop, fixed.N=TRUE)

  # build the detection function
  if(df$key=="hr"){
    detect <- DSsim::make.detectability(key.function = df$key,
                                        scale.param = df$scale,
                                        shape.param = df$shape,
                                        truncation = df$truncation)
  }else{
    detect <- DSsim::make.detectability(key.function = df$key,
                                        scale.param = df$scale,
                                        truncation = df$truncation)
  }

  this_design <- DSsim::make.design(transect.type = "Line",
                                    design.details = c("user specified"),
                                    region = region, plus.sampling = FALSE,
                                    path = design_path)

  # set up an analysis that we won't use
  ddf.analyses <- DSsim::make.ddf.analysis.list(
                                    dsmodel=list(#~cds(key="hn", formula=~1)),
                                                 ~cds(key="hr", formula=~1)),
                                     method = "ds", criteria = "AIC")

  # setup the simulation preferences
  my_simulation <- DSsim::make.simulation(reps=n_sim, single.transect.set=TRUE,
                                   region.obj=region, design.obj=this_design,
                                   population.description.obj=pop.description,
                                   detectability.obj=detect,
                                   ddf.analyses.list=ddf.analyses)

  return(my_simulation)
}

