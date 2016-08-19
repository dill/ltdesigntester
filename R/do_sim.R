#' Run a simulation of a given scenario, fit models, report results
#'
#' Given a simulation scenario, this function will generate from that specification, fit models to the data and record the results.
#'
#'
#' @param nsim the number of simulations to run
#' @param scenario all the survey information, result of \code{\link{build_sim}}
#' @param pred_dat prediction \code{data.frame}
#' @param stratification the stratification scheme to use for the stratified Horvtiz-Thompson estimate, specified as internal cutpoints
#' @return \code{data.frame} with one row per (successful) simulation. The \code{data.frame} has the following columns:
#' \describe{
#' \item{\code{model}}{which model was fitted to the data}
#' \item{\code{iter}}{which simulation this result is for}
#' \item{\code{quantile}}{the quantile of the estimated distribution of abundance that the true abundance is in}
#' \item{\code{Nhat}}{the estimated abundance}
#' \item{\code{cvN}}{the coefficient of variation of the estimated abundance}
#' \item{\code{n}}{number of observed objects}
#' \item{\code{sp1}}{smoothing parameter 1}
#' \item{\code{sp2}}{smoothing parameter 2}}
#' @export
#' @author David L Miller
#' @importFrom graphics plot
#' @importFrom stats plnorm setNames
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @importFrom plyr ldply laply
#' @importFrom Distance ds
#' @importFrom mrds dht
#' @importFrom dsm dsm dsm_varprop
#' @importFrom mgcv tw
do_sim <- function(nsim, scenario, pred_dat, stratification){

  # results storage
  big_res <- c()

  # build a progress bar
  pb <- txtProgressBar(min=1, max=nsim, style=3)

  # rotation matrix
  R <- matrix(c(cos(pi/4), sin(pi/4), -sin(pi/4), cos(pi/4)),2,2)

  # what is the true population size?
  true_N <- scenario@population.description@N

  # iterate over the simulations
  for(ii in 1:nsim){

    # generate a survey
    survey_res <- DSsim::create.survey.results(scenario, dht.tables=TRUE)

    # put the data in dsm format
    dsm_data <- dsmify(survey_res)
    dist.data <- dsm_data$dist
    obs <- dsm_data$obs
    segs <- dsm_data$segs
    rm(dist.data); gc()

    # rotated segs
    segs[,c("xr","yr")] <- t(R %*% t(segs[,c("x","y")]))

    # fit a detection function
    df_model <- suppressMessages(try(ds(dist.data, key="hr", adjustment=NULL)))

    # if something goes wrong, move on
    if(any(class(df_model) == "try-error") || abs(df_model$ddf$par[1])<1e-6){
      next
    }

    # model list object
    ll <- list()

    # function that times out, so our fits don't go on forever
    timer <- function(x){
      x <- substitute(x)
      setTimeLimit(elapsed=300) # 5 mins?
      on.exit(setTimeLimit(elapsed=Inf))
      model <- try(eval(x))
      if(any(class(model)=="try-error")){
        return(NULL)
      }else{
        return(model)
      }
    }

    # thin plate
    ll[["m_xy_tp"]] <- timer(dsm(count~s(x, y, bs="tp"), df_model, segs, obs,
                                 method="REML", family=tw(a=1.2)))
    # thin plate te
    ll[["m_xy_te"]] <- timer(dsm(count~te(x, y, bs="tp"), df_model, segs, obs,
                                 method="REML", family=tw(a=1.2)))
    # thin plate te
    ll[["m_xyr_te"]] <- timer(dsm(count~te(xr, yr, bs="tp"), df_model, segs,
                                  obs, method="REML", family=tw(a=1.2)))
    # thin plate rotation
    ll[["m_xyr_tp"]] <- timer(dsm(count~s(xr, yr, bs="tp"), df_model, segs, obs,
                                  method="REML", family=tw(a=1.2)))
    # thin plate w/ shrinkage
    ll[["m_xy_ts"]] <- timer(dsm(count~s(x, y, bs="ts"), df_model, segs, obs,
                                 method="REML", family=tw(a=1.2)))
    # Duchon
    ll[["m_xy_ds"]] <- timer(dsm(count~s(x, y, bs="ds", m=c(1, 0.5)), df_model,
                                 segs, obs, method="REML", family=tw(a=1.2)))


    # process -- get N and CVs for the spatial models
    all_res <- ldply(ll, function(x){
      if(x$converged){
        xx <- dsm_varprop(x, pred_dat)
        N <- sum(xx$pred)
        cv <- sqrt(xx$var[1,1])/N
        return(c(N, cv))
      }else{
        return(c(NA, NA))
      }
    })

    # get N and CVs for the HT model
    HT <- quick_dht(df_model, survey_res)

    # for the stratified model
    HT_strat <- quick_dht_strat(df_model, survey_res, stratification)

    # bind them together
    all_res <- rbind.data.frame(all_res,
                                c("HT", unname(HT)),
                                c("HT_strat", unname(HT_strat)))

    all_res$V1 <- as.numeric(all_res$V1)
    all_res$V2 <- as.numeric(all_res$V2)

    # get the quantiles
    qs <- apply(all_res[,-1], 1,
                function(x) get_N_quantile(N=true_N, Nhat=x[1], cvN=x[2]))

    # build some storage for this set of results
    res <- data.frame(model    = all_res[,1],
                      iter     = ii,
                      quantile = qs)

    res <- cbind(res, setNames(all_res[,2:3], c("Nhat", "cvN")))

    ## save smoothing pars
    # bind on the smoothing parameters
    sps <- laply(ll, function(x) {
                   xx <- c(sp1=NA, sp2=NA)
                   xx[1:length(x$sp)] <- x$sp
                   xx})
    # no smoothing parameter for HT
    sps <- rbind(sps, c(NA,NA), c(NA,NA))

    ## save # samples
    res$n <- nrow(dist.data)

    res <- cbind(res, sps)

    # bind to the rest
    big_res <- rbind(big_res, res)

    setTxtProgressBar(pb, ii)

  }

  return(big_res)
}