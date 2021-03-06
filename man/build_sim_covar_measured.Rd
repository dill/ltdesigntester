% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/build_sim_covar_measured.R
\name{build_sim_covar_measured}
\alias{build_sim_covar_measured}
\title{Set up a simulation with a measured covariate}
\usage{
build_sim_covar_measured(survey_spec_list, cov_values, survey)
}
\arguments{
\item{survey_spec_list}{\code{list} of two simulation specs (results from \code{\link{build_sim}}}

\item{cov_values}{covariate values, vector with same length as number of segments}
}
\value{
a \code{list} with three \code{data.frame}s: \code{obs} the observation table, \code{segs} the segment table and \code{dist} the distance data.
}
\description{
This sets up a simulation where we have a covariate we already measured. For each segment we have a value for that covariate, we give this function a detection function for each covariate.
}
\details{
\code{survey_spec_list} has as many elements as there are levels of the covariate. Truncation from \code{survey_spec_list[[1]]} will be used.
}
\author{
David L Miller
}
