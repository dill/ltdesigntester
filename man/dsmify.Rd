% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dsmify.R
\name{dsmify}
\alias{dsmify}
\title{Make DSsim data usable by dsm}
\usage{
dsmify(survey)
}
\arguments{
\item{survey}{a simulated survey}
}
\value{
a \code{list} with three \code{data.frame}s: \code{obs} the observation table, \code{segs} the segment table and \code{dist} the distance data.
}
\description{
Get the simulated data and put it into a format that \code{dsm} can use. The consists of creating the three tables used by \code{\link{dsm}}.
}
\author{
David L Miller
}
