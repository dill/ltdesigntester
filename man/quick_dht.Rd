% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/quick_dht.R
\name{quick_dht}
\alias{quick_dht}
\title{Get a quick Horvitz-Thompson estimate of N and CV(N)}
\usage{
quick_dht(df, data)
}
\arguments{
\item{df}{detection function}

\item{data}{the result of calling \code{\link{dsmify}}}
}
\value{
vector of length 2 with abundance and CV
}
\description{
Get a quick Horvitz-Thompson estimate of N and CV(N)
}
