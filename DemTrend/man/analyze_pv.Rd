% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/analyze_pv.R
\name{analyze_pv}
\alias{analyze_pv}
\title{analyze_pv}
\usage{
analyze_pv(alpha, return_pv)
}
\arguments{
\item{alpha}{is a vector of alpha values to compare p-values to and to assess power. Default alpha values are alpha=seq(from=0.01, to=0.2, by=delta)}

\item{return_pv}{is list with p-values returned from regression analysis}
}
\description{
Analyzes the output p-values from the calc_pv function. This function can be used to analyze multiple
species at a time.
}
\references{
data derived from COMPADRE Plant Matrix Database. Max Planck Institute for Demographic Research (Germany). Available at www.compadre-db.org (data downloaded on [1/1/2014]).
}
\author{
Elizabeth Hiroyasu
}
