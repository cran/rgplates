% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{gws}
\alias{gws}
\title{Valid reconstructable feature collections of the GPlates Web Service}
\format{
A \code{data.frame} with 5 variables and 26 observations.
}
\usage{
data(gws)
}
\description{
This is version 1.1. The object contains valid returns of GWS v0.2.1, as of 2024-09-03.
}
\details{
The valid return combinations for selected models and feature collections are copied from \url{https://gwsdoc.gplates.org/models}. 

\describe{
	\item{\code{model}}{The name of the reconstruction model.}
	\item{\code{feature}}{The name of the feature collection.}
	\item{\code{from}}{The oldest reconstruction age accepted by the model to return the feature collection.}
	\item{\code{to}}{The youngest reconstruction age accepted by the model to return the feature collection.}
	\item{\code{description}}{The short description of the feature collection.}
}
}
\keyword{datasets}
