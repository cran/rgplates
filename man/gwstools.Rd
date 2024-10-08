% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/online.R
\name{getgws}
\alias{getgws}
\alias{setgws}
\alias{checkgws}
\title{Return and set the remote URL for the GPlates Web Service}
\usage{
getgws()

setgws(url = "", check = TRUE, reset = FALSE, silent = FALSE)

checkgws(silent = FALSE)
}
\arguments{
\item{url}{(\code{character}) A single string specifying the URL of the GPlates Web Service (with trailing slash).}

\item{check}{(\code{logical}) Flag to specify whether the immediate querying of the GWS is to be performed? If this fails the url won't be set!}

\item{reset}{(\code{logical}) Flag to specify whether the factory default should be reset.}

\item{silent}{Logical flag indicating wheth the output should be silent?}
}
\value{
\code{getws} returns a single character string with the URL of the GWS.

Invisible return, either FALSE, or a character string with the version number.
}
\description{
This set of functions allows the configuration of the remote URL, so the R client package can be used with a different instance of the GPlates web service, including a local implementation (served on localhost).

The function will use the http get method to access the version number of the GPlates Web Service.
}
\details{
The \code{getws} function returns the current url of the GPLates Web Service (defaults to: \code{https://gws.gplates.org/}).
The \code{setws} function allows the setting of GPLates Web Service URL.
}
\examples{
# Access currently set remote URL.
getgws()
# In case you have the GWS running on localhost (default port 18000):
# At time of writing this, the local instance does not return version, checking
# does not work!
setgws("http://localhost:18000/", check=FALSE)
# To reset factory defaults
setgws(reset=TRUE, check=FALSE)
}
