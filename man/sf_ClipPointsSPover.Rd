% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sf_Helpers.R
\name{sf_ClipPointsSPover}
\alias{sf_ClipPointsSPover}
\title{Clip points using \code{sp::over}}
\usage{
sf_ClipPointsSPover(inputPoints, clipPoly)
}
\arguments{
\item{inputPoints}{sf Point object}

\item{clipPoly}{sf Polygon object}
}
\value{
Returns an sf object clipped to the \code{clipPoly}.
}
\description{
Clips points by converting sf features to sp, using
\code{sp::over}, then converting back to sf for the output. Can be faster
than some sf methods.
}
\section{Creation notes}{
 First created on 2019-Mar-21 in 1-2_FIA_CleanRawData.R
}

\examples{

}
