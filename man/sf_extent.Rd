% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sf_Helpers.R
\name{sf_extent}
\alias{sf_extent}
\title{Make extent class object for \code{sf} object}
\usage{
sf_extent(sfLayer)
}
\arguments{
\item{sfLayer}{sf object}
}
\value{
Returns an extent object from \code{raster::extent()}.
}
\description{
Makes an extent class object using \code{sf::st_bbox} and
  \code{raster::extent()}.
}
\section{Creation notes}{
 First created on 2019-Apr-02 in
  1-2_FIA_CleanRawData.R
}

\section{Future directions}{
 I hope to create a new extent class method with
  this in the future once I learn more about class specific methods.
}

\examples{

}
