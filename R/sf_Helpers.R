#' @title Better random point sample
#'
#' @description \code{sf_sample()} is a wrapper on st_sample that allows it to
#'   return an sf object with a data frame.
#'
#' @param num Integer. How many samples to draw?
#' @param sampleArea Polygon depicting study area to draw from.
#'
#' @return Returns an sf object with one column (id) that gives a unique
#'   identifier to each point created.
#'
#' @section Creation notes: First created on 2019-Mar-6 in Lab4inR.R as I was
#'   working to create a NetLogo Program in R.
#'
#' @examples
#'
#' @export
sf_sample <- function(num, sampleArea) {
  sf::st_sample(sampleArea, num) %>%
    data.frame("id"=seq(num), .) %>% sf::st_sf()
}


#' @title Add XY columns from point sf
#'
#' @description \code{sf_XYtoCols()} adds and x and y column to the sf dataframe
#' from the sf geometry.
#'
#' @param x sf Point feature
#' @param names Names for x and y columns. (Default is c("x", "y"))
#'
#' @return Returns the inputed sf object with two new columns.
#'
#' @section Creation notes: First conceptualized on 2019-Mar-6 in Lab4inR.R as
#' I was working to create a NetLogo Program in R. But I was able to find code
#' online that fit my needs here: https://github.com/r-spatial/sf/issues/231
#'
#' @examples
#'
#' @export
sf_XYtoCols <- function(x, names = c("x","y")) {
  stopifnot(inherits(x,"sf") && inherits(sf::st_geometry(x),"sfc_POINT"))
  ret <- sf::st_coordinates(x)
  ret <- tibble::as_tibble(ret)
  stopifnot(length(names) == ncol(ret))
  x <- x[ , !names(x) %in% names]
  ret <- setNames(ret,names)
  dplyr::bind_cols(x,ret)
}
