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



#' @title Clip points using \code{sp::over}
#'
#' @description Clips points by converting sf features to sp, using
#' \code{sp::over}, then converting back to sf for the output. Can be faster
#' than some sf methods.
#'
#' @param inputPoints sf Point object
#' @param clipPoly sf Polygon object
#'
#' @return Returns an sf object clipped to the \code{clipPoly}.
#'
#' @section Creation notes: First created on 2019-Mar-21 in 1-2_FIA_CleanRawData.R
#'
#' @examples
#'
#' @export
sf_ClipPointsSPover <- function(inputPoints, clipPoly) {

  #coerce to sp
  inputPoints_sp <- as(inputPoints, "Spatial")
  clipPoly_sp <- as(clipPoly, "Spatial")

  #clip with over operator (only works well for points)
  clippedPoints_sp <- inputPoints_sp[clipPoly_sp,]

  #coerce result to sf
  clippedPoints_sf <- sf::st_as_sf(clippedPoints_sp)

  return(clippedPoints_sf)
}




#' @title Make extent class object for \code{sf} object
#'
#' @description Makes an extent class object using \code{sf::st_bbox} and
#'   \code{raster::extent()}.
#'
#' @param sfLayer sf object
#'
#' @return Returns an extent object from \code{raster::extent()}.
#'
#' @section Creation notes: First created on 2019-Apr-02 in
#'   1-2_FIA_CleanRawData.R
#'
#' @section Future directions: I hope to create a new extent class method with
#'   this in the future once I learn more about class specific methods.
#'
#' @examples
#'
#' @export
sf_extent <- function(sfLayer) {
  bbox <- sf::st_bbox(sfLayer)
  extentBbox <- bbox[c(1,3,2,4)]
  ext <- raster::extent(extentBbox)
  return(ext)
}


#' @title Make polygon grid over \code{sf} feature using lat-lon coordinates
#'
#' @description Make polygon grid over \code{sf} feature using lat-lon
#'   coordinates. This can be used to split data into regions determined by the
#'   exported grid. Can be used to create a standard grid using the bounding box
#'   of the original feature or a 'pretty' grid that locks grid edges to more
#'   even numbers.
#'
#' @param sfLayer sf object
#' @param gridSize grid resolution, uses coordinate system measurement. Default:
#'   1
#' @param prettyGrid logical, Use pretty grid measuremetns? Defalut: TRUE
#' @param epsg an EPSG key
#' @param output character, Do you want a polygon or raster output of the
#'   estimated grid?
#'
#' @return Returns either a raster or sf Polygon grid to overlay over the
#'   sfLayer data.
#'
#' @section Creation notes: First created on 2019-Apr-02 in
#'   1-2_FIA_CleanRawData.R
#'
#' @examples
#'
#' @export
sf_LatLongGrid <- function(sfLayer, gridSize = 1, prettyGrid = TRUE, epsg = 4269, output = c("polygon", "raster")) {

  # match output method
  output <- match.arg(output)

  # make sure data is in a lat long transformation
  sfLayer <- sf::st_transform(sfLayer, crs = epsg)

  # get extent for sfLayer
  ext <- jpfxns2::sf_extent(sfLayer)

  # create raster to base grid off of
  if (isTRUE(prettyGrid)) {
    lonMin <- (floor(ext@xmin) + floor(ext@xmin) %% gridSize)
    lonMax <- (ceiling(ext@xmax) + ceiling(ext@xmax) %% gridSize)
    latMax <- (ceiling(ext@ymax) + ceiling(ext@ymax) %% gridSize)
    latMin <- (floor(ext@ymin) + floor(ext@ymin) %% gridSize)
    ras <- raster::raster(xmn = lonMin, xmx = lonMax, ymn = latMin, ymx = latMax,
                          crs = sp::CRS(paste0("+init=epsg:", epsg)), resolution = gridSize, vals=1)
  } else {
    ras <- raster::raster(ext = ext, crs = sp::CRS(paste0("+init=epsg:", epsg)), resolution = gridSize, vals=1)
  }

  # convert to polygon object if needed
  if (output == "polygon") {
    poly <- raster::rasterToPolygons(ras)
    poly <- sf::st_as_sf(poly) %>% dplyr::mutate(ID = as.integer(rownames(.))) %>% dplyr::select(ID)
    return(poly)
  } else {
    return(ras)
  }
}



#' @title Make Study Area Polygons
#'
#' @description Make study area polygons from the extent (bbox) of an sf layer.
#'   Includes scaling options to increase or decrease size of the polygon by a
#'   ratio or by a distance calculated in map units. Because of this, the
#'   distance parameter only works for projected layers.
#'
#' @param sfLayer sf object
#' @param ratio Numerical vector of length 1 or 2 (as it pertains to the x,y
#'   axes). Default: 1 to create a basic study area polygon for the sfLayer.
#'   Ignored if a \code{dist} is given.
#' @param dist Numerical vector of length 1 or 2 (as it pertains to the x,y
#'   axes). Default: \code{NULL}.
#'
#' @return Returns a polygon sf layer that is a rectangle.
#'
#' @section Creation notes: First created on 2019-Apr-22 while working on my IBM
#'   class project.
#'
#' @section Future directions: I may need to add checks to make sure negative
#'   distance values won't flip the min and max values in the extent creation
#'   section.
#'
#' @examples
#'
#' @export
sf_studyArea <- function(sfLayer, ratio = 1, dist = NULL) {

  if (length(ratio) == 1) {ratio <- rep(ratio, 2)}

  ext <- jpfxns2::sf_extent(sfLayer)

  if (!is.null(dist)) {
    if (length(dist) == 1) {dist <- rep(dist, 2)}

    ratio[1] <- (dist[1] / (ext@xmax - ext@xmin)) + 1
    ratio[2] <- (dist[2] / (ext@ymax - ext@ymin)) + 1
  }

  ext@xmin <- ext@xmin - (((ext@xmax - ext@xmin) * ratio[1]) - (ext@xmax - ext@xmin))
  ext@xmax <- ext@xmax + (((ext@xmax - ext@xmin) * ratio[1]) - (ext@xmax - ext@xmin))
  ext@ymin <- ext@ymin - (((ext@ymax - ext@ymin) * ratio[2]) - (ext@ymax - ext@ymin))
  ext@ymax <- ext@ymax + (((ext@ymax - ext@ymin) * ratio[2]) - (ext@ymax - ext@ymin))

  SA <- as(ext, "SpatialPolygons") %>%
    sf::st_as_sf()

  return(SA)
}
