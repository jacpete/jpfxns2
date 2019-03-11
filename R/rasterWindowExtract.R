#' @title Extract raster data within a window
#'
#' @description Raster package was missing a function to easily extract data
#'   within a window. This is accomplished by using a radius instead of a true
#'   window. These are wrappers for the \code{windowExtract()} function that
#'   does the heavy lifting.
#'
#' @param lon,lat Input locations that you are extracting from.
#' @param ras raster* object that the data is being pulled from
#' @param rasData data.frame object that contains the correct headers ('x', 'y',
#'   (valcolumns)). Will need to seperate out this function and create a class
#'   for it.
#' @param radius numeric. radius used to define extractWindow
#'
#' @details \code{radiusExtractXY()} extracts data directly from a raster using
#'   lat long coordinates.
#'
#'   \code{radiusExtractData()} is an optomized version that works better if the
#'   raster you are extracting from never changes and you are using it for
#'   multiple points. It uses an input data.frame of the raster values that you
#'   can create once and then extracts data by cellID.
#'
#' @return Returns a data.frame with the raster values extracted at the current
#'   radius.
#'
#' @section Creation notes: First created on 2019-Mar-6 in the Lab4inR.R and
#'   select.window.R scripts in my IBM class folder of the purdueResearch git.
#'   They were developed using base code found at
#'   http://rfunctions.blogspot.com/2017/08/extracting-data-from-rasters-using.html?view=classic.
#'    It uses another function \code{windowExtract()} to do the main work.
#'
#'
#' @examples
#'
#'
#' @rdname radiusExtractXY
#' @export
radiusExtractXY <- function(lon, lat, ras, radius=1) { #for a raster that you dont have a table created or that has changed or updated (this one is more general)
  # create reference data frame (need to make this section its own exported function and define a class for it)
  coor <- coordinates(ras)
  data <- data.frame(x = coor[, 1], y = coor[, 2], val = values(ras))

  # check for lat longs out of bounds #if raster gives coordinates from the middle of the cell this may need to be improved to use the entire extent of raster
  if (min(data$x) > lon | max(data$x) < lon |
      min(data$y) > lat | max(data$y) < lat) stop("Given lat long out of bounds")

  # identify the current cell values from long lat data
  xCell <- coor[, 1][which(abs(coor[, 1] - lon) %in% min(abs(coor[, 1] - lon)))[1]]
  yCell <- coor[, 2][which(abs(coor[, 2] - lat) %in% min(abs(coor[, 2] - lat)))[1]]

  return(windowExtract(xf = xCell, yf = yCell, radius = radius, xydata = data))
}

#' @rdname radiusExtractXY
#' @export
radiusExtractData <- function(cellID, rasData, radius=1) { #for a quicker calculations that you can keep an indexed raster as a data frame
  xCell <- rasData$x[rasData$cellID == cellID]
  yCell <- rasData$y[rasData$cellID == cellID]

  # check for invalid cellIDs
  if (length(xCell) == 0 | length(yCell) == 0 | is.null(cellID)) stop("Invalid cellID")

  return(windowExtract(xf = xCell, yf = yCell, radius = radius, xydata = rasData))
}




#' @title Extract raster values in a window
#'
#' @description Extracts rasters using a defined radius from a cell location
#'
#' @param xf,yf Input cellIDs that you are extracting from.
#' @param radius numeric. radius used to define extractWindow
#' @param xydata data.frame object that contains the correct headers ('x', 'y',
#'   (valcolumns)). Will need to seperate out this function and create a class
#'   for it.
#'
#' @return Returns a data.frame with the raster values extracted at the current
#'   radius.
#'
#' @section Creation notes: First created on 2019-Mar-6 in the Lab4inR.R and
#'   select.window.R scripts in my IBM class folder of the purdueResearch git.
#'   They were developed using base code from \code{CommEcol::select.window()}.
windowExtract <- function(xf, yf, radius = 1, xydata){

  x <- xydata$x
  y <- xydata$y

  sele <- which(x >= (xf - radius) & x <= (xf + radius) &  y >= (yf - radius) & y <= (yf + radius) )
  n<-length(sele)

  if (n == 1) {
    selected <- xydata[sele, ]
    resu <- as.data.frame(selected)
  }

  if(n > 1) {
    f <- which(x == xf & y == yf) ##focal cell
    sele.s <- sele[sele != f]   ##remove focal cell
    resu0 <- rbind(xydata[f, ], xydata[sele.s, ] ) #focal cell in the first row.
    resu <- as.data.frame(resu0)
  }

  return(resu)
}










