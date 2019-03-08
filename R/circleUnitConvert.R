#' Convert Degrees to Radians and Vice Versa
#'
#' @description Base R doesn't have a native function to change degrees to but
#'   requires radians for trig functions. These are to help handle that.
#'
#' @param deg Input degree you want to change to radians.
#' @param rad Input radian you want to change to degrees.
#'
#'
#' @section Creation notes: First created in 2019-Feb in FIAspatialAdditions.R
#'   for my BigDataFNR class presentation on working with spatial data in R in
#'   my purdueResearch repo.
#'
#' @examples
#' @rdname deg2rad
#'

deg2rad <- function(deg) {
  (deg * pi) / (180)
  }

#' @rdname deg2rad
rad2deg <- function(rad) {
  (rad * 180) / (pi)
}
