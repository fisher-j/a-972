#' Calculate XY coordinates based on distance and bearing
#'
#' @description Function converts distance and bearing into XY coordinates.
#' It is useful to generate stem maps (forestry) based on field data
#' (in forestry usually distance and bearing to a tree from a plot center are
#' collected to define tree locations).
#' Optionally center coordinates of the plot may be provided.
#'
#' @param distance a vector of distances.
#' @param bearing a vector of bearings, in degrees (0 - 360).
#' @param center.x An x coordinate of a plot center
#' @param center.y An y coordinate of a plot center
#' @return a data frame with x and y columns
#' @examples
#' d <- runif(10,min = 0.5,max=10)
#' b <- runif(10,min = 0,max=360)
#' plot(circular2xy(d,b))
#' plot(circular2xy(d,b,1000,2500))
#' @export


circular2xy <- function(distance, bearing, center.x = 0, center.y = 0) {

  #check inputs
  if(!is.numeric(distance) | !is.numeric(bearing)) stop("Input data not numeric")
  if(any(bearing > 360)) stop("bearing value(s) exceed 360")

  bearing.rad <- bearing * (pi / 180) #convert bearing to radians
  delta.x <- distance * sin(bearing.rad)
  delta.y <- distance * cos(bearing.rad)
  tree.x <- center.x + delta.x
  tree.y <- center.y + delta.y
  return(data.frame(x=tree.x,y=tree.y))
}
