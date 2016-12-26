#' Check Coordinate Dimensions
#'
#' @keywords internal
#' \code{checkBounds} is an internal function that checks that the requested
#' latitude, longitude, time bounds are within th dataset bounds
#'
#' @param dataStruct A structure describing the dataset from erddapStruct.rda
#' @param xposLim A list of reals size 2 that contains the longitude limits of
#'   the data request
#' @param yposLim A list of reals size 2 that contains the latitude limits of
#'   the data request
#' @param xposLim A list of strings  size 2 that contains the time limits of the
#'   data request
#' @return returnCode of 0 if all data is in bounds of the served data, -1
#'   otherwise
#'

checkBounds <- function(dataCoordList, dimargs) {
  returnCode <- 0
  dimLen <- length(names(dataCoordList))
  for (i in (1:dimLen)) {
    cIndex <- which(names(dataCoordList)[i] == names(dimargs))
    if ((min(dimargs[[cIndex]]) < min(dataCoordList[[i]])) |  (max(dimargs[[cIndex]]) > max(dataCoordList[[i]]))) {
      print('dimension coordinate out of bounds')
      print(paste0('dimension name: ', names(dimargs)[cIndex]))
      print(paste('given coordinate bounds', dimargs[cIndex]))
      returnCode <- 1
      print(paste('ERDDAP datasets bounds',  min(dataCoordList[i]),  max(dataCoordList[i])))
    }

  }



  if (returnCode != 0) {
      stop("Coordinates out of dataset bounds - see messages above")
  }

  # return(returnCode)
}
