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
    if (names(dimargs)[cIndex] == 'time') {
      min_dimargs <- min(as.numeric(dimargs[[cIndex]]))
      max_dimargs <- max(as.numeric(dimargs[[cIndex]]))
      temp_time <- parsedate::parse_iso_8601(dataCoordList[[i]])
      min_coord <- min( as.numeric(temp_time))
      max_coord <- max( as.numeric(temp_time))
    } else {
      min_dimargs <- min(dimargs[[cIndex]])
      max_dimargs <- max(dimargs[[cIndex]])
      min_coord <- min(dimargs[[cIndex]])
      max_coord <- max(dimargs[[cIndex]])
    }
    if ((min_dimargs < min_coord) |  (max_dimargs > max_coord)) {
      if (names(dimargs)[cIndex] == 'time') {
        print('dimension coordinate out of bounds')
        print(paste0('dimension name: ', names(dimargs)[cIndex]))
        print(paste('given coordinate bounds', min_dimargs, max_dimargs))
        returnCode <- 1
        print(paste('ERDDAP datasets bounds',  as.Date(min_coord), as.Date( max_coord)))
      } else {
        print('dimension coordinate out of bounds')
        print(paste0('dimension name: ', names(dimargs)[cIndex]))
        print(paste('given coordinate bounds', min_dimargs, max_dimargs))
        returnCode <- 1
        print(paste('ERDDAP datasets bounds',  min_coord,  max_coord))
        }
    }
  }



  if (returnCode != 0) {
      stop("Coordinates out of dataset bounds - see messages above")
  }

  # return(returnCode)
}
