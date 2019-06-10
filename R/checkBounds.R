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

checkBounds <- function(dataCoordList, dimargs, cross_dateline_180) {
  returnCode <- 0
  dimLen <- length(names(dataCoordList))
  for (i in seq_len(dimLen)) {
    # find which dimension we are dealing with
    cIndex <- which(names(dataCoordList)[i] == names(dimargs))
    # time has to be treated differently because it is character string
    if (names(dimargs)[cIndex] == 'time') {
      # get user requested bounds
      min_dimargs <- min(as.numeric(dimargs[[cIndex]]))
      max_dimargs <- max(as.numeric(dimargs[[cIndex]]))
      # get actual dataset bounds by converting to number
      temp_time <- parsedate::parse_iso_8601(dataCoordList[[i]])
      min_coord <- min( as.numeric(temp_time))
      max_coord <- max( as.numeric(temp_time))
    } else {
      # get user requested bounds
      min_dimargs <- min(dimargs[[cIndex]])
      max_dimargs <- max(dimargs[[cIndex]])
      # get actual dataset bounds
      min_coord <- min(dataCoordList[[i]])
      max_coord <- max(dataCoordList[[i]])
    }
    # if cross_dateline_180 = TRUE will not check longitude bound
    # for the nonce.  Too many issues.
    if (!(names(dimargs)[cIndex] == 'longitude')) {
      if ((min_dimargs < min_coord) |  (max_dimargs > max_coord)) {
        # time is treated differently because it is printed out differently
        if (names(dimargs)[cIndex] == 'time') {
          print('dimension coordinate out of bounds')
          print(paste0('dimension name: ', names(dimargs)[cIndex]))
          print(paste('given coordinate bounds',min(dimargs[[cIndex]]), max(dimargs[[cIndex]])))
          returnCode <- 1
          print(paste('ERDDAP datasets bounds',  min(temp_time), max(temp_time)))
        } else {
          print('dimension coordinate out of bounds')
          print(paste0('dimension name: ', names(dimargs)[cIndex]))
          print(paste('given coordinate bounds', min_dimargs, max_dimargs))
          returnCode <- 1
          print(paste('ERDDAP datasets bounds',  min_coord,  max_coord))
        }
      }
    }
  }



  if (returnCode != 0) {
      stop("Coordinates out of dataset bounds - see messages above")
  }

  # return(returnCode)
}
