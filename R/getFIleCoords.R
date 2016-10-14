#'
#' Get Cooordinate  (Dimension) Data from ERDDAP Dataset
#'
#' @keywords internal
#' \code{getFileCoords} is an internal function that gets the coordinate
#' (dimension) variables of the requested dataset
#'
#' @param dataStruct A structure describing the dataset from erddapStruct.rda
#' @param urlbase A character string giving the base URL of the ERDDAP server
#' @return A list containing the values of the coordinate variables
#'



getfileCoords <- function(datasetID, dataCoords, urlbase) {
# to start do brute force way with for loop
  coordList <- list()
  for (i in 1:length(dataCoords)) {
    myURL <- paste0(urlbase, '/griddap/', datasetID, '.csv?', dataCoords[i], '[0:1:last]')
    coordVals <- utils::read.csv(myURL, skip=2, header=FALSE, stringsAsFactors=FALSE)
    coordVals <- coordVals[, 1]
    coordList[[i]] <- coordVals
  }
  names(coordList) <- dataCoords
  return(coordList)
} #function getfileinfo
