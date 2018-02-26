#'
#' Get Coordinate  (Dimension) Data from ERDDAP Dataset
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
# get actual coordinate values from ERDDAP as csv
# skip headers
  coordList <- list()
  for (i in seq_len(length(dataCoords))) {
    myURL <- paste0(urlbase, 'griddap/', datasetID, '.csvp?',
                    dataCoords[i], '[0:1:last]')
#    coordVals <- utils::read.csv(myURL, skip = 2, header = FALSE, stringsAsFactors = FALSE)
#    coordVals <- coordVals[, 1]
#    coordList[[i]] <- coordVals

    r1 <- httr::GET(myURL)
    if (dataCoords[i] == "time" ) {
      coordVals <- suppressMessages(readr::read_csv(r1$content, col_types = readr::cols(.default  = readr::col_character()  ))[[1]])

    } else {
      coordVals <- suppressMessages(readr::read_csv(r1$content)[[1]])
    }
    coordList[[i]] <- coordVals
  }
# add names based on names in function call
  names(coordList) <- dataCoords
  return(coordList)
} #function getfileinfo
