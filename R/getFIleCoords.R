#
# Get Coordinate  (Dimension) Data from ERDDAP Dataset
#
#  'getFileCoords()' is an internal function that gets the coordinate
# (dimension) variables of the requested dataset
#
# dataStruct - A structure describing the dataset from erddapStruct.rda
# urlbase  - A character string giving the base URL of the ERDDAP server
# return - A list containing the values of the coordinate variables
#



getfileCoords <- function(datasetID, dataCoords, urlbase) {
# get actual coordinate values from ERDDAP as csv
# skip headers
  coordList <- list()
  for (i in seq_len(length(dataCoords))) {
#    myURL <- paste0(urlbase, 'griddap/', datasetID, '.csvp?',
#                    dataCoords[i], '[0:1:last]')
    myURL <- paste0(urlbase, 'griddap/', datasetID, '.csvp?', dataCoords[i])
    #    coordVals <- utils::read.csv(myURL, skip = 2, header = FALSE,
#    stringsAsFactors = FALSE)
#    coordVals <- coordVals[, 1]
#    coordList[[i]] <- coordVals

    numtries <- 10
    tryn <- 0
    goodtry <- -1
    while ((tryn <= numtries) & (goodtry == -1)) {
      tryn <- tryn + 1
      r1 <- try( httr::GET(myURL), silent = TRUE)
      if (!class(r1)[1] == "try-error") {
        goodtry <- 1
      } else{
        Sys.sleep(tryn * 0.5)
      }
    }
    # if (class(r1) == "try-error") {
    if (goodtry == -1) {
      print('error in trying to retrieve the dataset coordinate variables')
      print(paste('failed on dimension ', dataCoords[i] ))
      stop('check on the ERDDAP server that the dataset is active')
    }
    if (dataCoords[i] == "time" ) {
      coordVals <- suppressMessages(readr::read_csv(r1$content,
         col_types = readr::cols(.default  = readr::col_character()  ))[[1]])

    } else {
      coordVals <- suppressMessages(readr::read_csv(r1$content)[[1]])
    }
    coordList[[i]] <- coordVals
  }
# add names based on names in function call
  names(coordList) <- dataCoords
  return(coordList)
} #function getfileinfo
