checkInput <- function(dataInfo, parameter, urlbase, callDims) {

  #  check that a valid rerddap info structure is being passed
  if (!(methods::is(dataInfo, "info"))) {
    print("error - dataInfo is not a valid info structure from rerddap")
    return(-999)
  }

  #  check that the dataset is a grid
  if (!("Grid" %in% dataInfo$alldata$NC_GLOBAL$value)) {
    print("error - dataset is not a Grid")
    return(-999)
  }


  # check that there are the correct number of coordinates given
  # and the correct coordinates  by
  # 1) checking that given corrdinates are in dataset
  # 2) the correct number of coordinates are given

  allvars <- getallvars(dataInfo)
  allCoords <- dimvars(dataInfo)
  callDims <- callDims[!vapply(callDims, is.null, logical(1))]

  # test coordinate names
  namesTest <- names(callDims) %in% allCoords
  if (any(!namesTest, na.rm = TRUE)) {
    print('Requested coordinate names do no match dataset coordinate names')
    print(paste('Requested coordinate names:', names(callDims)))
    print(paste('Dataset coordinate names:', allCoords))
    return(-999)
  }
  if (length(callDims) != length(allCoords)) {
    print("Ranges not given for all of the dataset dimensions")
    print("Coordinates given: ")
    print(names(callDims))
    print("Dataset Coordinates: ")
    print(allCoords)
    print("Execution halted")
    return(-999)
  }

  # check that the field given part of the dataset
  if (!(parameter %in% allvars)) {
    cat("Parameter given is not in dataset")
    cat("Parameter given: ", parameter)
    cat("Dataset Parameters: ",
        allvars[(length(allCoords) + 1):length(allvars)])
    print("Execution halted")
    return(-999)
  }
  #  check that the base url ends in /
  lenURL <- nchar(urlbase)
  if (substr(urlbase, lenURL, lenURL) != '/') {
    urlbase <- paste0(urlbase, '/')
  }

  # check that urlbase connects to an ERDDAP™
  myHTTP <- try(rerddap::version(urlbase), silent = TRUE)
  if (inherits(myHTTP, 'try-error') || substr(myHTTP, 1, 6) != 'ERDDAP') {
    print('failed to connect to given ERDDAP(TM), program will stop')
    return(-1000)
  }

  return(urlbase)
}
