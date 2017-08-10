checkInput <- function(dataInfo, parameter, urlbase, callDims) {

  #  check that a valid rerddap info structure is being passed
  if (!(methods::is(dataInfo, "info"))) {
    print("error - dataInfo is not a valid info structure from rerddap")
    return()
  }

  #  check that the dataset is a grid
  if (!("Grid" %in% dataInfo$alldata$NC_GLOBAL$value)) {
    print("error - dataset is not a Grid")
    return()
  }


  # check that there are the correct number of coordinates given
  # and the correct coordinates  by
  # 1) checking that given corrdinates are in dataset
  # 2) the correct number of coordinates are given

  allvars <- getallvars(dataInfo)
  # numparms <- dim(dataInfo$variables)[1]
  allCoords <- dimvars(dataInfo)
  callDims <- callDims[!sapply(callDims,is.null)]

  # test coordinate names
  namesTest <- names(callDims) %in% allCoords
  if (any(namesTest == FALSE, na.rm = TRUE)) {
    print('Requested coordinate names do no match dataset coordinate names')
    print(paste('Requested coordinate names:', names(callDims)))
    print(paste('Dataset coordinate names:', allCoords))
    stop(sprintf("Execution halted"), call. = FALSE)
  }
  if (!(length(callDims) == length(allCoords))) {
    print("Ranges not given for all of the dataset dimensions")
    print("Coordinates given: ")
    print(names(callDims))
    print("Dataset Coordinates: ")
    print(allCoords)
    stop(sprintf("Execution halted"), call. = FALSE)
  }

  # check that the field given part of the dataset
  if (!(parameter %in% allvars)) {
    cat("Parameter given is not in dataset")
    cat("Parameter given: ", parameter)
    cat("Dataset Parameters: ", allvars[(length(allCoords) + 1):length(allvars)])
    stop("execution halted", call. = FALSE)
  }
  #  check that the base url ends in /
  lenURL <- nchar(urlbase)
  if (substr(urlbase, lenURL, lenURL) == '/') {
    urlbase <- substr(urlbase, 1, (lenURL - 1))
  }

  # check that urlbase connects to an ERDDAP
  myHTTP <- httr::HEAD(urlbase)
  if (!(myHTTP$status_code == 200)) {
    stop("urlbase did not resolve to a valid server", call. = FALSE)
  }


  return(urlbase)
}
