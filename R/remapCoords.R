remapCoords <- function(dataInfo, callDims, is3D, urlbase) {
  #
  # make requested longtiudes be on same limits as data longitudes (0,360) or
  # (-180, 180)
  xcoord1 <- unlist(callDims[1])
  if ('longitude' %in% names(callDims)) {
    lonVal <- dataInfo$alldata$longitude[dataInfo$alldata$longitude$attribute_name == "actual_range", "value"]
    lonVal2 <- as.numeric(strtrim1(strsplit(lonVal, ",")[[1]]))
    #grid is -180, 180
    if (min(lonVal2) < 0.) {xcoord1 <- make180(xcoord1)}
    if (max(lonVal2) > 180.) {xcoord1 <- make360(xcoord1)}
  }


  ### now that corrdiantes are okay, let's make copies so we can manipulate if needed
  ycoord1 <- unlist(callDims[2])
  zcoord1 <- unlist(callDims[3])
  tcoord1 <- callDims[4]
  tcoord1 <- tcoord1[[1]]


  ### If ycoord is latitude, check for dataset going north-south, convert request
   ycoordLim <- c(min(ycoord1), max(ycoord1))
   if ('latitude' %in% names(callDims)) {
    if (is3D){
      latVal <- dataInfo$alldata$latitude[dataInfo$alldata$latitude$attribute_name == "actual_range", "value"]
      latVal2 <- as.numeric(strtrim1(strsplit(latVal, ",")[[1]]))
      #north-south  datasets
      if (latVal2[1] > latVal2[2]) {ycoordLim <- c(max(ycoord1), min(ycoord1))}
    }
  }

  xcoordLim <- c(min(xcoord1), max(xcoord1))
  zcoordLim <- NULL
  if (!is.null(zcoord1)) {
    zcoordLim <- c(zcoord1, zcoord1)
  }

  # get the actual coordinate values for the dataset
  allCoords <- dimvars(dataInfo)
  dataCoordList <- getfileCoords(attr(dataInfo, "datasetid"), allCoords, urlbase)
  if (length(dataCoordList) == 0) {
    stop("Error retrieving coordinate variable")
  }

  # if time is a coordinate, deal with "last" as a limit, convert to R time
  tcoordLim <- NULL
  newTime <- NULL
  if (!is.null(tcoord1)) {
    newTime <- timeClean(dataCoordList$time, tcoord1)
    tcoordLim <- newTime$tcoordLim
    tcoord1 <- newTime$tcoord1
  } else {
    newTime <- list(isotime = NULL, udtime = NULL, udtpos = NULL, tcoord1 = NULL, tcoordLim = NULL)
}
  return(list(xcoord1 = xcoord1, ycoord1 = ycoord1, zcoord1 = zcoord1, tcoord1 = tcoord1, xcoordLim = xcoordLim, ycoordLim = ycoordLim, zcoordLim = zcoordLim,  tcoordLim = tcoordLim, newTime = newTime, dataCoordList = dataCoordList))
}
