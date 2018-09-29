remapCoords <- function(dataInfo, callDims, dataCoordList,  urlbase) {
  # Logic here
  #  make copies of given coordinates that can be changed if needed without changing original values
  xcoord1 <- unlist(callDims[1])
  ycoord1 <- unlist(callDims[2])
  zcoord1 <- unlist(callDims[3])
  tcoord1 <- callDims[4]
  tcoord1 <- tcoord1[[1]]

  # if the xcoord is longitude, map to longitude range of ERDDAP dataset
  if ('longitude' %in% names(callDims)) {
    lonVal <- dataInfo$alldata$longitude[dataInfo$alldata$longitude$attribute_name
                                         == "actual_range", "value"]
    lonVal2 <- as.numeric(strtrim1(strsplit(lonVal, ",")[[1]]))
    #grid is -180, 180
    if (min(lonVal2) < 0.) {xcoord1 <- make180(xcoord1)}
    if (max(lonVal2) > 180.) {xcoord1 <- make360(xcoord1)}
  }

   return(list(xcoord1 = xcoord1, ycoord1 = ycoord1, zcoord1 = zcoord1,
              tcoord1 = tcoord1, dataInfo1 = dataInfo))

}
