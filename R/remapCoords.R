remapCoords <- function(dataInfo, callDims, dataCoordList,  urlbase, xlen = 0., ylen = 0.) {
  # Logic here
  #  make copies of given coordinates that can be changed if needed without changing original values
  xlen_max <- max(xlen)/2
  ylen_max <- max(ylen)/2
  xcoord1 <- unlist(callDims[1])
  ycoord1 <- unlist(callDims[2])
  zcoord1 <- unlist(callDims[3])
  tcoord1 <- callDims[4]
  tcoord1 <- tcoord1[[1]]
  cross_dateline_180 = FALSE

  # if the xcoord is longitude, map to longitude range of ERDDAP dataset
  if ('longitude' %in% names(callDims)) {
    lonVal <- dataInfo$alldata$longitude[dataInfo$alldata$longitude$attribute_name
                                         == "actual_range", "value"]
    lonVal2 <- as.numeric(strtrim1(strsplit(lonVal, ",")[[1]]))
    #grid is -180, 180
    if (min(lonVal2) < 0.) {
        # check to see if the request crosses dateline on
        # (180, 180) dataset.  If so flag and do nothing
        temp_coord1 <- min(xcoord1) - xlen_max
        temp_coord2 <- max(xcoord1) + xlen_max
        if ((temp_coord1 < 180.) && (temp_coord2 > 180.)) {
          cross_dateline_180 = TRUE
        }
        #  else put the request on (-180,  180)
        else {
          xcoord1 <- make180(xcoord1)
          }
    }
    if (max(lonVal2) > 180.) {xcoord1 <- make360(xcoord1)}
  }

   return(list(xcoord1 = xcoord1, ycoord1 = ycoord1, zcoord1 = zcoord1,
              tcoord1 = tcoord1, dataInfo1 = dataInfo, cross_dateline_180 = cross_dateline_180))

}
