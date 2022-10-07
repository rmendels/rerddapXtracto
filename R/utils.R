####
# All of these utilities are from the rerddap package
#####



getvar <- function(x, y){
  x$alldata$NC_GLOBAL[ x$alldata$NC_GLOBAL$attribute_name == y, "value"]
}

getvars <- function(x){
  vars <- names(x$alldata)
  vars[ !vars %in% c("NC_GLOBAL", "time", x$variables$variable_name) ]
}

getallvars <- function(x){
  vars <- names(x$alldata)
  vars[ !vars %in% "NC_GLOBAL" ]
}

dimvars <- function(x){
  vars <- names(x$alldata)
  vars[ !vars %in% c("NC_GLOBAL", x$variables$variable_name) ]
}


strextract1 <- function(str, pattern) regmatches(str, regexpr(pattern, str))

strtrim1 <- function(str) gsub("^\\s+|\\s+$", "", str)

###  End utilites from rerddap

findERDDAPcoord <- function(dataCoordList, isotime, udtime, xcoordLim,
                            ycoordLim, tcoordLim, zcoordLim,
                            xName, yName, tName, zName, cross_dateline_180) {

  # make ERDDAP call to actual closest coordinate
  # for rxtracto the indices of the call need to be stored
  # so that two successive locations less than grid size will not repeat same call
  newxIndex <-  rep(NA_integer_, 2)
  newyIndex <-  rep(NA_integer_, 2)
  newTimeIndex <-  rep(NA_integer_, 2)
  newzIndex <- NA_integer_
  erddapXcoord <- rep(NA_real_, 2)
  erddapYcoord <- rep(NA_real_, 2)
  erddapTcoord <- rep(NA, 2)
  erddapZcoord <- rep(NA_real_, 2)
  # if xcoord exists,  gets the closest coordinates,
  # and the value of those coordinates
  if (xName %in% names(dataCoordList)) {
    # if longitude crosses dateline have to be careful
    # request is in still in terms of (0, 360)
    # data is in terms of (-180,  180)
    temp_xlimit <- xcoordLim
    if (cross_dateline_180) {temp_xlimit <- make180(temp_xlimit)}
    newxIndex[1] <- which.min(abs(dataCoordList[[xName]] - temp_xlimit[1]))
    newxIndex[2] <- which.min(abs(dataCoordList[[xName]] - temp_xlimit[2]))
    erddapXcoord[1] <- dataCoordList[[xName]][newxIndex[1]]
    erddapXcoord[2] <- dataCoordList[[xName]][newxIndex[2]]
    # check for edge effects and rounding errors
    if (newxIndex[1]  == 1) {
      if (abs(dataCoordList[[xName]][newxIndex[1]] - xcoordLim[1]) < .0001) {
        erddapXcoord[1] <- xcoordLim[1]
      }
    }
    if (newxIndex[2]  == length(dataCoordList[[xName]])) {
      if (abs(dataCoordList[[xName]][newxIndex[2]] - xcoordLim[2]) < .0001) {
        erddapXcoord[2] <- xcoordLim[2]
      }
    }
  }
  # if ycoord exists,  gets thee closest coordinates,
  # and the value of those coordinates
  if (yName %in% names(dataCoordList)) {
    newyIndex[1] <- which.min(abs(dataCoordList[[yName]] - ycoordLim[1]))
    newyIndex[2] <- which.min(abs(dataCoordList[[yName]] - ycoordLim[2]))
    erddapYcoord[1] <- dataCoordList[[yName]][newyIndex[1]]
    erddapYcoord[2] <- dataCoordList[[yName]][newyIndex[2]]
    # check for edge effects and rounding errors
    if (newyIndex[1]  == 1) {
      if (abs(dataCoordList[[yName]][newyIndex[1]] - ycoordLim[1]) < .0001) {
        erddapYcoord[1] <- ycoordLim[1]
      }
    }
    if (newyIndex[2]  == length(dataCoordList[[yName]])) {
      if (abs(dataCoordList[[yName]][newyIndex[2]] - ycoordLim[2]) < .0001) {
        erddapYcoord[2] <- ycoordLim[2]
      }
    }
  }
  # if time is a coordinate,  gets the closest coordinates,
  # and the value of those coordinates
  if (tName %in% names(dataCoordList)) {
    newTimeIndex[1] <- which.min(abs(udtime - tcoordLim[1]))
    newTimeIndex[2] <- which.min(abs(udtime - tcoordLim[2]))
    erddapTcoord[1] <- as.character(isotime[newTimeIndex[1]])
    erddapTcoord[2] <- as.character(isotime[newTimeIndex[2]])
  }
  # if zcoord exists,  gets hte closest coordinates,
  # and the value of those coordinates
    if (zName %in% names(dataCoordList)) {
    newzIndex[1] <- which.min(abs(dataCoordList[[zName]] - zcoordLim[1]))
    newzIndex[2] <- which.min(abs(dataCoordList[[zName]] - zcoordLim[2]))
    erddapZcoord[1] <- dataCoordList[[zName]][newzIndex[1]]
    erddapZcoord[2] <- dataCoordList[[zName]][newzIndex[2]]
    # check for edge effects and rounding errors
    if (newzIndex[1]  == 1) {
      if (abs(dataCoordList[[zName]][newzIndex[1]] - zcoordLim[1]) < .0001) {
        erddapZcoord[1] <- zcoordLim[1]
      }
    }
    if (newzIndex[2]  == length(dataCoordList[[zName]])) {
      if (abs(dataCoordList[[zName]][newzIndex[2]] - zcoordLim[2]) < .0001) {
        erddapZcoord[2] <- zcoordLim[2]
      }
    }
    }
  # want to return the coordinate limits for the call
  # and the indices of these coordinates for rxtracto()
  erddapCoords <- list(erddapXcoord = erddapXcoord,
                       erddapYcoord = erddapYcoord,
                       erddapTcoord = erddapTcoord,
                       erddapZcoord = erddapZcoord)
  newIndex <- list(newxIndex = newxIndex, newyIndex = newyIndex,
                   newTimeIndex = newTimeIndex, newzIndex = newzIndex)
  return(list(erddapCoords = erddapCoords, newIndex = newIndex))
}


makeCmd <- function(dataInfo, urlbase, xName, yName, zName, tName, parameter,
                    erddapXcoord, erddapYcoord, erddapTcoord, erddapZcoord,
                    verbose) {
  # build up a list with the arguments for rerddap::griddap() using do.call()
  # rerddap needs first the results from calling info
  myCallOpts <- list(dataInfo)
  if (packageVersion('rerddap') < '1.0.0') {
    myCallOptsNames <- list('x')
  } else {
    myCallOptsNames <- list('datasetx')
  }
  myCallOptsNames <- list('datasetx')
  # if the url is not the default URL, add it to the list to be set
  if (!(urlbase == "https://upwell.pfeg.noaa.gov/erddap/")) {
    myCallOpts$url <- urlbase
    myCallOptsNames <- c(myCallOptsNames, 'url')
  }
  # if verbose output desired,  add to list
  if (verbose) {
    myCallOpts$callopts <- list(verbose = TRUE)
    myCallOptsNames <- c(myCallOptsNames, 'callopts')
  }
  # if xcoord exists,  add its limits
  if (!is.na(erddapXcoord[1])) {
    myCallOpts$xName <- erddapXcoord
    myCallOptsNames <- c(myCallOptsNames, xName)
  }
  # if ycoord exists,  add its limits
    if (!is.na(erddapYcoord[1])) {
    myCallOpts$yName <- erddapYcoord
    myCallOptsNames <- c(myCallOptsNames, yName)
    }
  # if zcoord exists,  add its limits
  if (!is.na(erddapZcoord[1])) {
    myCallOpts$zName <- erddapZcoord
    myCallOptsNames <- c(myCallOptsNames, zName)
  }
  # if time coordinate exists, add its limits
  if (!is.na(erddapTcoord[1])) {
    myCallOpts$tName <- erddapTcoord
    myCallOptsNames <- c(myCallOptsNames, tName)
  }
  # add the parameter name to the "fields" argument
  myCallOpts$fields <- parameter
  myCallOptsNames <- c(myCallOptsNames, 'fields')
  # tell rerddap::griddap() not to read in and melt data
  myCallOpts$read <- FALSE
  myCallOptsNames <- c(myCallOptsNames, 'read')
  # set the names of the fields added to the list
  names(myCallOpts) <- myCallOptsNames

  return(myCallOpts)
}


removeLast <- function(isotime, tcoord1) {
  # looks for "last" in the time bound
  # calculates actual date, including arithmetic on "last"
  # calculates the bounds as R dates
  lenTime <- length(isotime)

  if (grepl("last", tcoord1[1])) {
    tlen <- nchar(tcoord1[1])
    arith <- substr(tcoord1[1], 5, tlen)
    tempVar <- paste0(as.character(lenTime), arith)
    tIndex <- eval(parse(text = tempVar))
    tcoord1[1] <- isotime[tIndex]
  }

  if (grepl("last", tcoord1[2])) {
    tlen <- nchar(tcoord1[2])
    arith <- substr(tcoord1[2], 5, tlen)
    tempVar <- paste0(as.character(lenTime), arith)
    tIndex <- eval(parse(text = tempVar))
    tcoord1[2] <- isotime[tIndex]
  }
  #udtpos <- parsedate::parse_date(tcoord1)
  #tcoordLim <- c(min(udtpos), max(udtpos))
  return(tcoord1)
}



