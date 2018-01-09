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

findERDDAPcoord <- function(dataCoordList, isotime, udtime, xcoordLim, ycoordLim,
                            tcoordLim, zcoordLim, xName, yName, tName, zName) {
  newxIndex <-  rep(NA_integer_, 2)
  newyIndex <-  rep(NA_integer_, 2)
  newTimeIndex <-  rep(NA_integer_, 2)
  newzIndex <- NA_integer_
  erddapXcoord <- rep(NA_real_, 2)
  erddapYcoord <- rep(NA_real_, 2)
  erddapTcoord <- rep(NA, 2)
  erddapZcoord <- rep(NA_real_, 2)
  if (xName %in% names(dataCoordList)) {
    newxIndex[1] <- which.min(abs(dataCoordList[[xName]] - xcoordLim[1]))
    newxIndex[2] <- which.min(abs(dataCoordList[[xName]] - xcoordLim[2]))
    erddapXcoord[1] <- dataCoordList[[xName]][newxIndex[1]]
    erddapXcoord[2] <- dataCoordList[[xName]][newxIndex[2]]
  }
  if (yName %in% names(dataCoordList)) {
    newyIndex[1] <- which.min(abs(dataCoordList[[yName]] - ycoordLim[1]))
    newyIndex[2] <- which.min(abs(dataCoordList[[yName]] - ycoordLim[2]))
    erddapYcoord[1] <- dataCoordList[[yName]][newyIndex[1]]
    erddapYcoord[2] <- dataCoordList[[yName]][newyIndex[2]]

  }
  if (tName %in% names(dataCoordList)) {
    newTimeIndex[1] <- which.min(abs(udtime - tcoordLim[1]))
    newTimeIndex[2] <- which.min(abs(udtime - tcoordLim[2]))
    erddapTcoord[1] <- as.character(isotime[newTimeIndex[1]])
    erddapTcoord[2] <- as.character(isotime[newTimeIndex[2]])
  }

  if (zName %in% names(dataCoordList)) {
    newzIndex[1] <- which.min(abs(dataCoordList[[zName]] - zcoordLim[1]))
    newzIndex[2] <- which.min(abs(dataCoordList[[zName]] - zcoordLim[2]))
    erddapZcoord[1] <- dataCoordList[[zName]][newzIndex[1]]
    erddapZcoord[2] <- dataCoordList[[zName]][newzIndex[2]]
  }

  erddapCoords <- list(erddapXcoord = erddapXcoord, erddapYcoord = erddapYcoord,
                        erddapTcoord = erddapTcoord, erddapZcoord = erddapZcoord)
  newIndex <- list(newxIndex = newxIndex, newyIndex = newyIndex,
                   newTimeIndex = newTimeIndex, newzIndex = newzIndex)
  return(list(erddapCoords = erddapCoords, newIndex = newIndex))
}


makeCmd <- function(dataInfo, urlbase, xName, yName, zName, tName, parameter,
                    erddapXcoord, erddapYcoord, erddapTcoord, erddapZcoord,
                    verbose ) {

  myCallOpts <- list(dataInfo)
  myCallOptsNames <- list('x')
  if (!(urlbase == "https://upwell.pfeg.noaa.gov/erddap/")) {
    myCallOpts$url <- urlbase
    myCallOptsNames <- c(myCallOptsNames, 'url')
  }
  if (verbose) {
    myCallOpts$callopts <- httr::verbose()
    myCallOptsNames <- c(myCallOptsNames, 'callopts')
  }
  if (!is.na(erddapXcoord[1])) {
    myCallOpts$xName <- erddapXcoord
    myCallOptsNames <- c(myCallOptsNames, xName)
  }
    if (!is.na(erddapYcoord[1])) {
    myCallOpts$yName <- erddapYcoord
    myCallOptsNames <- c(myCallOptsNames, yName)
    }
  if (!is.na(erddapZcoord[1])) {
    myCallOpts$zName <- erddapZcoord
    myCallOptsNames <- c(myCallOptsNames, zName)
  }
  if (!is.na(erddapTcoord[1])) {
    myCallOpts$tName <- erddapTcoord
    myCallOptsNames <- c(myCallOptsNames, tName)
  }
  myCallOpts$fields = parameter
  myCallOptsNames <- c(myCallOptsNames, 'fields')
  myCallOpts$read <- FALSE
  myCallOptsNames <- c(myCallOptsNames, 'read')

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



