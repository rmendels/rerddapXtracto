#'
#' \code{rxtracto_3D} uses the R program rerddap to extract environmental data
#' from an ERDDAP server in an (x,y,z, time) bounding box.
#' The same call could be made directly in rerddap,
#' but function is maintained as it is used in the polygon routine.
#' @export
#' @param dataInfo - the return from an rerddap "info" call to an ERDDAP server
#' @param parameter - character string containing the name of the parameter to extract
#' @param xcoord - a real array with the x-coordinates of the trajectory (if longitude in #'   decimal degrees East, either 0-360 or -180 to 180)
#' @param ycoord -  a real array with the y-coordinate of the trajectory (if latitude in
#'   decimal degrees N; -90 to 90)
#' @param zcoord -  a real number with the z-coordinate (usually altitude or depth)
#' @param tcoord - a character array with the times of the trajectory in
#'   "YYYY-MM-DD" - for now restricted to be time.
#' @param xName - character string with name of the xcoord in the ERDDAP dataset (default "longitude")
#' @param yName - character string with name of the ycoord in the ERDDAP dataset (default "latitude")
#' @param zName - character string with name of the zcoord in the ERDDAP dataset (default "altitude")
#' @param tName - character string with name of the tcoord in the ERDDAP dataset (default "time")
#' @param urlbase - base URL of the ERDDAP server being accessed - default "http://upwell.pfeg.noaa.gov/erddap"
#' @param verbose - logical variable (default FALSE) if the the URL request should be verbose
#' @return structure with data and dimensions:
#' \itemize{
#'   \item extract$data - the data array dimensioned (lon,lat,time)
#'   \item extract$varname - the name of the parameter extracted
#'   \item extract$datasetname - ERDDAP dataset name
#'   \item extract$longitude - the longitudes on some scale as request
#'   \item extract$latitude - the latitudes always going south to north
#'   \item extract$time - the times of the extracts
#'   }
#' @examples
#' urlbase <- 'http://upwell.pfeg.noaa.gov/erddap'
#' dataInfo <- rerddap::info('erdPHssta8day')
#' parameter <- 'sst'
#' xcoord <- c(230,235)
#' ycoord <- c(40,45)
#' tcoord <- c('2006-01-15','2006-01-20')
#' zcoord <- 0.
#' extract <- rxtracto_3D(dataInfo, parameter, xcoord=xcoord, ycoord=ycoord, tcoord=tcoord, zcoord=zcoord)
#' 2-D example getting bathymetry
#' dataInfo <- rerddap::info('etopo360')
#' parameter <- 'altitude'
#' extract <- xtracto_3D(dataInfo, parameter, xcoord=xcoord, ycoord=ycoord)
#' Example where grid is not latitude-longitude
#' dataInfo <- rerddap::info('glos_tds_5912_ca66_3f41')
#' parameter = 'temp'
#' xName <- 'nx'
#' yName <- 'ny'
#' zName <- 'nsigma'
#' xcoord <- c(10,11)
#' ycoord <- c(10,11)
#' zcoord <- c(1,1)
#' tcoord <- c('2016-09-02','2016-09-03')
#' extract <- rxtracto_3D(dataInfo, parameter, xcoord=xcoord, ycoord=ycoord, zcoord=zcoord, tcoord=tcoord, xName=xName, yName=yName, zName=zName)
#' Dataset that has depth also
#' 3 months of subsurface temperature at 70m depth from SODA 2.2.4
#' dataInfo <- rerddap::info('hawaii_d90f_20ee_c4cb')
#' parameter = 'temp'
#' xName <- 'longitude'
#' yName <- 'latitude'
#' zName <- 'depth'
#' xcoord <- c(230.25,250.25)
#' ycoord <- c(30.25,43.25)
#' zcoord <- c(70.02,70.02)
#' tcoord <- c('2010-10-15','2010-12-15')
#' extract <- rxtracto_3D(dataInfo, parameter, xcoord=xcoord, ycoord=ycoord, zcoord=zcoord, tcoord=tcoord, xName=xName, yName=yName, zName=zName)
#'

rxtracto_3D <- function(dataInfo, parameter = NULL, xcoord=NULL, ycoord=NULL, zcoord = NULL, tcoord=NULL, xName='longitude', yName='latitude', zName='altitude', tName='time', urlbase='http://upwell.pfeg.noaa.gov/erddap', verbose=FALSE) {

  #  check that a valid rerddap info structure is being passed
  if (!(is(dataInfo, "info"))) {
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
  numparms <- dim(dataInfo$variables)[1]
  allCoords <- dimvars(dataInfo)
  callDims <- list(xcoord, ycoord, zcoord, tcoord)
  names(callDims) <- c(xName, yName, zName, tName)
  callDims <- callDims[!sapply(callDims,is.null)]
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
    cat("Dataset Parameters: ", allvars[(length(allCoords)+1):length(allvars)])
    stop("execution halted", call. = FALSE)
  }

  #reconcile longitude grids
  #get dataset longitude range
  xcoord1 <- xcoord
  if (xName == 'longitude'){
    lonVal <- dataInfo$alldata$longitude[dataInfo$alldata$longitude$attribute_name == "actual_range", "value"]
    lonVal2 <- as.numeric(strtrim1(strsplit(lonVal, ",")[[1]]))
    #grid is -180, 180
    if (min(lonVal2) < 0.) {xcoord1 <- make180(xcoord1)}
    if (max(lonVal2) > 180.) {xcoord1 <- make360(xcoord1)}
  }


### now that corrdiantes are okay, let's make copies so we can manipulate if needed
  ycoord1 <- ycoord
  zcoord1 <- zcoord
  tcoord1 <- tcoord


### If ycoord is latitude, check for dataset going north-south, convert request
  if (yName == 'latitude') {
    latVal <- dataInfo$alldata$latitude[dataInfo$alldata$latitude$attribute_name == "actual_range", "value"]
    latVal2 <- as.numeric(strtrim1(strsplit(latVal, ",")[[1]]))
    #north-south  datasets
    if(latVal2[1] > latVal2[2]) {ycoord1<-rev(ycoord1)}
  }

  ### If tcoord is not empty, check for value "last" and convert, and make time an R date
   xcoordLim <- c(min(xcoord1), max(xcoord1))
  ycoordLim <- c(min(ycoord1), max(ycoord1))
  if (!is.null(zcoord)) {
    zcoordLim <- c(zcoord, zcoord)
  }
#  dimargs <- dimargs[dimargs != "NULL"]




dataCoordList <- getfileCoords(attr(dataInfo, "datasetid"), allCoords, urlbase)
if (length(dataCoordList) == 0) {
  stop("Error retrieving coordinate variable")
}
tcoordLim <- NULL
if(!is.null(tcoord)){
  isotime <- dataCoordList$time
  udtime <- as.Date(dataCoordList$time, origin='1970-01-01', tz= "GMT")
  lenTime <-length(isotime)

  if (grepl("last", tcoord1[1])) {
    tlen <- nchar(tcoord1[1])
    arith <- substr(tcoord1[1], 5, tlen)
    tempVar <- paste0(as.character(lenTime), arith)
    tIndex <- eval(parse(text=tempVar))
    tcoord1[1] <- isotime[tIndex]
  }

  if (grepl("last", tcoord1[2])) {
    tlen <- nchar(tcoord1[2])
    arith <- substr(tcoord1[2], 5, tlen)
    tempVar <- paste0(as.character(lenTime), arith)
    tIndex <- eval(parse(text=tempVar))
    tcoord1[2] <- isotime[tIndex]
  }
  udtpos <- as.Date(tcoord1, origin='1970-01-01', tz= "GMT")
  tcoordLim <- c(min(udtpos), max(udtpos))
}

dimargs <- list(xcoordLim, ycoordLim, zcoord, tcoordLim)
names(dimargs) <- c(xName, yName, zName, tName)
dimargs <- Filter(Negate(is.null), dimargs)

#check that coordinate bounds are contained in the dataset
checkBounds(dataCoordList, dimargs)

#map request limits to nearest ERDDAP coordinates
erddapXcoord <- rep(NA_real_, 2)
erddapYcoord <- rep(NA_real_, 2)
erddapTcoord <- rep(NA_real_, 2)
erddapZcoord <- zcoord1

if(xName %in% names(dataCoordList)){
  cindex <- which(names(dataCoordList) == xName)
  erddapXcoord[1] <- dataCoordList[[cindex]][which.min(abs(dataCoordList[[cindex]] - xcoordLim[1]))]
  erddapXcoord[2] <- dataCoordList[[cindex]][which.min(abs(dataCoordList[[cindex]] - xcoordLim[2]))]
}
if(yName %in% names(dataCoordList)){
  cindex <- which(names(dataCoordList) == yName)
  erddapYcoord[1] <- dataCoordList[[cindex]][which.min(abs(dataCoordList[[cindex]]- ycoordLim[1]))]
  erddapYcoord[2] <- dataCoordList[[cindex]][which.min(abs(dataCoordList[[cindex]] - ycoordLim[2]))]

}
if (tName %in% names(dataCoordList)) {
    cindex <- which(names(dataCoordList) == tName)
    erddapTcoord[1] <- isotime[which.min(abs(udtime- tcoordLim[1]))]
    erddapTcoord[2] <- isotime[which.min(abs(udtime - tcoordLim[2]))]
}
myCallOpts <- ""
if(verbose){
  myCallOpts <- "callopts = httr::verbose()"
}
griddapCmd <- 'rerddap::griddap(dataInfo,'
if(!is.null(xcoord)){
  griddapCmd <- paste0(griddapCmd, xName,'=c(',erddapXcoord[1],',',erddapXcoord[2],'),')
}
if(!is.null(ycoord)){
  griddapCmd <- paste0(griddapCmd, yName,'=c(',erddapYcoord[1],',',erddapYcoord[2],'),')
}
if(!is.null(zcoord)){
  griddapCmd <- paste0(griddapCmd, zName,'=c(',zcoord[1],',',zcoord[1],'),')
}
if(!is.null(tcoord)){
  griddapCmd <- paste0(griddapCmd, tName,'=c("',erddapTcoord[1],'","',erddapTcoord[2],'"),')
}
griddapCmd <- paste0(griddapCmd,'fields="', parameter,'",read = FALSE',myCallOpts,')')
griddapExtract <- eval(parse(text = griddapCmd))

datafileID <- ncdf4::nc_open(griddapExtract$summary$filename)

dataX <- ncdf4::ncvar_get(datafileID, varid=xName)
dataY <- ncdf4::ncvar_get(datafileID, varid=yName)
if (!is.null(zcoord)) {
  dataZ <- ncdf4::ncvar_get(datafileID, varid=zName)
}

if (!is.null(tcoord)) {
  datatime <- ncdf4::ncvar_get(datafileID, varid="time")
  datatime <- as.POSIXlt(datatime, origin='1970-01-01', tz= "GMT")
}

param <- ncdf4::ncvar_get(datafileID, varid=parameter, collapse_degen=FALSE)

ncdf4::nc_close(datafileID)
#  put longitudes back on the requestors scale
#  reqeust is on (0,360), data is not
if (xName == 'longitude'){
  if (max(xcoord) > 180.) {
    dataX <- make360(dataX)
  }
  #request is on (-180,180)
  if (min(xcoord) < 0.) {
    dataX <- make180(dataX)
  }
}

if (yName == 'latitude'){
  if (length(dataY) > 1) {
    if (dataY[1] > dataY[2]) {
      dataY <- rev(dataY)
      dataYLen <- length(dataY)
#      param <- param[, rev(seq_len(dataYLen)) ,, drop = FALSE]
      paramLen <- length(names(datafileID$dim))
      latLoc <- which(names(datafileID$dim) == 'latitude')
      myComma1 <- paste(rep(',', times = (latLoc-1)), 'rev(seq_len(dataYLen))', sep="", collapse="")
      myComma2 <- paste(rep(',', times = (paramLen-latLoc+1)),sep="", collapse="")
      paramCommand <- paste0('param <- param[', myComma1, myComma2, 'drop = FALSE]')
      paramReverse <- eval(parse(text = paramCommand))
    }
  }
}

extract <- list(NA, NA, NA, NA, NA, NA)
extract[[1]] <- param
extract[[2]] <- attributes(dataInfo)$datasetid
extract[[3]] <- dataX
extract[[4]] <- dataY
if (!is.null(zcoord)) {
  extract[[5]] <- dataZ
}
if (!is.null(tcoord)) {
  extract[[6]] <- datatime
}
if (grepl('etopo',extract[[2]])){
  names(extract) <- c('depth',"datasetname",xName, yName, zName, "time")

}else{
  names(extract) <- c(parameter,"datasetname",xName, yName, zName, "time")

}
fcopy <- file.copy(griddapExtract$summary$filename, paste0(getwd(),'/',parameter,'.nc'))
if(!fcopy){
  print('copying and renaming downloaded file from default ~/.rerddap failed')
}
fremove <- file.remove(griddapExtract$summary$filename)
if(!fremove){
  print('removing cached file from ~/.rerddap failed')
  print('check that directory as files can pile up and use up space')
}
return(extract)
}

