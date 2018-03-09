#' Extract environmental data in a 3-dimnesional box from an ERDDAP server using rerddap.
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
#' @param zcoord -  a real array with the z-coordinate (usually altitude or depth)
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
#' urlbase <- 'https://upwell.pfeg.noaa.gov/erddap'
#' dataInfo <- rerddap::info('erdMBsstd8day')
#' parameter <- 'sst'
#' xcoord <- c(230, 235)
#' ycoord <- c(40, 45)
#' tcoord <- c('2006-01-15', '2006-01-20')
#' zcoord <- c(0., 0.)
#' extract <- rxtracto_3D(dataInfo, parameter, xcoord = xcoord, ycoord = ycoord,
#'                        tcoord = tcoord, zcoord = zcoord)
#'
#' # 2-D example getting bathymetry
#' dataInfo <- rerddap::info('etopo360')
#' parameter <- 'altitude'
#' extract <- rxtracto_3D(dataInfo, parameter, xcoord = xcoord, ycoord = ycoord)
#'
#' # Example where grid is not latitude-longitude
#' dataInfo <- rerddap::info('glos_tds_5912_ca66_3f41')
#' parameter = 'temp'
#' xName <- 'nx'
#' yName <- 'ny'
#' zName <- 'nsigma'
#' xcoord <- c(10, 11)
#' ycoord <- c(10, 11)
#' zcoord <- c(1, 1)
#'  # time span changes in this dataset - get last three times
#'  myURL <- "https://upwell.pfeg.noaa.gov/erddap/griddap/glos_tds_5912_ca66_3f41.csv0?time[last - 2:1:last]"
#' myTimes <- utils::read.csv(utils::URLencode(myURL), header = FALSE, stringsAsFactors = FALSE)[[1]]
#' tcoord <- c(myTimes[1], myTimes[3])
#' extract <- rxtracto_3D(dataInfo, parameter, xcoord = xcoord, ycoord = ycoord,
#'                        zcoord = zcoord, tcoord = tcoord, xName = xName,
#'                        yName = yName, zName = zName)
#'
#' # Dataset that has depth also
#' # 3 months of subsurface temperature at 70m depth from SODA 2.2.4
#' dataInfo <- rerddap::info('erdSoda331oceanmday')
#' parameter = 'temp'
#' xName <- 'longitude'
#' yName <- 'latitude'
#' zName <- 'depth'
#' xcoord <- c(230.25, 250.25)
#' ycoord <- c(30.25, 43.25)
#' zcoord <- c(5.03355, 15.10065)
#' tcoord <- c('2010-10-15', '2010-12-15')
#' extract <- rxtracto_3D(dataInfo, parameter, xcoord = xcoord, ycoord = ycoord,
#'                        zcoord = zcoord, tcoord = tcoord, xName = xName,
#'                        yName = yName, zName = zName)
#'

rxtracto_3D <- function(dataInfo, parameter = NULL, xcoord = NULL, ycoord = NULL, zcoord = NULL, tcoord = NULL, xName = 'longitude', yName = 'latitude', zName = 'altitude', tName = 'time', urlbase = 'https://upwell.pfeg.noaa.gov/erddap/', verbose=FALSE) {


# Check Passed Info -------------------------------------------------------
 rerddap::cache_setup(temp_dir = TRUE)
 callDims <- list(xcoord, ycoord, zcoord, tcoord)
 names(callDims) <- c(xName, yName, zName, tName)
 dataInfo1 <- dataInfo
 urlbase <- checkInput(dataInfo1, parameter, urlbase, callDims)



# Check and readjust coordinate variables ---------------------------------
# get the actual coordinate values for the dataset
allCoords <- dimvars(dataInfo1)
dataCoordList <- getfileCoords(attr(dataInfo1, "datasetid"), allCoords, urlbase)
if (length(dataCoordList) == 0) {
   stop("Error retrieving coordinate variable")
}


working_coords <- remapCoords(dataInfo1, callDims, dataCoordList,  urlbase)
dataInfo1 <- working_coords$dataInfo1

# Check request is within dataset bounds ----------------------------------
#get limits over new coordinates
xcoordLim <- working_coords$xcoord1
if (working_coords$latSouth) {
    ycoordLim <- c(min(working_coords$ycoord1), max(working_coords$ycoord1))
} else {
    ycoordLim <- c(max(working_coords$ycoord1), min(working_coords$ycoord1))
}

zcoordLim <- NULL
if (!is.null(working_coords$zcoord1)) {
  zcoordLim <- working_coords$zcoord1
  if (length(zcoordLim) == 1) {
    zcoordLim <- c(zcoordLim, zcoordLim)
    }
}

tcoordLim <- NULL
if (!is.null(working_coords$tcoord1)) {
  # check for last in time,  and convert
  isoTime <- dataCoordList$time
  udtTime <- parsedate::parse_iso_8601(isoTime)
  tcoord1 <- removeLast(isoTime, working_coords$tcoord1)
  tcoord1 <- parsedate::parse_iso_8601(tcoord1)
  tcoordLim <- tcoord1
}

dimargs <- list(xcoordLim, ycoordLim, zcoordLim, tcoordLim)
names(dimargs) <- c(xName, yName, zName, tName)
dimargs <- Filter(Negate(is.null), dimargs)

#check that coordinate bounds are contained in the dataset
checkBounds(dataCoordList, dimargs)


# Find dataset coordinates closest to requested coordinates ---------------


erddapList <- findERDDAPcoord(dataCoordList, isoTime, udtTime,
                  xcoordLim,  ycoordLim, tcoordLim,  zcoordLim,
                  xName, yName, tName, zName)
erddapCoords <- erddapList$erddapCoords


# Construct the griddap() command from the input --------------------------



griddapCmd <- makeCmd(dataInfo1, urlbase, xName, yName, zName, tName, parameter,
                    erddapCoords$erddapXcoord, erddapCoords$erddapYcoord,
                    erddapCoords$erddapTcoord, erddapCoords$erddapZcoord,
                    verbose )


# Get the data ------------------------------------------------------------


griddapExtract <- do.call(rerddap::griddap, griddapCmd )



# read in the downloaded netcdf file --------------------------------------


datafileID <- ncdf4::nc_open(griddapExtract$summary$filename)

dataX <- ncdf4::ncvar_get(datafileID, varid = xName)
dataY <- ncdf4::ncvar_get(datafileID, varid = yName)
if (!is.null(zcoord)) {
  dataZ <- ncdf4::ncvar_get(datafileID, varid = zName)
}

if (!is.null(tcoord)) {
  datatime <- ncdf4::ncvar_get(datafileID, varid = "time")
  datatime <- as.POSIXlt(datatime, origin = '1970-01-01', tz = "GMT")
}

param <- ncdf4::ncvar_get(datafileID, varid = parameter, collapse_degen = FALSE)

ncdf4::nc_close(datafileID)


# Readjust lat-lon coordinates --------------------------------------------

tempCoords <- readjustCoords(param, dataX, dataY, xcoord, datafileID, callDims)
dataX <- tempCoords$dataX
dataY <- tempCoords$dataY

# create output list ------------------------------------------------------


extract <- list(NA, NA, NA, NA, NA, NA)
extract[[1]] <- tempCoords$param
extract[[2]] <- attributes(dataInfo1)$datasetid
extract[[3]] <- dataX
extract[[4]] <- dataY
if (!is.null(zcoord)) {
  extract[[5]] <- dataZ
}
if (!is.null(tcoord)) {
  extract[[6]] <- datatime
}
if (grepl('etopo',extract[[2]])) {
  names(extract) <- c('depth', "datasetname", xName, yName, zName, "time")

}else{
  names(extract) <- c(parameter, "datasetname", xName, yName, zName, "time")

}

# copy netcdf file from cache to the present directory and rename
copyFile <- paste0(getwd(), '/', parameter, '.nc')
iFile <- 1
while (file.exists(copyFile)) {
  copyFile <- paste0(getwd(), '/', parameter, '_', iFile, '.nc')
  iFile <- iFile + 1
}
fcopy <- file.copy(griddapExtract$summary$filename, copyFile)
if (!fcopy) {
  print('copying and renaming downloaded file from default ~/.rerddap failed')
}
# remove netcdf file from cache
rerddap::cache_delete(griddapExtract)
extract <- structure(extract, class = 'rxtracto3D')
return(extract)
}

