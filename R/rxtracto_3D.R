#' Extract environmental data in a 3-dimensional box from an 'ERDDAP' server using 'rerddap'.
#'
#' \code{rxtracto_3D} uses the R program 'rerddap' to extract environmental data
#' from an 'ERDDAP' server in an (x,y,z, time) bounding box.
#' The same call could be made directly in rerddap,
#' but function is maintained as it is used in the polygon routine.
#' @export
#' @param dataInfo - the return from an 'rerddap:info' call to an 'ERDDAP' server
#' @param parameter - character string containing the name of the parameter to extract
#' @param xcoord - a real array with the x-coordinates of the trajectory (if longitude in #'   decimal degrees East, either 0-360 or -180 to 180)
#' @param ycoord -  a real array with the y-coordinate of the trajectory (if latitude in
#'   decimal degrees N; -90 to 90)
#' @param zcoord -  a real array with the z-coordinate (usually altitude or depth)
#' @param tcoord - a character array with the times of the trajectory in
#'   "YYYY-MM-DD" - for now restricted to be time.
#' @param xName - character string with name of the xcoord in the 'ERDDAP' dataset (default "longitude")
#' @param yName - character string with name of the ycoord in the 'ERDDAP' dataset (default "latitude")
#' @param zName - character string with name of the zcoord in the 'ERDDAP' dataset (default "altitude")
#' @param tName - character string with name of the tcoord in the 'ERDDAP' dataset (default "time")
#' @param verbose - logical variable (default FALSE) if the the URL request should be verbose
#' @param cache_remove - logical variable (default TRUE) whether to delete 'rerddap' cache
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
#' # toy example to show use
#' # and keep execution time low
#' \donttest{
#' dataInfo <- rerddap::info('erdHadISST')
#' }
#' parameter <- 'sst'
#' xcoord <- c(-130.5, -130.5)
#' ycoord <- c(40.5, 40.5)
#' tcoord <- c('2006-01-16', '2006-01-16')
#' extract <- rxtracto_3D(dataInfo, parameter, xcoord = xcoord, ycoord = ycoord,
#'                        tcoord = tcoord)
#'
#' \donttest{
#' # 2-D example getting bathymetry
#' dataInfo <- rerddap::info('etopo360')
#' parameter <- 'altitude'
#' extract <- rxtracto_3D(dataInfo, parameter, xcoord = xcoord, ycoord = ycoord)
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
#' }
#'

rxtracto_3D <- function(dataInfo, parameter = NULL, xcoord = NULL,
                        ycoord = NULL, zcoord = NULL, tcoord = NULL,
                        xName = 'longitude', yName = 'latitude',
                        zName = 'altitude', tName = 'time',
                        verbose=FALSE, cache_remove = TRUE) {


# Check Passed Info -------------------------------------------------------
 rerddap::cache_setup(temp_dir = TRUE)
 callDims <- list(xcoord, ycoord, zcoord, tcoord)
 names(callDims) <- c(xName, yName, zName, tName)
 dataInfo1 <- dataInfo
 urlbase <- dataInfo1$base_url
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
cross_dateline_180 <- working_coords$cross_dateline_180
# Check request is within dataset bounds ----------------------------------
#get limits over new coordinates
xcoordLim <- working_coords$xcoord1
#if (working_coords$latSouth) {
#    ycoordLim <- c(min(working_coords$ycoord1), max(working_coords$ycoord1))
#} else {
#    ycoordLim <- c(max(working_coords$ycoord1), min(working_coords$ycoord1))
#}
ycoordLim <- c(min(working_coords$ycoord1), max(working_coords$ycoord1))

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
checkBounds(dataCoordList, dimargs, cross_dateline_180)


# Find dataset coordinates closest to requested coordinates ---------------


erddapList <- findERDDAPcoord(dataCoordList, isoTime, udtTime,
                  xcoordLim,  ycoordLim, tcoordLim,  zcoordLim,
                  xName, yName, tName, zName, cross_dateline_180)
erddapCoords <- erddapList$erddapCoords


# Construct the griddap() command from the input --------------------------

if (cross_dateline_180) {
  # upper_bound <- round(max(dataCoordList$longitude), 3)
  upper_bound <- max(dataCoordList$longitude) - 0.0001
  xcoord_temp <- c(erddapCoords$erddapXcoord[1], upper_bound)
  extract1 <- data_extract_read(dataInfo1, callDims, urlbase,
                               xName, yName, zName, tName, parameter,
                               xcoord_temp, erddapCoords$erddapYcoord,
                               erddapCoords$erddapTcoord, erddapCoords$erddapZcoord,
                               verbose, cache_remove )
  if (!is.list(extract1)) {
    text1 <- "There was an error in the url call, perhaps a time out."
    text2 <- "See message on screen and URL called"
    print(paste(text1, text2))
    stop("stopping download")
  }
  # lower_bound <- round(min(dataCoordList$longitude), 3)
  lower_bound <- min(dataCoordList$longitude) + 0.0001
  xcoord_temp <- c(lower_bound, erddapCoords$erddapXcoord[2])
  extract2 <- data_extract_read(dataInfo1, callDims, urlbase,
                                xName, yName, zName, tName, parameter,
                                xcoord_temp, erddapCoords$erddapYcoord,
                                erddapCoords$erddapTcoord, erddapCoords$erddapZcoord,
                                verbose, cache_remove )
  if (!is.list(extract2)) {
    text1 <- "There was an error in the url call, perhaps a time out."
    text2 <- "See message on screen and URL called"
    print(paste(text1, text2))
    stop("stopping download")
  }
  extract2$longitude = make360(extract2$longitude)
  # extract <- list(NA, NA, NA, NA, NA, NA)
  extract <- vector("list", 6)
  lat_len <- length(extract1$latitude)
  time_len <- length(extract1$time)
  lon_len <- length(extract1$longitude) + length(extract2$longitude)
  temp_array <- array(NA_real_, dim = c(lon_len, lat_len, time_len))
  temp_array[1:length(extract1$longitude), ,] <- extract1[[1]]
  temp_array[(length(extract1$longitude) + 1):lon_len, ,] <- extract2[[1]]
  names(extract) <- names(extract1)
  extract[[1]] <- temp_array
  extract$datasetname <- extract1$datasetname
  extract$latitude <- extract1$latitude
  extract$altitude <- ifelse(is.null(extract1$altitude), NA, extract1$altitude)
  if (is.null(extract1$time)) {
    extract$time <- NA
  } else{
    extract$time <-  extract1$time
  }
  extract$longitude <- c(extract1$longitude, extract2$longitude)
}else {
  extract <- data_extract_read(dataInfo1, callDims, urlbase,
                               xName, yName, zName, tName, parameter,
                               erddapCoords$erddapXcoord, erddapCoords$erddapYcoord,
                               erddapCoords$erddapTcoord, erddapCoords$erddapZcoord,
                               verbose, cache_remove )

}
if (!is.list(extract)) {
  text1 <- "There was an error in the url call, perhaps a time out."
  text2 <- "See message on screen and URL called"
  print(paste(text1, text2))
  stop("stopping download")
}

extract <- structure(extract, class = c('list', 'rxtracto3D'))

}

