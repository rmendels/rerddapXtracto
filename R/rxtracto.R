#' Extract environmental data along a trajectory from an ERDDAP server using rerddap.
#'
#' \code{rxtracto} uses the R program rerddap to extract environmental
#' data from an ERDDAP server along a (x,y,z, time) trajectory.
#' @export
#' @param dataInfo - the return from an rerddap "info" call to an ERDDAP server
#' @param parameter - character string containing the name of the parameter to extract
#' @param xcoord - a real array with the x-coordinates of the trajectory (if longitude in #'   decimal degrees East, either 0-360 or -180 to 180)
#' @param ycoord -  a real array with the y-coordinate of the trajectory (if latitude in
#'   decimal degrees N; -90 to 90)
#' @param zcoord -  a real array with the z-coordinate of the trajectory (usually altitude or depth)
#' @param tcoord - a character array with the times of the trajectory in
#'   "YYYY-MM-DD" - for now restricted to be time.
#' @param xlen - real array defining the longitude box around the given point (xlen/2 around the point)
#' @param ylen - real array defining the latitude box around the given point (ylen/2 around the point)
#' @param zlen - real array defining the depth or altitude box around the given point (zlen/2 around the point)
#' @param xName - character string with name of the xcoord in the ERDDAP dataset (default "longitude")
#' @param yName - character string with name of the ycoord in the ERDDAP dataset (default "latitude")
#' @param zName - character string with name of the zcoord in the ERDDAP dataset (default "altitude")
#' @param tName - character string with name of the tcoord in the ERDDAP dataset (default "time")
#' @param urlbase - base URL of the ERDDAP server being accessed - default "http://upwell.pfeg.noaa.gov/erddap"
#' @param verbose - logical variable (default FALSE) if the the URL request should be verbose
#' @return A dataframe containing:
#' \itemize{
#'  \item column 1 = mean of data within search radius
#'  \item column 2 = standard deviation of data within search radius
#'  \item column 3 = number of points found within search radius
#'  \item column 4 = time of returned value
#'  \item column 5 = min longitude of call (decimal degrees)
#'  \item column 6 = max longitude of call (decimal degrees)
#'  \item column 7 = min latitude of call (decimal degrees)
#'  \item column 8 = max latitude of call (decimal degrees)
#'  \item column 9 = requested time in tag
#'  \item column 10 = median of data within search radius
#'  \item column 11 = median absolute deviation of data within search radius
#'  }
#' @examples
#' urlbase <- 'https://upwell.pfeg.noaa.gov/erddap'
#' dataInfo <- rerddap::info('erdMBsstd8day')
#' parameter <- 'sst'
#' xcoord <- c(230, 231)
#' ycoord <- c(40, 41)
#' tcoord <- c('2006-01-15', '2006-01-20')
#' zcoord <- c(0., 0.)
#' xlen <- 0.5
#' ylen <- 0.5
#' extract <- rxtracto(dataInfo, parameter = parameter, xcoord = xcoord,
#'                     ycoord = ycoord, tcoord= tcoord, zcoord = zcoord,
#'                     xlen = xlen, ylen = ylen)
#' # 2-D example getting bathymetry
#' dataInfo <- rerddap::info('etopo360')
#' parameter <- 'altitude'
#' extract <- rxtracto(dataInfo, parameter, xcoord = xcoord, ycoord = ycoord,
#'                     xlen = xlen, ylen = ylen)
#'
#' # Example where grid is not latitude-longitude
#' dataInfo <- rerddap::info('glos_tds_5912_ca66_3f41')
#' parameter <- 'temp'
#' xName <- 'nx'
#' yName <- 'ny'
#' zName <- 'nsigma'
#' xcoord <- c(10, 11)
#' ycoord <- c(10, 11)
#' zcoord <- c(1, 1)
#' tcoord <- c('2016-09-02', '2016-09-03')
#' xlen <- 0
#' ylen <- 0
#' extract <- rxtracto(dataInfo, parameter, xcoord = xcoord, ycoord = ycoord,
#'                     zcoord = zcoord, tcoord = tcoord, xlen = xlen,
#'                     ylen = ylen, xName = xName, yName = yName, zName = zName)




rxtracto <- function(dataInfo, parameter = NULL, xcoord=NULL, ycoord = NULL, zcoord = NULL, tcoord = NULL, xlen = 0., ylen = 0., zlen = 0., xName = 'longitude', yName = 'latitude', zName = 'altitude', tName = 'time', urlbase = 'https://upwell.pfeg.noaa.gov/erddap', verbose = FALSE) {

  # Check Passed Info -------------------------------------------------------
  rerddap::cache_setup(temp_dir = TRUE)
  callDims <- list(xcoord, ycoord, zcoord, tcoord)
  names(callDims) <- c(xName, yName, zName, tName)

  # Check that the non-null input vectors are the same length
  dimLengths <- lapply(callDims, length)
  dimLengths[dimLengths == 0] <- NULL
  dimLengths1 <- Filter(Negate(is.null), dimLengths)
  lengthTest <- all(sapply(dimLengths1, function(x) x == dimLengths1[1]))
  if (!lengthTest) {
    print("The length of the non-empty coordinates do not agree")
    names(dimLengths) <- c(xName, yName, zName, tName)
    print(dimLengths)
    stop("Correct Input Vectors")
  }

  # Check and readjust coordinate variables ---------------------------------
  # get the actual coordinate values for the dataset
  urlbase <- checkInput(dataInfo, parameter, urlbase, callDims)
  allCoords <- dimvars(dataInfo)
  dataCoordList <- getfileCoords(attr(dataInfo, "datasetid"), allCoords, urlbase)
  if (length(dataCoordList) == 0) {
    stop("Error retrieving coordinate variable")
  }

  working_coords <- remapCoords(dataInfo, callDims, dataCoordList, urlbase)
  #newTime <- coordLims$newTime


  # deal with xlen = constant v. vector
  if (length(xlen) == 1) {
    xrad <- rep(xlen, length(xcoord))
    yrad <- rep(ylen, length(ycoord))
    zrad <- rep(zlen, length(zcoord))
  } else {
    xrad <- xlen
    yrad <- ylen
    zrad <- zlen
  }



# correct xcoord and ycoord by given "radius"
 xcoordLim <- c(min(working_coords$xcoord1 - (xrad/2)), max(working_coords$xcoord1 + (xrad/2)))
 ycoordLim <- c(min(working_coords$ycoord1 - (yrad/2)), max(working_coords$ycoord1 + (yrad/2)))
 zcoordLim <- NULL
 if (!is.null(working_coords$zcoord1)) {
   zcoordLim <- c(min(working_coords$zcoord1 - (zrad/2)), max(working_coords$zcoord1 + (zrad/2)))
 }
 tcoordLim <- NULL
 if (!is.null(working_coords$tcoord1)) {
   isoTime <- dataCoordList$time
   udtTime <- parsedate::parse_iso_8601(isoTime)
   tcoord1 <- parsedate::parse_iso_8601(working_coords$tcoord1)
   tcoordLim <- c(min(tcoord1), max(tcoord1))
 }




 # Check request is within dataset bounds ----------------------------------

 # create list with requested coordinate limits, check that all are in bounds
 dimargs <- list(xcoordLim, ycoordLim, zcoordLim, tcoordLim)
 names(dimargs) <- c(xName, yName, zName, tName)
 dimargs <- Filter(Negate(is.null), dimargs)
 #check that coordinate bounds are contained in the dataset
 checkBounds(dataCoordList, dimargs)


# create structures to store request --------------------------------------


  out_dataframe <- as.data.frame(matrix(ncol = 13,nrow = length(xcoord)))
 if (("latitude" %in% names(dimargs))  & ("longitude" %in% names(dimargs))) {
  dimnames(out_dataframe)[[2]] <- c(paste0('mean ', parameter), paste0('stdev ', parameter), 'n', 'satellite date', 'requested lon min',      'requested lon max', 'requested lat min', 'requested lat max', 'requested z min', 'requested z max', 'requested date', paste0('median ', parameter), paste0('mad ', parameter))
} else{
  dimnames(out_dataframe)[[2]] <- c(paste0('mean ', parameter), paste0('stdev ', parameter), 'n', 'satellite date', 'requested x min',  'requested x max', 'requested y min', 'requested y max', 'requested z min', 'requested z max', 'requested date', paste0('median ', parameter), paste0('mad ', parameter))
}

# Will calculate actual index of each coordinate requested to compare old and new request
# store in oldIndex, as well as dataframe to store create dataframe
oldIndex <- list(xIndex = rep(NA_integer_, 2), yIndex = rep(NA_integer_, 2), zIndex = rep(NA_integer_, 2), TimeIndex = rep(NA_integer_, 2))
newIndex <- oldIndex
oldDataFrame <- out_dataframe[1, ]

# logical variable if the latitude coordinate goes south to north
latSouth <- working_coords$latSouth

# loop over the track positions
 for (i in 1:length(xcoord)) {


# define bounding box
  xmax <- working_coords$xcoord1[i] + (xrad[i]/2)
  xmin <- working_coords$xcoord1[i] - (xrad[i]/2)
  if (latSouth) {
     ymax <- working_coords$ycoord1[i] + (yrad[i]/2)
     ymin <- working_coords$ycoord1[i] - (yrad[i]/2)
  } else {
     ymin <- working_coords$ycoord1[i] + (yrad[i]/2)
     ymax <- working_coords$ycoord1[i] - (yrad[i]/2)
  }
  zmin <- NA
  zmax <- NA
  if (!is.null(working_coords$zcoord1[i])) {
    zmax <- working_coords$zcoord1[i] + (zrad[i]/2)
    zmin <- working_coords$zcoord1[i] - (zrad[i]/2)
  }

  erddapList <- findERDDAPcoord(dataCoordList, isoTime, udtTime,
                                c(xmin, xmax), c(ymin, ymax),
                                c(tcoord1[i], tcoord1[i]), c(zmin, zmax),
                                xName, yName, tName, zName)
  newIndex <- erddapList$newIndex
  erddapCoords <- erddapList$erddapCoords
  requesttime <- erddapCoords$erddapTcoord[1]

# test if the indices of the new request are the same as the previous request
# if the same just copy the old dataframe
   if (identical(newIndex, oldIndex)) {
     # the call will be the same as last time, so no need to repeat
     out_dataframe[i,] <- oldDataFrame
   } else {
     griddapCmd <- makeCmd(dataInfo, urlbase, xName, yName, zName, tName, parameter,
                           erddapCoords$erddapXcoord, erddapCoords$erddapYcoord,
                           erddapCoords$erddapTcoord, erddapCoords$erddapZcoord,
                           verbose )

     extract <- do.call(rerddap::griddap, griddapCmd )

    if (length(extract) == 0) {
       print(griddapCmd)
       stop("There was an error in the url call.  See message on screen and URL called")
        }
    # read in netcdf file
     datafileID <- ncdf4::nc_open(extract$summary$filename)
     paramdata <- ncdf4::ncvar_get(datafileID, varid = parameter, collapse_degen = FALSE)
     ncdf4::nc_close(datafileID)

     # adjust coordiantes back to original scale,  latitudes always go south-north
     # tempCoords <- readjustCoords(dataX, dataY, xcoord, datafileID, callDims)

    # populate the dataframe
     out_dataframe[i, 1] <- mean(paramdata, na.rm = T)
     out_dataframe[i, 2] <- stats::sd(paramdata, na.rm = T)
     out_dataframe[i, 3] <- length(stats::na.omit(paramdata))
     if (!is.null(working_coords$tcoord1)) {
       out_dataframe[i, 4] <- requesttime
     }
     out_dataframe[i, 5] <- xmin
     out_dataframe[i, 6] <- xmax
     out_dataframe[i, 7] <- ymin
     out_dataframe[i, 8] <- ymax
     out_dataframe[i, 9] <- zmin
     out_dataframe[i, 10] <- zmax
     if (!is.null(working_coords$tcoord1)) {
       out_dataframe[i, 11] <- as.character.Date(tcoord[i])
     }
     out_dataframe[i, 12] <- stats::median(paramdata, na.rm = T)
     out_dataframe[i, 13] <- stats::mad(paramdata, na.rm = T)

     remove('paramdata')
     rerddap::cache_delete(extract)
     }
   oldIndex <- newIndex
   oldDataFrame <- out_dataframe[i,]

}
return(out_dataframe)
}


