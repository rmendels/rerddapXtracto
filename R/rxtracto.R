#' Extract environmental data along a trajectory from an 'ERDDAP' server using 'rerddap'.
#'
#' \code{rxtracto} uses the R program 'rerddap' to extract environmental
#' data from an 'ERDDAP' server along a (x,y,z, time) trajectory.
#' @export
#' @param dataInfo - the return from an 'rerddap::info' call to an 'ERDDAP' server
#' @param parameter - character string containing the name of the parameter to extract
#' @param xcoord - a real array with the x-coordinates of the trajectory (if longitude in #'   decimal degrees East, either 0-360 or -180 to 180)
#' @param ycoord -  a real array with the y-coordinate of the trajectory (if latitude in
#'   decimal degrees N; -90 to 90)
#' @param zcoord -a real array with the z-coordinate of the trajectory (usually altitude or depth)
#' @param tcoord - a character array with the times of the trajectory in
#'   "YYYY-MM-DD" - for now restricted to be time.
#' @param xlen - real array defining the longitude box around the given point (xlen/2 around the point)
#' @param ylen - real array defining the latitude box around the given point (ylen/2 around the point)
#' @param zlen - real array defining the depth or altitude box around the given point (zlen/2 around the point)
#' @param xName - character string with name of the xcoord in the 'ERDDAP' dataset (default "longitude")
#' @param yName - character string with name of the ycoord in the 'ERDDAP' dataset (default "latitude")
#' @param zName - character string with name of the zcoord in the 'ERDDAP' dataset (default "altitude")
#' @param tName - character string with name of the tcoord in the 'ERDDAP' dataset (default "time")
#' @param verbose - logical variable (default FALSE)
#'                   if the the URL request should be verbose
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
#' # toy example to show use
#' # but keep execution time down
#' \donttest{
#' dataInfo <- rerddap::info('erdHadISST')
#' }
#' parameter <- 'sst'
#' xcoord <- c(-130.5)
#' ycoord <- c(40.5)
#' tcoord <- c('2006-01-16')
#' xlen <- 0.01
#' ylen <- 0.01
#' extract <- rxtracto(dataInfo, parameter = parameter, xcoord = xcoord,
#'                     ycoord = ycoord, tcoord= tcoord,
#'                     xlen = xlen, ylen = ylen)
#' \donttest{
#' # 2-D example getting bathymetry
#' dataInfo <- rerddap::info('etopo360')
#' parameter <- 'altitude'
#' extract <- rxtracto(dataInfo, parameter, xcoord = xcoord, ycoord = ycoord,
#'                     xlen = xlen, ylen = ylen)
#' }
#'




rxtracto <- function(dataInfo, parameter = NULL, xcoord=NULL, ycoord = NULL,
                  zcoord = NULL, tcoord = NULL, xlen = 0., ylen = 0., zlen = 0.,
                  xName = 'longitude', yName = 'latitude', zName = 'altitude',
                  tName = 'time',
                  verbose = FALSE) {

  # Check Passed Info -------------------------------------------------------
  rerddap::cache_setup(temp_dir = TRUE)
  callDims <- list(xcoord, ycoord, zcoord, tcoord)
  names(callDims) <- c(xName, yName, zName, tName)
  dataInfo1 <- dataInfo
  urlbase <- dataInfo1$base_url
  # Check that the non-null input vectors are the same length
  dimLengths <- lapply(callDims, length)
  dimLengths[dimLengths == 0] <- NULL
  dimLengths1 <- Filter(Negate(is.null), dimLengths)
  lengthTest <- all(vapply(dimLengths1, function(x) x == dimLengths1[1],
                           logical(1)))
  if (!lengthTest) {
    print("The length of the non-empty coordinates do not agree")
    names(dimLengths) <- c(xName, yName, zName, tName)
    print(dimLengths)
    stop("Correct Input Vectors")
  }

  # check "radius" input
  if ( (length(xlen) > 1) && (length(xlen) != length(xcoord))) {
    stop('xlen is of size greater than one but not equal to length of xccoord')
  }
  if ( (length(ylen) > 1) && (length(ylen) != length(ycoord))) {
    stop('ylen is of size greater than one but not equal to length of yccoord')
  }
  if ( (!is.null(zcoord)) && length(xlen)  > 1) {
    if (length(zlen) == 1)  {
      message('warning - zlen has a single value')
      message('xlen and ylen have length greater than 1')
      message('zlen will be extended to be same length with repeated value')
    }
  }
  urlbase <- checkInput(dataInfo1, parameter, urlbase, callDims)

  # Check and readjust coordinate variables ---------------------------------
  # get the list of coordinates from the info call
  allCoords <- dimvars(dataInfo1)
  # get the actual coordinate values from ERDDAP
  dataCoordList <- getfileCoords(attr(dataInfo1, "datasetid"),
                                 allCoords, urlbase)
  if (length(dataCoordList) == 0) {
    stop("Error retrieving coordinate variable")
  }

  # remap coordinates as needed,  so requested longtiudes are same as dataset
  # and deal with latitude running north-south
   working_coords <- remapCoords(dataInfo1, callDims, dataCoordList, urlbase, xlen, ylen)
   dataInfo1 <- working_coords$dataInfo1
   cross_dateline_180 <- working_coords$cross_dateline_180
   #newTime <- coordLims$newTime


  # deal with xlen = constant v. vector
  if (length(xlen) == 1) {
    xrad <- rep(xlen, length(xcoord))
    yrad <- rep(ylen, length(ycoord))
    zrad <- rep(zlen, length(zcoord))
  } else {
    xrad <- xlen
    yrad <- ylen
    if ( (!is.null(zcoord)) && (length(zlen) == 1)) {
        zrad <- rep(zlen, length(zcoord))
    } else {
        zrad <- zlen
    }
  }



# correct xcoord and ycoord by given "radius"
 xcoordLim <- c(min(working_coords$xcoord1 - (xrad/2)),
                max(working_coords$xcoord1 + (xrad/2)))
 ycoordLim <- c(min(working_coords$ycoord1 - (yrad/2)),
                max(working_coords$ycoord1 + (yrad/2)))
 zcoordLim <- NULL
 if (!is.null(working_coords$zcoord1)) {
   zcoordLim <- c(min(working_coords$zcoord1 - (zrad/2)),
                  max(working_coords$zcoord1 + (zrad/2)))
 }
 # not only get time limits but convert to an R date
 # so numerical value for comparison is available
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
 checkBounds(dataCoordList, dimargs, cross_dateline_180)


# create structures to store request --------------------------------------


  out_dataframe <- as.data.frame(matrix(ncol = 13,nrow = length(xcoord)))
 if (("latitude" %in% names(dimargs))  & ("longitude" %in% names(dimargs))) {
  dimnames(out_dataframe)[[2]] <- c(paste0('mean ', parameter),
                                    paste0('stdev ',parameter), 'n',
                                    'satellite date', 'requested lon min',
                                    'requested lon max', 'requested lat min',
                                    'requested lat max', 'requested z min',
                                    'requested z max', 'requested date',
                                    paste0('median ', parameter),
                                    paste0('mad ', parameter))
} else{
  dimnames(out_dataframe)[[2]] <- c(paste0('mean ', parameter),
                                    paste0('stdev ', parameter), 'n',
                                    'satellite date', 'requested x min',
                                    'requested x max', 'requested y min',
                                    'requested y max', 'requested z min',
                                    'requested z max', 'requested date',
                                    paste0('median ', parameter),
                                    paste0('mad ', parameter))
}

# Will calculate actual index of each coordinate requested
# to compare old and new request
# store in oldIndex, as well as dataframe to store create dataframe
oldIndex <- list(xIndex = rep(NA_integer_, 2), yIndex = rep(NA_integer_, 2),
                 zIndex = rep(NA_integer_, 2), TimeIndex = rep(NA_integer_, 2))
newIndex <- oldIndex
oldDataFrame <- out_dataframe[1, ]

# logical variable if the latitude coordinate goes south to north
#latSouth <- working_coords$latSouth

# loop over the track positions
 for (i in seq_len(length(xcoord))) {


# define bounding box
  xmax <- working_coords$xcoord1[i] + (xrad[i]/2)
  xmin <- working_coords$xcoord1[i] - (xrad[i]/2)
  ymax <- working_coords$ycoord1[i] + (yrad[i]/2)
  ymin <- working_coords$ycoord1[i] - (yrad[i]/2)

  zmin <- NA
  zmax <- NA
  if (!is.null(working_coords$zcoord1[i])) {
    zmax <- working_coords$zcoord1[i] + (zrad[i]/2)
    zmin <- working_coords$zcoord1[i] - (zrad[i]/2)
  }

  erddapList <- findERDDAPcoord(dataCoordList, isoTime, udtTime,
                                c(xmin, xmax), c(ymin, ymax),
                                c(tcoord1[i], tcoord1[i]), c(zmin, zmax),
                                xName, yName, tName, zName, cross_dateline_180)
  newIndex <- erddapList$newIndex
  erddapCoords <- erddapList$erddapCoords
  requesttime <- erddapCoords$erddapTcoord[1]

# test if the indices of the new request are the same as the previous request
# if the same just copy the old dataframe
   if (identical(newIndex, oldIndex)) {
     # the call will be the same as last time, so no need to repeat
     out_dataframe[i,] <- oldDataFrame
   } else {
     # cross_dateline_180 tests if the request ever crosses dateline
     # cross_dateline_180_local tests for the spefic location
     # along the track
     cross_dateline_180_local <- FALSE
     lon_signs <- sign(erddapCoords$erddapXcoord)
     if (lon_signs[1] != lon_signs[2]) {cross_dateline_180_local <- TRUE}
       if (cross_dateline_180_local) {
       # upper_bound <- round(max(dataCoordList$longitude), 3)
       upper_bound <- max(dataCoordList$longitude) - 0.0001
       xcoord_temp <- c(erddapCoords$erddapXcoord[1], upper_bound)
       extract1 <- data_extract_read(dataInfo1, callDims, urlbase,
                                     xName, yName, zName, tName, parameter,
                                     xcoord_temp, erddapCoords$erddapYcoord,
                                     erddapCoords$erddapTcoord, erddapCoords$erddapZcoord,
                                     verbose, cache_remove = TRUE)
       if (!is.list(extract1)) {
         text1 <- "There was an error in the url call, perhaps a time out."
         text2 <- "See message on screen and URL called"
         print(paste(text1, text2))
         print("Returning incomplete download")
         out_dataframe <- out_dataframe[1:(i - 1), ]
         remove('paramdata')
         rerddap::cache_delete(extract1)
         return(out_dataframe)
       }
       # lower_bound <- round(min(dataCoordList$longitude), 3)
       lower_bound <- min(dataCoordList$longitude) + 0.0001
       xcoord_temp <- c(lower_bound, erddapCoords$erddapXcoord[2])
       extract2 <- data_extract_read(dataInfo1, callDims, urlbase,
                                     xName, yName, zName, tName, parameter,
                                     xcoord_temp, erddapCoords$erddapYcoord,
                                     erddapCoords$erddapTcoord, erddapCoords$erddapZcoord,
                                     verbose, cache_remove = TRUE)
       if (!is.list(extract2)) {
         text1 <- "There was an error in the url call, perhaps a time out."
         text2 <- "See message on screen and URL called"
         print(paste(text1, text2))
         print("Returning incomplete download")
         out_dataframe <- out_dataframe[1:(i - 1), ]
         remove('paramdata')
         rerddap::cache_delete(extract2)
         return(out_dataframe)
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
                                    verbose, cache_remove = TRUE)

     }
     if (!is.list(extract)) {
       text1 <- "There was an error in the url call, perhaps a time out."
       text2 <- "See message on screen and URL called"
       print(paste(text1, text2))
       print("Returning incomplete download")
       out_dataframe <- out_dataframe[1:(i - 1), ]
       remove('paramdata')
       rerddap::cache_delete(extract)
       return(out_dataframe)
     }


    # populate the dataframe
     parameter1 <- parameter
     if (grepl('etopo',attributes(dataInfo)$datasetid)) {
       parameter1 <- "depth"
     }
     out_dataframe[i, 1] <- mean(extract[[parameter1]], na.rm = TRUE)
     out_dataframe[i, 2] <- stats::sd(extract[[parameter1]], na.rm = TRUE)
     out_dataframe[i, 3] <- length(extract[[parameter1]][!is.na(extract[[parameter1]])])
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
     out_dataframe[i, 12] <- stats::median(extract[[parameter1]], na.rm = TRUE)
     out_dataframe[i, 13] <- stats::mad(extract[[parameter1]], na.rm = TRUE)

   }
   # store last request in case next one is same
   oldIndex <- newIndex
   oldDataFrame <- out_dataframe[i,]

 }
out_dataframe <- structure(out_dataframe, class = c('list', 'rxtractoTrack'))
return(out_dataframe)
}


