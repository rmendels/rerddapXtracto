#' Extract environmental data along a trajectory from an 'ERDDAP' server using 'rerddap'.
#'
#' \code{rxtracto_new} uses the R program 'rerddap' to extract environmental
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
#' @param interp - array (size 2) of character strings - c(interpolation type, neighborhood)
#'                 Uses the new ERDDAP interpoation option to get values
#'                 See Vignette for details
#'                 Default is Null, do not use the interpolation option
#' @param verbose - logical variable (default FALSE)
#'                   if the the URL request should be verbose
#' @param progress_bar - logical variable (default FALSE)
#'                   should a progress bar be displayed
#' @return  If success a dataframe containing:
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
#'   else an error string
#' @examples
#' ## toy example to show use
#' ## but keep execution time down
#' ##
#' # dataInfo <- rerddap::info('erdHadISST')
#' ##
#' parameter <- 'sst'
#' xcoord <- c(-130.5)
#' ycoord <- c(40.5)
#' tcoord <- c('2006-01-16')
#' # extract <- rxtracto(dataInfo, parameter = parameter, xcoord = xcoord,
#' #                   ycoord = ycoord, tcoord= tcoord
#' #                   )
#' ##
#' ## bathymetry example
#' ## 2-D example getting bathymetry
#' dataInfo <- rerddap::info('etopo360')
#' parameter <- 'altitude'
#' # extract <- rxtracto(dataInfo, parameter, xcoord = xcoord, ycoord = ycoord)
#'




rxtracto <- function(dataInfo, parameter = NULL, xcoord=NULL, ycoord = NULL,
                  zcoord = NULL, tcoord = NULL, xlen = 0., ylen = 0., zlen = 0.,
                  xName = 'longitude', yName = 'latitude', zName = 'altitude',
                  tName = 'time',
                  interp = NULL,
                  verbose = FALSE, progress_bar = FALSE) {

  #### Check Passed Info ------------------------------------------

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
  if (is.numeric(urlbase)) {
    if (urlbase == -999) {
      return("error in inputs")
    } else {
      return('url is not a valid erddap server')
    }
  }

  # Check and readjust coordinate variables ---------------------------------

  # get the list of coordinates from the info call
  allCoords <- dimvars(dataInfo1)
  # get the actual coordinate values from ERDDAP
  dataCoordList <- getfileCoords(attr(dataInfo1, "datasetid"),
                                 allCoords, urlbase)
  if (is.numeric(dataCoordList)) {
    return("Error retrieving coordinate variable")
  }

  # remap coordinates as needed,  so requested longtiudes are same as dataset
  # and deal with latitude running north-south
  working_coords <- remapCoords(dataInfo1, callDims, dataCoordList, urlbase, xlen, ylen)
  dataInfo1 <- working_coords$dataInfo1
  cross_dateline_180 <- working_coords$cross_dateline_180
  #newTime <- coordLims$newTime
  if (!(is.null(interp))){
    return_code = check_interp(urlbase, interp, working_coords$xcoord,
                               working_coords$ycoord,
                               working_coords$zcoord,
                               working_coords$tcoord)
    if(return_code == 1){
      print("Errors in interpolation information, see above")
      return("Errors in interpolation information")
    }
    extract <- erddap_interp(urlbase, attr(dataInfo1, "datasetid"), parameter,
                             working_coords$xcoord, working_coords$ycoord,
                             working_coords$zcoord, working_coords$tcoord,
                             interp, verbose, progress_bar)
    extract <- structure(extract, class = c('list', 'rxtractoTrack'))
    return(extract)
  }

  # deal with xlen = constant v. vector -----------------------

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

  # correct xcoord and ycoord by given "radius" ----------------

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
    udtTime <- parsedate::parse_iso_8601(isoTime, default_tz = "UTC")
    tcoord1 <- parsedate::parse_iso_8601(working_coords$tcoord1, default_tz = "UTC")
    tcoordLim <- c(min(tcoord1), max(tcoord1))
    req_time_index <- array(NA_integer_, dim = length(tcoord1))
    for (i in seq(1, length(tcoord1))) {
      temp_time <- parsedate::parse_iso_8601(tcoord1[i], default_tz = "UTC")
      req_time_index[i] <- which.min(abs(udtTime - temp_time))

    }
    # unique_req_time_index contains the unique times where extracts
    # need to be made
    unique_req_time_index <- unique(req_time_index)
  }
  # Check request is within dataset bounds ----------------------------------

  # create list with requested coordinate limits, check that all are in bounds
  dimargs <- list(xcoordLim, ycoordLim, zcoordLim, tcoordLim)
  names(dimargs) <- c(xName, yName, zName, tName)
  dimargs <- Filter(Negate(is.null), dimargs)
  #check that coordinate bounds are contained in the dataset
  bound_check <-  checkBounds(dataCoordList, dimargs, cross_dateline_180)
  if (bound_check != 0){
    return( 'error in given bounds')
  }


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
  if (!is.null(working_coords$tcoord1)) {
      out_dataframe[, 11] <- as.character.Date(tcoord)
   }

  # get unique time periods that actually will be called ..........


  # loop over the unique time periods .............

  if (!is.null(working_coords$tcoord1)) {
    outer_loop <- unique_req_time_index
    if (progress_bar){
      pb <- utils::txtProgressBar(min = 0, max = length(outer_loop), style = 3)
      i_pb <- 0
    }
  } else{
    outer_loop <- seq(1,1)
    if (progress_bar){
      pb <- utils::txtProgressBar(min = 0, max = length(xcoord), style = 3)
      i_pb <- 0
    }
  }
  for (iloop in outer_loop) {
    if (progress_bar){
      i_pb <- i_pb + 1
      utils::setTxtProgressBar(pb, i_pb)
     }

    # this_time_index contains  the indexes of which extracts all occur
    # at a given time
    temp_tcoord <- NA
    if (!is.null(working_coords$tcoord1)) {
      this_time_index <- which(req_time_index == iloop)
      temp_tcoord <- working_coords$tcoord1[unique_req_time_index[iloop]]
    } else {
      this_time_index <- seq(1, length(xcoord))
    }
    # define bounding box
    time_ypos <- working_coords$ycoord1[this_time_index]
    time_xpos <- working_coords$xcoord1[this_time_index]
    time_xrad <- max(xrad[this_time_index])
    time_yrad <- max(yrad[this_time_index])
    xmin <- min(time_xpos) - (time_xrad/2)
    xmax <- max(time_xpos) + (time_xrad/2)
    ymin <- min(time_ypos) - (time_yrad/2)
    ymax <- max(time_ypos) + (time_yrad/2)

    zmin <- NA
    zmax <- NA
    if (!is.null(working_coords$zcoord1)) {
      time_zpos <- working_coords$zcoord1[this_time_index]
      time_zrad <- max(zrad[this_time_index])
      zmax <- time_zpos + (time_zrad/2)
      zmin <- time_zpos - (time_zrad/2)
    }
    erddapList <- findERDDAPcoord(dataCoordList, isoTime, udtTime,
                                  c(xmin, xmax), c(ymin, ymax),
                                  c(udtTime[iloop],udtTime[iloop]), c(zmin, zmax),
                                  xName, yName, tName, zName, cross_dateline_180)
    newIndex <- erddapList$newIndex
    erddapCoords <- erddapList$erddapCoords
    requesttime <- erddapCoords$erddapTcoord[1]
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
                                    erddapCoords$erddapTcoord,
                                    erddapCoords$erddapZcoord,
                                    verbose, cache_remove = TRUE)
      if (!is.list(extract1)) {
        text1 <- "There was an error in the url call, perhaps a time out."
        text2 <- "See message on screen and URL called"
        print(paste(text1, text2))
        print("Returning incomplete download")
        out_dataframe <- out_dataframe[1:(i - 1), ]
        if (exists('paramdata')) {
          suppressWarnings(try(remove('paramdata'), silent = TRUE))
        }
        if (exists('extract1')) {
          suppressWarnings(try(rerddap::cache_delete(extract1), silent = TRUE))
        }
        return(out_dataframe)
      }
      # lower_bound <- round(min(dataCoordList$longitude), 3)
      lower_bound <- min(dataCoordList$longitude) + 0.0001
      xcoord_temp <- c(lower_bound, erddapCoords$erddapXcoord[2])
      extract2 <- data_extract_read(dataInfo1, callDims, urlbase,
                                    xName, yName, zName, tName, parameter,
                                    xcoord_temp, erddapCoords$erddapYcoord,
                                    erddapCoords$erddapTcoord,
                                    erddapCoords$erddapZcoord,
                                    verbose, cache_remove = TRUE)
      if (!is.list(extract2)) {
        text1 <- "There was an error in the url call, perhaps a time out."
        text2 <- "See message on screen and URL called"
        print(paste(text1, text2))
        print("Returning incomplete download")
        out_dataframe <- out_dataframe[1:(i - 1), ]
        if (exists('paramdata')) {
          suppressWarnings(try(remove('paramdata'), silent = TRUE))
        }
        if (exists('extract2')) {
          suppressWarnings(try(rerddap::cache_delete(extract1), silent = TRUE))
        }
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
                                   erddapCoords$erddapXcoord,
                                   erddapCoords$erddapYcoord,
                                   erddapCoords$erddapTcoord,
                                   erddapCoords$erddapZcoord,
                                   verbose, cache_remove = TRUE)

      if (!is.list(extract)) {
        text1 <- "There was an error in the url call, perhaps a time out."
        text2 <- "See message on screen and URL called"
        print(paste(text1, text2))
        print("Returning incomplete download")
        out_dataframe <- out_dataframe[1:(i - 1), ]
        if (exists('paramdata')) {
          suppressWarnings(try(remove('paramdata'), silent = TRUE))
        }
        if (exists('extract')) {
          suppressWarnings(try(rerddap::cache_delete(extract1), silent = TRUE))
        }
        if (progress_bar) {
          close(pb)
        }
        return(out_dataframe)
      }
    }

    # done extract now loop over the appropriate locations
    # populate the dataframe
    #extract[[1]] <- drop(extract[[1]])
    if (!is.null(working_coords$tcoord1)) {
      loc_loop <- this_time_index
    } else {
      loc_loop <- seq(1, length(xcoord))
    }
    for (ipos in loc_loop) {
      xIndex <- array(NA_integer_, dim = 2)
      yIndex <- array(NA_integer_, dim = 2)
      xmax <- xcoord[ipos] + (xrad[ipos]/2)
      xmin <- xcoord[ipos] - (xrad[ipos]/2)
      ymax <- ycoord[ipos] + (yrad[ipos]/2)
      ymin <- ycoord[ipos] - (yrad[ipos]/2)
      xIndex[1] <- which.min(abs(extract$longitude - xmin))
      xIndex[2] <- which.min(abs(extract$longitude - xmax))
      yIndex[1] <- which.min(abs(extract$latitude - ymin))
      yIndex[2] <- which.min(abs(extract$latitude - ymax))
      #
      # if a zcoord get its limits
      #
      if (!is.null(working_coords$zcoord1)) {
        zIndex <- array(NA_integer_, dim = 2)
        zmax <- zcoord[ipos] + (zrad[ipos]/2)
        zmin <- zcoord[ipos] - (zrad[ipos]/2)
        zIndex[1] <- which.min(abs(extract[[5]] - zmin))
        zIndex[2] <- which.min(abs(extract[[5]] - zmax))
        # if z-coordinate and time coordinate,  include in extract
        if (!is.null(working_coords$tcoord1)) {
          param <- extract[[1]][xIndex[1]:xIndex[2], yIndex[1]: yIndex[2],
                              zIndex[1]: zIndex[2], 1]
        #  just z-coordinate
        } else {
          param <- extract[[1]][xIndex[1]:xIndex[2], yIndex[1]: yIndex[2],
                                zIndex[1]: zIndex[2]]
        }
      # no z-coordinate
      } else {
        # time coordinate
        if (!is.null(working_coords$tcoord1)) {
          param <- extract[[1]][xIndex[1]:xIndex[2], yIndex[1]: yIndex[2], 1]
        # no time coordinate
        } else {
          param <- extract[[1]][xIndex[1]:xIndex[2], yIndex[1]: yIndex[2]]
        }
      }
      parameter1 <- parameter
      if (grepl('etopo',attributes(dataInfo)$datasetid)) {
        parameter1 <- "depth"
      }
      out_dataframe[ipos, 1] <- mean(param, na.rm = TRUE)
      out_dataframe[ipos, 2] <- stats::sd(param, na.rm = TRUE)
      out_dataframe[ipos, 3] <- length(param[!is.na(param)])
      if (!is.null(working_coords$tcoord1)) {
        out_dataframe[ipos, 4] <- requesttime
      }
      out_dataframe[ipos, 5] <- xmin
      out_dataframe[ipos, 6] <- xmax
      out_dataframe[ipos, 7] <- ymin
      out_dataframe[ipos, 8] <- ymax
      out_dataframe[ipos, 9] <- zmin
      out_dataframe[ipos, 10] <- zmax
    #  if (!is.null(working_coords$tcoord1)) {
    #    out_dataframe[ipos, 11] <- as.character.Date(tcoord[i])
    # }
      out_dataframe[ipos, 12] <- stats::median(param, na.rm = TRUE)
      out_dataframe[ipos, 13] <- stats::mad(param, na.rm = TRUE)

    }


  }
  out_dataframe <- structure(out_dataframe,
                             class = c('list', 'rxtractoTrack'),
                             base_url = urlbase,
                             datasetid = attributes(dataInfo)$datasetid

                             )
  if (progress_bar) {
    close(pb)
  }
  return(out_dataframe)
}


