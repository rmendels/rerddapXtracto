#' Extract environmental data along a trajectory from an 'ERDDAP™' server using 'rerddap'.
#'
#' \code{rxtracto} uses the R program 'rerddap' to extract environmental
#' data from an 'ERDDAP™' server along a (x,y,z, time) trajectory.
#' @export
#' @param dataInfo - the return from an 'rerddap::info' call to an 'ERDDAP™' server
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
#' @param xName - character string with name of the xcoord in the 'ERDDAP™' dataset (default "longitude")
#' @param yName - character string with name of the ycoord in the 'ERDDAP™' dataset (default "latitude")
#' @param zName - character string with name of the zcoord in the 'ERDDAP™' dataset (default "altitude")
#' @param tName - character string with name of the tcoord in the 'ERDDAP™' dataset (default "time")
#' @param interp - array (size 2) of character strings - c(interpolation type, neighborhood)
#'                 Uses the new ERDDAP™ interpoation option to get values
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
#' ## Wrap rerddap::info('etopo360') call in function that insures proper finish if it fails
#' dataInfo <- safe_info('etopo360')
#' parameter <- 'altitude'
#' # extract <- rxtracto(dataInfo, parameter, xcoord = xcoord, ycoord = ycoord)
#'
rxtracto <- function(dataInfo, parameter = NULL, xcoord=NULL, ycoord = NULL,
                  zcoord = NULL, tcoord = NULL, xlen = 0., ylen = 0., zlen = 0.,
                  xName = 'longitude', yName = 'latitude', zName = 'altitude',
                  tName = 'time',
                  interp = NULL,
                  verbose = FALSE, progress_bar = FALSE) {

  # Check passed info -------------------------------------------------------

  rerddap::cache_setup(temp_dir = TRUE)
  callDims <- list(xcoord, ycoord, zcoord, tcoord)
  names(callDims) <- c(xName, yName, zName, tName)
  check_track_lengths(callDims, xcoord, ycoord, zcoord, xlen, ylen, zlen)
  urlbase <- checkInput(dataInfo, parameter, dataInfo$base_url, callDims)
  if (is.numeric(urlbase)) {
    if (urlbase == -999) {
      return("error in inputs")
    } else {
      return('url is not a valid erddap server')
    }
  }

  # Check and readjust coordinate variables ---------------------------------

  # get the list of coordinates from the info call
  allCoords <- dimvars(dataInfo)
  # get the actual coordinate values from ERDDAP™
  dataCoordList <- getfileCoords(attr(dataInfo, "datasetid"), allCoords, urlbase)
  if (is.numeric(dataCoordList)) {
    return("Error retrieving coordinate variable")
  }

  # remap coordinates as needed,  so requested longtiudes are same as dataset
  # and deal with latitude running north-south
  working_coords <- remapCoords(dataInfo, callDims, dataCoordList, urlbase, xlen, ylen)
  cross_dateline_180 <- working_coords$cross_dateline_180
  has_time <- !is.null(working_coords$tcoord1)
  has_z <- !is.null(working_coords$zcoord1)

  # ERDDAP™ interpolation true
  if (!(is.null(interp))){
    # check that the info is correct to perform the interpolation
    return_code <- check_interp(urlbase, interp, working_coords$xcoord1,
                               working_coords$ycoord1,
                               working_coords$zcoord1,
                               working_coords$tcoord1)
    if(return_code == 1){
      print("Errors in interpolation information, see above")
      return("Errors in interpolation information")
    }
    # interpolate along the track
    extract <- erddap_interp(urlbase, attr(dataInfo, "datasetid"), parameter,
                             working_coords$xcoord1, working_coords$ycoord1,
                             working_coords$zcoord1, working_coords$tcoord1,
                             interp, verbose, progress_bar)
    extract <- structure(extract, class = c('list', 'rxtractoTrack'))
    return(extract)
  }

  # one "radius" per track location -----------------------------------------

  xrad <- rep_len(xlen, length(xcoord))
  yrad <- rep_len(ylen, length(ycoord))
  zrad <- rep_len(zlen, length(zcoord))

  # correct xcoord and ycoord by given "radius" ------------------------------

  xcoordLim <- c(min(working_coords$xcoord1 - (xrad/2)),
                max(working_coords$xcoord1 + (xrad/2)))
  ycoordLim <- c(min(working_coords$ycoord1 - (yrad/2)),
                max(working_coords$ycoord1 + (yrad/2)))
  zcoordLim <- NULL
  if (has_z) {
    zcoordLim <- c(min(working_coords$zcoord1 - (zrad/2)),
                  max(working_coords$zcoord1 + (zrad/2)))
  }
  # not only get time limits but convert to an R date
  # so numerical value for comparison is available
  tcoordLim <- NULL
  isoTime <- NULL
  udtTime <- NULL
  req_time_index <- NULL
  unique_req_time_index <- NULL
  if (has_time) {
    isoTime <- dataCoordList$time
    udtTime <- parsedate::parse_iso_8601(isoTime, default_tz = "UTC")
    tcoord1 <- parsedate::parse_iso_8601(working_coords$tcoord1, default_tz = "UTC")
    tcoordLim <- c(min(tcoord1), max(tcoord1))
    req_time_index <- array(NA_integer_, dim = length(tcoord1))
    for (i in seq_along(tcoord1)) {
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

  # Extract along the track -------------------------------------------------

  out_dataframe <- create_out_dataframe(parameter, dimargs, working_coords, xcoord, tcoord)

  # one extract per dataset time period,  or a single extract if no time
  outer_loop <- if (has_time) unique_req_time_index else 1L
  pb <- NULL
  if (isTRUE(progress_bar)) {
    pb <- utils::txtProgressBar(min = 0, max = length(outer_loop), style = 3)
  }
  i_pb <- 0
  last_ipos <- 0L
  for (iloop in outer_loop) {
    if (!is.null(pb)) {
      i_pb <- i_pb + 1
      utils::setTxtProgressBar(pb, i_pb)
    }

    # the track locations that use this extract
    track_index <- if (has_time) which(req_time_index == iloop) else seq_along(xcoord)

    # define bounding box
    bbox <- define_bbox(track_index, working_coords, xrad, yrad, zrad)
    erddapList <- findERDDAPcoord(dataCoordList, isoTime, udtTime,
                                  c(bbox$xmin, bbox$xmax), c(bbox$ymin, bbox$ymax),
                                  c(udtTime[iloop],udtTime[iloop]), c(bbox$zmin, bbox$zmax),
                                  xName, yName, tName, zName, cross_dateline_180)
    erddapCoords <- erddapList$erddapCoords
    requesttime <- erddapCoords$erddapTcoord[1]

    # cross_dateline_180 tests if the request ever crosses dateline.
    # This box crosses only if its west limit maps east of its east limit
    # once put on (-180, 180).
    crosses_dateline <- cross_dateline_180 &&
      (erddapCoords$erddapXcoord[1] > erddapCoords$erddapXcoord[2])
    extract <- read_extract(dataInfo, callDims, urlbase,
                            xName, yName, zName, tName, parameter,
                            erddapCoords, dataCoordList, crosses_dateline,
                            verbose)
    if (!is.list(extract)) {
      out_dataframe <- write_extract_error(out_dataframe, last_ipos)
      break
    }

    # done extract now loop over the appropriate locations
    # populate the dataframe
    for (ipos in track_index) {
      xmin <- xcoord[ipos] - (xrad[ipos]/2)
      xmax <- xcoord[ipos] + (xrad[ipos]/2)
      ymin <- ycoord[ipos] - (yrad[ipos]/2)
      ymax <- ycoord[ipos] + (yrad[ipos]/2)
      zmin <- NA
      zmax <- NA
      if (has_z) {
        zmin <- zcoord[ipos] - (zrad[ipos]/2)
        zmax <- zcoord[ipos] + (zrad[ipos]/2)
      }
      param <- subset_track_point(extract, xName, yName, zName,
                                  xmin, xmax, ymin, ymax, zmin, zmax,
                                  has_z, has_time)
      out_dataframe <- populate_dataframe(out_dataframe, ipos, requesttime, working_coords, param,
                                     xmin, xmax, ymin, ymax, zmin, zmax)
    }
    last_ipos <- max(last_ipos, track_index)
  }

  # done loops, give the dataframe needed info
  out_dataframe <- structure(out_dataframe,
                             class = c('list', 'rxtractoTrack'),
                             base_url = urlbase,
                             datasetid = attributes(dataInfo)$datasetid
                             )
  # close progress_bar if there is one
  if (!is.null(pb)) {
    close(pb)
  }
  return(out_dataframe)
}

# Check that the track coordinates and "radius" inputs have consistent lengths
#
# "check_track_lengths()" stops with a message if they do not
#
# callDims - named list of the requested coordinates
# xcoord, ycoord, zcoord - the requested coordinates
# xlen, ylen, zlen - the requested "radius" around each location
check_track_lengths <- function(callDims, xcoord, ycoord, zcoord, xlen, ylen, zlen) {
  # Check that the non-null input vectors are the same length
  dimLengths <- lengths(callDims)
  dimLengths <- dimLengths[dimLengths > 0]
  if (length(unique(dimLengths)) > 1) {
    print("The length of the non-empty coordinates do not agree")
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
  if ( (!is.null(zcoord)) && (length(zlen) > 1) && (length(zlen) != length(zcoord))) {
    stop('zlen is of size greater than one but not equal to length of zcoord')
  }
  if ( (!is.null(zcoord)) && length(xlen)  > 1) {
    if (length(zlen) == 1)  {
      message('warning - zlen has a single value')
      message('xlen and ylen have length greater than 1')
      message('zlen will be extended to be same length with repeated value')
    }
  }
  invisible(NULL)
}

# read the data for one bounding box
#
# "read_extract()" makes one ERDDAP™ request,  or two if the box crosses the
# dateline on a (-180, 180) dataset,  and combines the two results
#
# erddapCoords - the ERDDAP™ coordinate limits from findERDDAPcoord()
# dataCoordList - the dataset coordinate values
# crosses_dateline - logical,  does this box cross the dateline
# return - list with the extract,  or -1 if a request failed
read_extract <- function(dataInfo, callDims, urlbase,
                         xName, yName, zName, tName, parameter,
                         erddapCoords, dataCoordList, crosses_dateline,
                         verbose, cache_remove = TRUE) {
  read_lon_range <- function(lon_limits) {
    data_extract_read(dataInfo, callDims, urlbase,
                      xName, yName, zName, tName, parameter,
                      lon_limits, erddapCoords$erddapYcoord,
                      erddapCoords$erddapTcoord, erddapCoords$erddapZcoord,
                      verbose, cache_remove)
  }
  if (!crosses_dateline) {
    return(read_lon_range(erddapCoords$erddapXcoord))
  }
  # extract to the west of the dateline
  # fuzz the longitudes to be a little away from the dateline
  extract1 <- read_lon_range(c(erddapCoords$erddapXcoord[1],
                               max(dataCoordList$longitude) - 0.0001))
  if (!is.list(extract1)) {
    return(extract1)
  }
  # extract east of the dateline
  extract2 <- read_lon_range(c(min(dataCoordList$longitude) + 0.0001,
                               erddapCoords$erddapXcoord[2]))
  if (!is.list(extract2)) {
    return(extract2)
  }
  combine_extracts(extract1, extract2)
}

# combine extracts when crossing the dateline
#
# "combine_extracts()" combines the two extracts made when crossing the dateline.
# Longitude is the first dimension of the data array,  which can have
# 2 to 4 dimensions depending on if there is a z-coordinate and a time coordinate
# If the dataset has grid points at both -180 and 180 (e.g. etopo180),  both
# extracts contain the 180 column,  so the duplicate in extract2 is dropped
#
# extract1 - extract on one side of the dateline
# extract2 - extract on other side of the dataline
# return - list with combined extract
combine_extracts <- function(extract1, extract2) {
  dim1 <- dim(extract1[[1]])
  # select everything in the other dimensions
  other_dims <- rep(list(TRUE), length(dim1) - 1)
  lon1 <- extract1$longitude
  lon2 <- make360(extract2$longitude)
  data2 <- extract2[[1]]
  if (isTRUE(abs(lon1[length(lon1)] - lon2[1]) < 1e-6)) {
    keep <- seq_along(lon2)[-1]
    lon2 <- lon2[keep]
    data2 <- do.call(`[`, c(list(data2), list(keep), other_dims, list(drop = FALSE)))
  }
  lon_len1 <- length(lon1)
  lon_len2 <- length(lon2)
  combined <- array(NA_real_, dim = c(lon_len1 + lon_len2, dim1[-1]))
  combined <- do.call(`[<-`, c(list(combined), list(seq_len(lon_len1)), other_dims,
                               list(value = extract1[[1]])))
  combined <- do.call(`[<-`, c(list(combined), list(lon_len1 + seq_len(lon_len2)), other_dims,
                               list(value = data2)))
  extract <- extract1
  extract[[1]] <- combined
  extract$longitude <- c(lon1, lon2)
  return(extract)
}

# get the data for one track location out of the extract
#
# "subset_track_point()" finds the part of the extract closest to the requested
# limits.  The data array is (x, y, [z], [time]),  and there is only one time
# in an extract
#
# return - the data values within the limits
subset_track_point <- function(extract, xName, yName, zName,
                               xmin, xmax, ymin, ymax, zmin, zmax,
                               has_z, has_time) {
  nearest_range <- function(values, lower, upper) {
    which.min(abs(values - lower)):which.min(abs(values - upper))
  }
  index <- list(nearest_range(extract[[xName]], xmin, xmax),
                nearest_range(extract[[yName]], ymin, ymax))
  if (has_z) {
    index <- c(index, list(nearest_range(extract[[zName]], zmin, zmax)))
  }
  if (has_time) {
    index <- c(index, list(1))
  }
  do.call(`[`, c(list(extract[[1]]), index))
}

# create the data frame to store the results along the track
#
# "create_out_dataframe()" creates the data frame to be populated as
# "rxtracto()" extracts data along the track
#
# parameter - the name of the parameter being extracted along the track
# dimargs - the coordinate dimensions of the extract
# working_coords - the coordinates from ERDDAP™
# xcoord - the vector of requested xcoord values
# tcoord - the vector of time coordinate if time is a coordinate of the dataset
# return - dataframe
create_out_dataframe <- function(parameter, dimargs, working_coords, xcoord, tcoord) {
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
  return(out_dataframe)
}

# Print error message when an extract failed and trim the output
#
# 'write_extract_error' is an internal function that reports an error in the
# outer loop.  Rows are kept up through the furthest position filled so far.
# If the track times are not in order,  rows inside that range whose time
# period was not reached are NA.
#
#  out_dataframe - output dataframe up to the point of the error
#  last_ipos - furthest position along the track with a successful extract
#  return out_dataframe
write_extract_error <- function(out_dataframe, last_ipos) {
  text1 <- "There was an error in the url call, perhaps a time out."
  text2 <- "See message on screen and URL called"
  print(paste(text1, text2))
  print("Returning incomplete download")
  suppressWarnings(try(rerddap::cache_delete_all(), silent = TRUE))
  return(out_dataframe[seq_len(last_ipos), ])
}

# populate the dataframe with the most recent extract along the track
populate_dataframe <- function(out_dataframe, ipos, requesttime, working_coords, param,
                               xmin, xmax, ymin, ymax, zmin, zmax){
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
  out_dataframe[ipos, 12] <- stats::median(param, na.rm = TRUE)
  out_dataframe[ipos, 13] <- stats::mad(param, na.rm = TRUE)
  return(out_dataframe)
}

# define the bounding box for all track locations that have the same time in the ERDDAP™ dataset
#
# track_index - the track locations that use this extract
define_bbox <- function(track_index, working_coords, xrad, yrad, zrad) {
  time_xpos <- working_coords$xcoord1[track_index]
  time_ypos <- working_coords$ycoord1[track_index]
  time_xrad <- max(xrad[track_index])
  time_yrad <- max(yrad[track_index])
  xmin <- min(time_xpos) - (time_xrad/2)
  xmax <- max(time_xpos) + (time_xrad/2)
  ymin <- min(time_ypos) - (time_yrad/2)
  ymax <- max(time_ypos) + (time_yrad/2)

  zmin <- NA
  zmax <- NA
  if (!is.null(working_coords$zcoord1)) {
    time_zpos <- working_coords$zcoord1[track_index]
    time_zrad <- max(zrad[track_index])
    zmin <- min(time_zpos) - (time_zrad/2)
    zmax <- max(time_zpos) + (time_zrad/2)
  }
  return(list(xmin = xmin, xmax = xmax,
              ymin = ymin, ymax = ymax,
              zmin = zmin, zmax = zmax)
  )
}
