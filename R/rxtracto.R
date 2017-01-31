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
#' @param zcoord -  a real number with the z-coordinate of the trajectory (usually altitude or depth)
#' @param tcoord - a character array with the times of the trajectory in
#'   "YYYY-MM-DD" - for now restricted to be time.
#' @param xlen - real array defining the longitude box around the given point (xlen/2 around the point)
#' @param ylen - real array defining the latitude box around the given point (tlen/2 around the point)
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
#' urlbase <- 'http://upwell.pfeg.noaa.gov/erddap'
#' dataInfo <- rerddap::info('erdMBsstd8day')
#' parameter <- 'sst'
#' xcoord <- c(230, 231)
#' ycoord <- c(40, 41)
#' tcoord <- c('2006-01-15', '2006-01-20')
#' zcoord <- 0.
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
#' zcoord <- 1
#' tcoord <- c('2016-09-02', '2016-09-03')
#' xlen <- 0
#' ylen <- 0
#' extract <- rxtracto(dataInfo, parameter, xcoord = xcoord, ycoord = ycoord,
#'                     zcoord = zcoord, tcoord = tcoord, xlen = xlen,
#'                     ylen = ylen, xName = xName, yName = yName, zName = zName)




rxtracto <- function(dataInfo, parameter = NULL, xcoord=NULL, ycoord = NULL, zcoord = NULL, tcoord = NULL, xlen = 0., ylen = 0., xName = 'longitude', yName = 'latitude', zName = 'altitude', tName = 'time', urlbase = 'http://upwell.pfeg.noaa.gov/erddap', verbose = FALSE) {

  #  check that a valid rerddap info structure is being passed
if (!(methods::is(dataInfo, "info"))) {
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
  allCoords <- dimvars(dataInfo)
  callDims <- list(xcoord, ycoord, zcoord, tcoord)
  names(callDims) <- c(xName, yName, zName, tName)
  callDims <- callDims[!sapply(callDims, is.null)]
  if (!(length(callDims) == length(allCoords))) {
    cat("You must give bounds for all of the coordinates of the grid")
    cat("Coordinates given: ", names(callDims))
    cat("Dataset Coordinates: ", dimvars(dataInfo))
    stop("execution halted", call. = FALSE)
  }

# check that the field given part of the dataset
  if (!(parameter %in% allvars)) {
    cat("Parameter given is not in dataset")
    cat("Parameter given: ", parameter)
    cat("Dataset Parameters: ", allvars[(length(allCoords) + 1):length(allvars)])
    stop("execution halted", call. = FALSE)
  }
  lenURL <- nchar(urlbase)
  if (substr(urlbase, lenURL, lenURL) == '/') {
    urlbase <- substr(urlbase, 1, (lenURL - 1))
  }

  #reconcile longitude grids
  #get dataset longitude range
  xcoord1 <- xcoord
  if (xName == 'longitude') {
    lonVal <- dataInfo$alldata$longitude[dataInfo$alldata$longitude$attribute_name == "actual_range", "value"]
    lonVal2 <- as.numeric(strtrim1(strsplit(lonVal, ",")[[1]]))
    #grid is -180, 180
    if (min(lonVal2) < 0.) {xcoord1 <- make180(xcoord1)}
    if (max(lonVal2) > 180.) {xcoord1 <- make360(xcoord1)}
  }

  # deal with xlen = constant v. vector
  if (length(xlen) == 1) {
    xrad <- c(rep(xlen, length(xcoord)))
    yrad <- c(rep(ylen, length(ycoord)))
  } else {
    xrad <- xlen
    yrad <- ylen
  }


if (!is.null(tcoord)) {
  udtpos <- parsedate::parse_date(as.character(tcoord))
  tcoordLim <- c(min(udtpos), max(udtpos))
}else{
  tcoordLim <- c(NULL, NULL)
}
 xcoordLim <- c(min(xcoord1 - (xrad/2)), max(xcoord1 + (xrad/2)))
 ycoordLim <- c(min(ycoord - (yrad/2)), max(ycoord + (yrad/2)))


#get dimension info
 dataCoordList <- getfileCoords(attr(dataInfo, "datasetid"), allCoords, urlbase)
if (length(dataCoordList) == 0) {
  stop("Error retrieving coordinate variable")
}

 if (xName %in% names(dataCoordList)) {
   xNameIndex <- which(names(dataCoordList) == xName)
 }
 if (yName %in% names(dataCoordList)) {
   yNameIndex <- which(names(dataCoordList) == yName)
  }
 if (zName %in% names(dataCoordList)) {
   zNameIndex <- which(names(dataCoordList) == zName)
 }

 if (tName %in% names(dataCoordList)) {
   tNameIndex <- which(names(dataCoordList) == tName)
   isotime <- dataCoordList[[tNameIndex]]
   udtime <- parsedate::parse_date(isotime)
 }

 dimargs <- list(xcoordLim, ycoordLim, zcoord, tcoordLim)
 names(dimargs) <- c(xName, yName, zName, tName)
 checkBounds(dataCoordList, dimargs)
 # if (result != 0) {
 # stop("Coordinates out of dataset bounds - see messages above")
 # }

#create structures to store last request in case it is the same
  out.dataframe <- as.data.frame(matrix(ncol = 11,nrow = length(xcoord)))
 if (("latitude" %in% names(dimargs))  & ("longitude" %in% names(dimargs))) {
  dimnames(out.dataframe)[[2]] <- c(paste0('mean ', parameter), paste0('stdev ', parameter), 'n', 'satellite date', 'requested lon min',      'requested lon max', 'requested lat min', 'requested lat max', 'requested date', paste0('median ', parameter), paste0('mad ', parameter))
} else{
  dimnames(out.dataframe)[[2]] <- c(paste0('mean ', parameter), paste0('stdev ', parameter), 'n', 'satellite date', 'requested x min',  'requested x max', 'requested y min', 'requested y max', 'requested date', paste0('median ', parameter), paste0('mad ', parameter))
}
oldxIndex <-  rep(NA_integer_, 2)
oldyIndex <-  rep(NA_integer_, 2)
oldTimeIndex <-  rep(NA_integer_, 2)
newxIndex <-  rep(NA_integer_, 2)
newyIndex <-  rep(NA_integer_, 2)
newTimeIndex <-  rep(NA_integer_, 2)
oldDataFrame <- as.data.frame(matrix(ncol = 11, nrow = 1))

latSouth <- (dataCoordList[[yNameIndex]][2] > dataCoordList[[yNameIndex]][1] )
if (zName %in% names(dataCoordList)) {
  newzIndex <-  which.min(abs(dataCoordList[[zNameIndex]] - zcoord))
  erddapZ <- rep(NA_real_, 2)
  erddapZ[1] <- dataCoordList[[zNameIndex]][newzIndex]
  erddapZ[2] <- erddapZ[1]
}

 for (i in 1:length(xcoord1)) {


# define bounding box
  xmax <- xcoord1[i] + (xrad[i]/2)
  xmin <- xcoord1[i] - (xrad[i]/2)
  if (latSouth) {
     ymax <- ycoord[i] + (yrad[i]/2)
     ymin <- ycoord[i] - (yrad[i]/2)
  } else {
     ymin <- ycoord[i] + (yrad[i]/2)
     ymax <- ycoord[i] - (yrad[i]/2)
  }


# find closest time of available data
#map request limits to nearest ERDDAP coordinates
   newyIndex[1] <- which.min(abs(dataCoordList[[yNameIndex]] - ymin))
   newyIndex[2] <- which.min(abs(dataCoordList[[yNameIndex]] - ymax))
   newxIndex[1] <- which.min(abs(dataCoordList[[xNameIndex]] - xmin))
   newxIndex[2] <- which.min(abs(dataCoordList[[xNameIndex]] - xmax))
   if (zName %in% names(dataCoordList)) {
       newzIndex <-  which.min(abs(dataCoordList[[zNameIndex]] - zcoord[i]))
   }
   if (tName %in% names(dataCoordList)) {
     newTimeIndex[1] <- which.min(abs(udtime - udtpos[i]))
    }

   if (identical(newyIndex, oldyIndex) && identical(newxIndex, oldxIndex) && identical(newTimeIndex[1], oldTimeIndex[1])) {
     # the call will be the same as last time, so no need to repeat
     out.dataframe[i,] <- oldDataFrame
   } else {
     erddapY <- rep(NA_real_, 2)
     erddapX <- rep(NA_real_, 2)
     erddapTimes <- rep(NA_real_, 2)
     erddapY[1] <- dataCoordList[[yNameIndex]][newyIndex[1]]
     erddapY[2] <- dataCoordList[[yNameIndex]][newyIndex[2]]
     erddapX[1] <- dataCoordList[[xNameIndex]][newxIndex[1]]
     erddapX[2] <- dataCoordList[[xNameIndex]][newxIndex[2]]
     if (tName %in% names(dataCoordList)) {
       requesttime <- isotime[newTimeIndex[1]]
       erddapTimes[1] <- requesttime
       erddapTimes[2] <- requesttime
      }

      extract <- list()
      myCallOpts <- ""
      if (!(urlbase == "http://upwell.pfeg.noaa.gov/erddap")) {
        myCallOpts <- paste0(", url='", urlbase,"/'")
      }
      if (verbose) {
        myCallOpts <- paste0(myCallOpts,",callopts = httr::verbose()")
      }
      griddapCmd <- 'rerddap::griddap(dataInfo,'
      if (!is.null(xcoord)) {
        griddapCmd <- paste0(griddapCmd, xName,'=c(',erddapX[1],',',erddapX[2],'),')
      }
      if (!is.null(ycoord)) {
        griddapCmd <- paste0(griddapCmd, yName,'=c(',erddapY[1],',',erddapY[2],'),')
      }
      if (!is.null(zcoord)) {
        griddapCmd <- paste0(griddapCmd, zName,'=c(',erddapZ[1],',',erddapZ[2],'),')
      }
      if (!is.null(tcoord)) {
        griddapCmd <- paste0(griddapCmd, tName,'=c("',erddapTimes[1],'","',erddapTimes[2],'"),')
      }

      griddapCmd <- paste0(griddapCmd,'fields="', parameter,'",read = FALSE', myCallOpts,')')
      extract <- eval(parse(text = griddapCmd))

    if (length(extract) == 0) {
       print(griddapCmd)
       stop("There was an error in the url call.  See message on screen and URL called")
        }



    #  put xmin, xmax on requesting scale
      if (xName == 'longitude') {
       if (max(xcoord) > 180.) {
         xmin <- make360(xmin)
         xmax <- make360(xmax)
       }
       #request is on (-180, 180)
       if (min(xcoord) < 0.) {
         xmin <- make180(xmin)
         xmax <- make180(xmax)
       }
     }

    if ((yName == 'latitude') & !latSouth) {
      tempY <- c(ymin, ymax)
      ymin <- min(tempY)
      ymax <- max(tempY)
    }
     ncFile <- ncdf4::nc_open(extract$summary$filename)
     paramdata <- ncdf4::ncvar_get(ncFile,parameter)
     ncdf4::nc_close(ncFile)
     out.dataframe[i, 1] <- mean(paramdata, na.rm = T)
     out.dataframe[i, 2] <- stats::sd(paramdata, na.rm = T)
     out.dataframe[i, 3] <- length(stats::na.omit(paramdata))
     if (tName %in% names(dataCoordList)) {
       out.dataframe[i, 4] <- requesttime
     }
     out.dataframe[i, 5] <- xmin
     out.dataframe[i, 6] <- xmax
     out.dataframe[i, 7] <- ymin
     out.dataframe[i, 8] <- ymax
     if (tName %in% names(dataCoordList)) {
       out.dataframe[i, 9] <- tcoord[i]
     }
     out.dataframe[i, 10] <- stats::median(paramdata, na.rm = T)
     out.dataframe[i, 11] <- stats::mad(paramdata, na.rm = T)

     remove('paramdata')
     rerddap::cache_delete(extract)
   }
   oldyIndex <- newyIndex
   oldxIndex <- newxIndex
   oldTimeIndex <- newTimeIndex
   oldDataFrame <- out.dataframe[i,]

}
return(out.dataframe)
}


