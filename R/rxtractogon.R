#' Extract environmental data in a polygon using ERDDAP.
#'
#' \code{rxtractogon} uses the ERD ERDDAP data web service to extact
#' environmental data inside a polygon defined by vectors of
#' latitudes and longitudes
#' @export
#' @param dataInfo - the return from an rerddap "info" call to an ERDDAP server
#' @param parameter - character string containing the name of the parameter to extract
#' @param xcoord - array giving longitudes (in decimal
#'   degrees East, either 0-360 or -180 to 180) of polygon
#' @param ycoord -  array giving latitudes (in decimal
#'   degrees N; -90 to 90)of polygon
#' @param tcoord - 2-array of minimum and maximum times as 'YYYY-MM-DD'
#' @param zcoord -  a real number with the z-coordinate(usually altitude or depth)
#' @param xName - character string with name of the xcoord in the ERDDAP dataset (default "longitude")
#' @param yName - character string with name of the ycoord in the ERDDAP dataset (default "latitude")
#' @param zName - character string with name of the zcoord in the ERDDAP dataset (default "altitude")
#' @param tName - character string with name of the tcoord in the ERDDAP dataset (default "time")
#' @param urlbase - base URL of the ERDDAP server being accessed - default "http://upwell.pfeg.noaa.gov/erddap"
#' @param verbose - logical variable (default FALSE) if the the URL request should be verbose
#' @return structure with data and dimensions
#' \itemize{
#'   \item extract$data - the masked data array dimensioned (lon,lat,time)
#'   \item extract$varname - the name of the parameter extracted
#'   \item extract$datasetname - ERDDAP dataset name
#'   \item extract$longitude - the longitudes on some scale as request
#'   \item extract$latitude - the latitudes always going south to north
#'   \item extract$time - the times of the extracts
#'   }
#' @examples
#' dataInfo <- rerddap::info('erdVH3chlamday')
#' parameter <- 'chla'
#' tcoord <- c("2016-06-15", "2016-06-15")
#' xcoord <- mbnms$Longitude
#' ycoord <- mbnms$Latitude
#' sanctchl <- rxtractogon (dataInfo, parameter, xcoord=xcoord, ycoord=ycoord,  tcoord=tcoord)
#' xcoord <- mbnms$Longitude
#' ycoord <- mbnms$Latitude
#' dataInfo <- rerddap::info('etopo180')
#' parameter = 'altitude'
#' xName <- 'longitude'
#' yName <- 'latitude'
#' bathy <- rxtractogon (dataInfo, parameter, xcoord=xcoord, ycoord=ycoord)
#' @section Details:
#'  xtractogon extracts the data from the smallest bounding box that contains
#'  the polygon, and then uses the function "point.in.polygon" from the "sp"
#'  package to mask out the areas outside of the polygon.





rxtractogon <- function(dataInfo, parameter, xcoord=NULL, ycoord=NULL, zcoord = NULL, tcoord=NULL, xName='longitude', yName='latitude', zName='altitude', tName='time', urlbase='http://upwell.pfeg.noaa.gov/erddap', verbose=FALSE) {


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

if (length(xcoord) != length(ycoord)) {
  print('xcoord and ycoord are not of the same length')
  stop('program stops')
}

#extend out tpos to be length 2 if not
tcoord1 <- tcoord
if (length(tcoord1) == 1) {
  tcoord1 <- rep(tcoord1, 2)
}
mypoly <- data.frame(xcoord, ycoord)
colnames(mypoly) <- c('x', 'y')
xcoord1 <- c(min(xcoord), max(xcoord))
ycoord1 <- c(min(ycoord), max(ycoord))

# call xtracto to get data
extract <-  xtracto_3D(dataInfo, parameter, xcoord=xcoord1, ycoord=ycoord1, zcoord=zcoord, tcoord=tcoord1, xName=xName, yName=yName, zName=zName, urlbase=urlbase, verbose=verbose)
#	extract <- xtracto_3D(xcoord1,ycoord1,tpos1,dtype, verbose)
str(extract)
if(length(dim(extract[[1]]))==2){
   extract[[1]]<-array(extract[[1]],c(dim(extract[[1]]),1))
  }



# make sure polygon is closed; if not, close it.
	if ((mypoly[length(mypoly[, 1]), 1] != mypoly[1, 1]) | (mypoly[length(mypoly[, 2]), 2] != mypoly[1, 2])) {
		mypoly <- rbind(mypoly, c(mypoly[1, 1], mypoly[1, 2]))
	}

#Parse grid lats and longs
#    x.vals <- matrix(rep(as.numeric(substr(dimnames(extract)[[1]], 1, nchar(dimnames(extract)[[1]])-1)),length(dimnames(extract)[[2]])), ncol = length(dimnames(extract)[[2]]))
#    y.vals <- matrix(sort(rep(as.numeric(substr(dimnames(extract)[[2]], 1, nchar(dimnames(extract)[[2]])-1)),length(dimnames(extract)[[1]]))), ncol = length(dimnames(extract)[[1]]))
x.vals <- matrix(rep(extract$longitude, length(extract$latitude)), ncol=length(extract$latitude))
y.vals <- sort(rep(extract$latitude, length(extract$longitude)))
y.vals <- matrix(y.vals, nrow=length(extract$latitude), ncol=length(extract$longitude))
# deal with polygon crossing 180
ew.sign <- sign(mypoly$x)
if (length(unique(ew.sign)) > 1) {
  mypoly$x[mypoly$x < 0] <- mypoly$x[poly$x < 0] + 360
  x.vals[x.vals < 0] <- x.vals[x.vals < 0] + 360
  print("Polygon data cross 180. Converted to E longitudes")
}

# create new array masked by polygon
#     in.poly <- matrix(point.in.polygon(x.vals, y.vals, poly$x, poly$y), ncol = length(dimnames(extract)[[1]]))
in.poly <- matrix(sp::point.in.polygon(x.vals, y.vals, mypoly$x, mypoly$y), ncol = length(extract$longitude))
in.poly[in.poly > 1] <- 1
in.poly[in.poly == 0] <- NA
dim(in.poly) <- dim(extract[[1]][,,1])
extract.in.poly <- apply(extract[[1]], 3, "*", in.poly)
dim(extract.in.poly) <- dim(extract[[1]])
extract[[1]]<-extract.in.poly

return(extract)
}



