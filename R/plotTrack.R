#' plot result of xtracto or rxtracto
#'
#' \code{plotTrack} is a function to plot the results from
#' rxtracto() and xtracto()
#' @export
#' @param xcoord passed to rxtracto() or xtracto()
#' @param ycoord passed to rxtracto() or xtracto()
#' @param resp data frame returned from rxtracto() or xtracto()
#' @param plotColor the color to use in plot from rerddap
#' @param name name for colorbar label
#' @param myFunc function of one argument to transform the data
#' @param shape shape to use to mark track
#' @param size size of shape to use to mark track
#' @return a plotdap plot
#'
#' @examples
#' tagData <- Marlintag38606
#' xpos <- tagData$lon
#' ypos <- tagData$lat
#' tpos <- tagData$date
#' zpos <- rep(0., length(xpos))
#' urlbase <- 'http://upwell.pfeg.noaa.gov/erddap'
#' swchlInfo <- rerddap::info('erdSWchla8day')
#' swchl <- rxtracto(swchlInfo, parameter = 'chlorophyll', xcoord = xpos, ycoord = ypos, tcoord = tpos, zcoord = zpos, xlen = .2, ylen = .2)
#' plotTrack(swchl, xpos, ypos, plotColor = 'chlorophyll')

plotTrack <- function(resp, xcoord, ycoord,  plotColor = 'viridis', name = NA, myFunc = NA, shape = 20, size = .5){
  require(rerddap)
  require(plotdap)
  ind <- which(xcoord > 180)
  xcoord[ind] <- xcoord[ind] - 360
  if (is.function(myFunc)) {
    resp[[1]] <- myFunc(resp[[1]])
  }
  myDataFrame = data.frame(xcoord, ycoord, resp[[1]])
  nameLen <- nchar(names(resp))
  if (is.na(name)) {
    paramName <-  substr(names(resp)[1], 6, nameLen)
  }else{
    paramName = name
  }
  names(myDataFrame) <- c('longitude', 'latitude', paramName)
myStruct <- structure(
    myDataFrame,
    class = c("tabledap", "data.frame")
  )
p <- plotdap::plotdap()
paramName1 <- as.formula(paste('~', paramName))
myList <- list(p, myStruct, paramName1, plotColor, shape, size)
names(myList) <- c('plot', 'table', 'var', 'color', 'shape', 'size')
#plotCmd <-  paste0('add_tabledap(plotdap(), myStruct, ~',  paramName,
#  ', color = ', deparse(plotColor), ', shape =20, size= .5)')
#myPlot <- eval(parse(text = plotCmd))
myPlot <- do.call(plotdap::add_tabledap, myList)
myPlot
}
