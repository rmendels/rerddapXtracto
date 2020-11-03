#' plot result of 'rxtracto'
#'
#' \code{plotTrack} is a function to plot the results from
#' 'rxtracto()'
#' @export
#' @param xcoord passed to 'rxtracto()'
#' @param ycoord passed to 'rxtracto()'
#' @param tcoord passed to 'rxtracto()'
#' @param resp data frame returned from 'rxtracto()'
#' @param plotColor the color to use in plot from 'cmocean'
#' @param myFunc function of one argument to transform the data
#' @param mapData map data from 'maps' or 'mapdata', must be of class 'map'
#' @param crs valid crs string
#' @param animate if multiple times, if TRUE will animate the maps
#' @param cumulative makes cumulative animation of data
#' @param name name for colorbar label
#' @param shape shape to use to mark track
#' @param size size of shape to use to mark track
#' @return a 'plotdap' plot
#'
#' @examples
#' tagData <- Marlintag38606
#' xpos <- tagData$lon[1:20]
#' ypos <- tagData$lat[1:20]
#' tpos <- tagData$date[1:20]
#' zpos <- rep(0., length(xpos))
#' \donttest{
#' swchlInfo <- rerddap::info('erdSWchla8day')
#' swchl <- rxtracto(swchlInfo, parameter = 'chlorophyll', xcoord = xpos,
#'         ycoord = ypos, tcoord = tpos, zcoord = zpos, xlen = .2, ylen = .2)
#' }
#' suppressWarnings(p <- plotTrack(swchl, xpos, ypos, tpos, plotColor = 'algae'))

plotTrack <- function(resp, xcoord, ycoord, tcoord, plotColor = 'viridis', myFunc = NA,
                      mapData = NULL, crs = NULL,
                      animate = FALSE, cumulative = FALSE,
                      name = NA,  shape = 20, size = .5){

  # check that the response is of the right class
   if (!('rxtractoTrack' %in% class(resp))) {
   print('given extract is not valid rxtracto() output')
   print("class of the response is not' 'rxtractoTrack' ")
   stop('execution halted')
 }
  # check that if a crs string is given,  that it is a valid string
  if (!is.null(crs)) {
    crs_test <- inherits(try(sf::st_crs(crs), silent = TRUE), "try-error")
    if (crs_test) {
      stop('crs is given but is not a valid crs string')
    }
  }
  # check that if outline is given,  it is of class maps
  if (!is.null(mapData)) {
    if (!("map" == class(mapData))) {
      stop('map outline given but not of class "maps" ')
    }
  }
# default maps ar eon (-180, 180).  If we don't cross dateline will
# transform longitudes
  cross_180 <- FALSE
  if ((min(xcoord) < 180.) && (max(xcoord) > 180.)) { cross_180 <- TRUE}
  if (!cross_180) {
    ind <- which(xcoord > 180)
    xcoord[ind] <- xcoord[ind] - 360
   }

  if (is.function(myFunc)) {
    resp[[1]] <- myFunc(resp[[1]])
  }
  myDataFrame <- data.frame(xcoord, ycoord, tcoord, resp[[1]])
  nameLen <- nchar(names(resp))
  if (is.na(name)) {
    paramName <-  substr(names(resp)[1], 6, nameLen)
  }else{
    paramName <- name
  }
  names(myDataFrame) <- c('longitude', 'latitude', 'time', paramName)
myStruct <- structure(
    myDataFrame,
    class = c("tabledap", "data.frame")
  )
# set up the plotdap parts an call plotdap
if (is.null(mapData)) {
  mapData <- maps::map("world", plot = FALSE, fill = TRUE)
}
plotdap_list <- list(mapData = mapData, crs = crs)
#p <- plotdap::plotdap()
p <- do.call(plotdap::plotdap, plotdap_list)
paramName1 <- stats::as.formula(paste('~', paramName))
myList <- list(p, myStruct, paramName1, plotColor,
               animate, cumulative, shape, size)
names(myList) <- c('plot', 'table', 'var', 'color', 'animate',
                   'cumulative', 'shape', 'size')
#plotCmd <-  paste0('add_tabledap(plotdap(), myStruct, ~',  paramName,
#  ', color = ', deparse(plotColor), ', shape =20, size= .5)')
#myPlot <- eval(parse(text = plotCmd))
myPlot <- do.call(plotdap::add_tabledap, myList)
if (animate) {
  xlim <- c(min(xcoord), max(xcoord))
  ylim <- c(min(ycoord), max(ycoord))
  suppressMessages(myPlot <- plotdap::add_ggplot(
    myPlot, ggplot2::coord_sf(
      crs = myPlot$crs, datum = myPlot$datum,
      xlim = xlim, ylim = ylim
    )
  ))

}

myPlot
}
