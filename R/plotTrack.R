#' plot result of 'rxtracto'
#'
#' \code{plotTrack} is a function to plot the results from
#' 'rxtracto()'
#' @export
#' @param xcoord passed to 'rxtracto()'
#' @param ycoord passed to 'rxtracto()'
#' @param tcoord passed to 'rxtracto()'
#' @param resp data frame returned from 'rxtracto()'
#' @param plotColor the color to use in plot from 'rerddap'
#' @param animate if multiple times, if TRUE will animate the maps
#' @param cumulative makes cumulative animation of data
#' @param name name for colorbar label
#' @param myFunc function of one argument to transform the data
#' @param shape shape to use to mark track
#' @param size size of shape to use to mark track
#' @return a 'plotdap' plot
#'
#' @examples
#' tagData <- Marlintag38606
#' xpos <- tagData$lon[1:20]
#' ypos <- tagData$lat[1:20]
#' tpos <- tagData$date[1:20]
#' tpos <- tagData$date[1:20]
#' zpos <- rep(0., length(xpos))
#' \donttest{
#' swchlInfo <- rerddap::info('erdSWchla8day')
#' swchl <- rxtracto(swchlInfo, parameter = 'chlorophyll', xcoord = xpos,
#'         ycoord = ypos, tcoord = tpos, zcoord = zpos, xlen = .2, ylen = .2)
#' }
#' p <- plotTrack(swchl, xpos, ypos, tpos, plotColor = 'chlorophyll')

plotTrack <- function(resp, xcoord, ycoord, tcoord, plotColor = 'viridis',
                      animate = FALSE, cumulative = FALSE,
                      name = NA, myFunc = NA, shape = 20, size = .5){

  # check that the response is of the right class
   if (!('rxtractoTrack' %in% class(resp))) {
   print('given extract is not valid rxtracto() output')
   print("class of the response is not' 'rxtractoTrack' ")
   stop('execution halted')
 }
  #  require(rerddap)
#  require(plotdap)
  ind <- which(xcoord > 180)
  xcoord[ind] <- xcoord[ind] - 360
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
p <- plotdap::plotdap()
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
