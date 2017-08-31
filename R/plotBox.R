#' plot result of xtracto_3D or rxtracto_3D
#'
#' \code{plotBox} is a function to plot the results from
#' rxtracto() and xtracto()
#'
#' @export
#' @param resp data frame returned from rxtracto() or xtracto()
#' @param plotColor the color to use in plot from rerddap
#' @param time a function to map multi-time to one,  or else identity for animation
#' @param animate if multiple times, if TRUE will animate the maps
#' @param myFunc function of one argument to transform the data
#' @param maxpixels maximum numbe rof pixels to use in making the map - controls resolution
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
#' plotBox(xpos, ypos, swchl, plotColor = 'chlorophyll')

plotBox <- function(resp, plotColor = 'viridis', time = NA, animate = FALSE, name = NA, myFunc = NA, maxpixels = 10000){
  require(rerddap)
  require(plotdap)
  if (!is.function(time)) {
    time <- function(x) mean(x, na.rm = TRUE)
  }
  if (is.function(myFunc)) {
    resp[[1]] <- myFunc(resp[[1]])
  }
  if (!is.na(name)) {
    names(resp)[1] <- name
  }
  paramName = names(resp)[1]
  myStruct <- meltnc(resp)
  myStruct <- structure(
    myStruct,
    class = c("griddap_nc", "nc", "data.frame")
  )
  p <- plotdap::plotdap()
  parameter1 <- as.formula(paste('~', paramName))
  myList <- list(p, myStruct, parameter1, plotColor, time, animate, maxpixels )
  names(myList) <- c('plot', 'grid', 'var', 'fill', 'time', 'animate', 'maxpixels')
  myplot <- do.call(plotdap::add_griddap, myList)
  myplot
}

meltnc <- function(resp ){
  ##  modified from rerddap::ncdf4_get
  rows = length(resp[[1]])
  if (is.null(resp$time)) {
    exout <- do.call("expand.grid", list(longitude = resp$longtiude, latitude = resp$latitude))
    meta <- dplyr::arrange_(exout, names(exout)[1])
  } else {
    time <- as.character(resp$time)
    time <- suppressWarnings(rep(time, each = rows/length(resp$time)))
    lat <- rep(rep(resp$latitude, each = length(resp$longitude)),
               length(resp$time))
    lon <- rep(rep(resp$longitude, times = length(resp$latitude)),
               times = length(resp$time))
    meta <- data.frame(time, lat, lon, stringsAsFactors = FALSE)
  }

  # make data.frame
  df <- as.vector(resp[[1]])
  df <- data.frame(df)
  names(df) <- names(resp)[1]
  alldf <- if (NROW(meta) > 0) cbind(meta, df) else df

  #  Fool plotdap that there is a summary
  summary_time = list(vals = as.numeric(resp$time))
  summary_lons <- list(vals = resp$longitude)
  summary_lats <- list(vals = resp$latitude)
  dims <- list(time = summary_time, longitude = summary_lons, latitude = summary_lats)
  summary <- list(dims)
  names(summary) <- 'dim'
  # output
  list(summary = summary, data = alldf)
}

