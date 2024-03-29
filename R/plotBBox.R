#' plot result of 'rxtracto_3D'
#'
#' \code{plotBBox} is a function to plot the results from
#' 'rxtracto_3D'()' and 'rxtractogon()'
#'
#' @export
#' @param resp data frame returned from 'rxtracto_3D'()' or 'rxtractogon()'
#' @param plotColor the color to use in plot from 'cmocean'
#' @param time a function to map multi-time to one, or else identity
#'  for animation
#' @param myFunc function of one argument to transform the data
#' @param mapData map data from 'maps' or 'mapdata', must be of class 'map'
#' @param crs valid crs string
#' @param animate if multiple times, if TRUE will animate the maps
#' @param cumulative makes cumulative animation of data
#' @param name name for colorbar label
#' @param maxpixels maximum number of pixels to use in making the map
#'  - controls resolution
#' @return a 'plotdap' plot
#'
#' @examples
#' ## example code to download data for plotBBox
#' ## dataInfo <- rerddap::info('erdMBsstd1day')
#' ## parameter <- 'sst'
#' ## xcoord <- c(230, 230.1)
#' ## ycoord <- c(33, 33.1)
#' ## tcoord <- c('2006-01-15', '2006-01-15')
#' ## zcoord <- c(0., 0.)
#' ## MBsst <- rxtracto_3D(dataInfo, parameter, xcoord = xcoord, ycoord = ycoord,
#' ##                      tcoord = tcoord, zcoord = zcoord)
#' ##
#' ## low resolution selected to keep time to render down
#' # suppressWarnings(p <- plotBBox(MBsst, maxpixels = 50))

plotBBox <- function(resp, plotColor = 'viridis', time = NA, myFunc = NA,
                mapData = NULL, crs = NULL,
                animate = FALSE, cumulative = FALSE, name = NA,
                maxpixels = 10000) {


  # check that the response is of the right class
  if (!('rxtracto3D' %in% class(resp))) {
    print('given extract is not valid rxtracto_3D() output')
    print("class of the response is not' 'rxtracto3D' ")
    stop('execution halted')
  }
    #  check that lat-lon exist for mapping
  if (!(('longitude' %in% names(resp)) && ('latitude' %in% names(resp)))) {
    print('longitude and latitude must be present in the response')
    print(paste('variables in extract:  ', names(resp)))
    stop('execution halted')
  }

  # check that time is POSIXlt
  if (!(is.na(resp$time[1]))) {
    if (!(methods::is(resp$time, "POSIXlt"))) {
      stop('time is a variable but is not POSIXlt')
    }
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
     stop('map outline given but not of class "map" ')
   }
 }
    # if time s not given as a function,  set it to the default
  if (!is.function(time)) {
    time <- function(x) mean(x, na.rm = TRUE)
  }
  # if time is a function,  apply that function to the data
  if (is.function(myFunc)) {
    resp[[1]] <- myFunc(resp[[1]])
  }
  # get the parameter name of the data
  paramName <- names(resp)[1]
  # "melt" the data into an rerddap type dataframe
  myStruct <- meltnc(resp)
  # add the class types
  myStruct <- structure(
    myStruct,
    class = c("griddap_nc", "nc", "data.frame")
  )
  # set up the plotdap parts an call plotdap
  if (is.null(mapData)) {
    mapData <- maps::map("world", plot = FALSE, fill = TRUE)
  }
  plotdap_list <- list(mapData = mapData, crs = crs)
  #p <- plotdap::plotdap()
  p <- do.call(plotdap::plotdap, plotdap_list)
  parameter1 <- stats::as.formula(paste('~', paramName))
  myList <- list(p, myStruct, parameter1, plotColor, time, animate, cumulative,
                 maxpixels )
  names(myList) <- c('plot', 'grid', 'var', 'fill', 'time',
                     'animate', 'cumulative', 'maxpixels')
  myplot <- do.call(plotdap::add_griddap, myList)
  # if a name os given,  change the colorbar label
  if (!is.na(name)) {
    myplot <- plotdap::add_ggplot(myplot,
              ggplot2::guides(fill = ggplot2::guide_colourbar(title = name)),
              ggplot2::guides(colour = "none"))
  }
  if (animate) {
    xlim <- c(min(myStruct$data$lon), max(myStruct$data$lon))
    ylim <- c(min(myStruct$data$lat), max(myStruct$data$lat))
    suppressMessages(myplot <- plotdap::add_ggplot(
      myplot, ggplot2::coord_sf(
        crs = myplot$crs, datum = myplot$datum,
        xlim = xlim, ylim = ylim
      )
    ))

  }

  myplot
}

meltnc <- function(resp, xName = 'longitude', yName = 'latitude', tName = 'time'){
  ##  modified from rerddap::ncdf4_get
  rows <- length(resp[[1]])
  # if no time coordinate, use expand.grid to expand ut lat-lon grid
  if (is.null(resp$time)) {
    exout <- do.call("expand.grid", list(x = resp[[xName]],
                                         y = resp[[yName]]))
    names(exout) <- c(xName, yName)
     # meta <- dplyr::arrange_(exout, names(exout)[1])
    meta <- dplyr::arrange(exout, names(exout)[1])
  } else {
    # need to expand grid along all three dimensions to melt to data frame
    time <- as.character(resp[[tName]])
    time <- suppressWarnings(rep(time, each = rows/length(resp[[tName]])))
    y <- rep(rep(resp[[yName]], each = length(resp[[xName]])),
               length(resp[[tName]]))
    x <- rep(rep(resp[[xName]], times = length(resp[[yName]])),
               times = length(resp[[tName]]))
    meta <- data.frame(time, x, y, stringsAsFactors = FALSE)
    names(meta) <- c(tName, xName, yName)
  }

  # make data.frame
  df <- as.vector(resp[[1]])
  df <- data.frame(df)
  names(df) <- names(resp)[1]
  alldf <- if (NROW(meta) > 0) cbind(meta, df) else df

  #  Fool plotdap that there is a summary
  summary_time <- list(vals = as.numeric(resp[[tName]]))
  summary_x <- list(vals = resp[[xName]])
  summary_y <- list(vals = resp[[yName]])
  dims <- list(time = summary_time, x = summary_x,
               y = summary_y)
  names(dims) <- c(tName, xName, yName)
  summary <- list(dims)
  names(summary) <- 'dim'
  # output a list that looks like an rerddap list
  list(summary = summary, data = alldf)
}

