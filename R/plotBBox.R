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
#' @param contours whether to include contours in the plot.
#' @param contour_opts NULL (the default) or a named list of options passed
#'  to 'plotdap::add_griddap_contours()'.  Allowed names are 'n_breaks',
#'  'breaks', 'color', 'linewidth' and 'alpha'.  Options not given use the
#'  'plotdap::add_griddap_contours()' defaults.  Only used if contours = TRUE.
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
#' ## with contours
#' # suppressWarnings(p <- plotBBox(MBsst, maxpixels = 50, contours = TRUE,
#' #                  contour_opts = list(n_breaks = 5, color = 'white')))

plotBBox <- function(resp, plotColor = 'viridis', time = NA, myFunc = NA,
                mapData = NULL, crs = NULL,
                animate = FALSE, cumulative = FALSE, name = NA,
                maxpixels = 10000, contours = FALSE, contour_opts = NULL) {


  # check that the response is of the right class
  if (!inherits(resp, 'rxtracto3D')) {
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
   if (!inherits(mapData, 'map')) {
     stop('map outline given but not of class "map" ')
   }
 }
  # check the contour options
  contour_opts <- check_contour_opts(contours, contour_opts)
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
  # add contours if requested,  options not given use the plotdap defaults
  if (contours) {
    contour_list <- c(list(p = myplot, data = contour_data(myStruct$data),
                           var = paramName),
                      contour_opts)
    myplot <- do.call(plotdap::add_griddap_contours, contour_list)
  }
  # if a name os given,  change the colorbar label
  if (!is.na(name)) {
    myplot <- plotdap::add_ggplot(myplot,
              ggplot2::guides(fill = ggplot2::guide_colourbar(title = name)),
              ggplot2::guides(colour = "none"))
  }
  if (animate) {
    xlim <- c(min(myStruct$data$lon), max(myStruct$data$lon))
    ylim <- c(min(myStruct$data$lat), max(myStruct$data$lat))
    myplot <- suppressMessages(plotdap::add_ggplot(
      myplot, ggplot2::coord_sf(
        crs = myplot$crs, datum = myplot$datum,
        xlim = xlim, ylim = ylim
      )
    ))
  }

  myplot
}

# check the contour options
#
# "check_contour_opts()" checks that 'contour_opts' is NULL or a named list
# with only the allowed 'plotdap::add_griddap_contours()' options,  each given
# once.  Names must match exactly.
#
# contours - logical,  whether contours are requested
# contour_opts - NULL or named list of contour options
# return - list of the contour options (empty list if none given)
check_contour_opts <- function(contours, contour_opts) {
  allowed_opts <- c('n_breaks', 'breaks', 'color', 'linewidth', 'alpha')
  allowed_text <- paste('allowed names are:', paste(allowed_opts, collapse = ', '))
  if (!(is.logical(contours) && length(contours) == 1 && !is.na(contours))) {
    stop('contours must be TRUE or FALSE')
  }
  if (is.null(contour_opts)) {
    return(list())
  }
  if (!contours) {
    stop('contour_opts given but contours = FALSE')
  }
  if (!is.list(contour_opts)) {
    stop('contour_opts must be NULL or a named list,  ', allowed_text)
  }
  if (length(contour_opts) == 0) {
    return(list())
  }
  opt_names <- names(contour_opts)
  if (is.null(opt_names) || anyNA(opt_names) || any(opt_names == '')) {
    stop('all elements of contour_opts must be named,  ', allowed_text)
  }
  bad_names <- setdiff(opt_names, allowed_opts)
  if (length(bad_names) > 0) {
    stop('invalid contour_opts name(s): ', paste(bad_names, collapse = ', '),
         ',  ', allowed_text)
  }
  if (anyDuplicated(opt_names)) {
    stop('contour_opts name(s) given more than once: ',
         paste(unique(opt_names[duplicated(opt_names)]), collapse = ', '))
  }
  contour_opts
}

# put contour data on the same longitude scale as the plotted grid
#
# "contour_data()" copies the rule 'plotdap::add_griddap()' uses for the
# raster: if both edges of the grid (cell centers +/- half a cell) are in
# (180, 360) the longitudes are shifted by -360.  Otherwise the contours
# would be drawn on the other side of the world from the grid.
#
# grid_df - the "data" dataframe from meltnc()
# return - grid_df with longitude possibly shifted
contour_data <- function(grid_df, xName = 'longitude') {
  lons <- sort(unique(grid_df[[xName]]))
  half_cell <- if (length(lons) > 1) stats::median(diff(lons)) / 2 else 0
  lon_edges <- c(min(lons) - half_cell, max(lons) + half_cell)
  if (all(lon_edges >= 180 & lon_edges <= 360)) {
    grid_df[[xName]] <- grid_df[[xName]] - 360
  }
  grid_df
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
