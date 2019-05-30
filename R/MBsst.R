#' MBsst Data
#'
#' pre-Download of Pacific West Coast SST fro use in `plotBBox()` example
#' can run within CRAN Time limits
#'
#' obtained using the `rxtracto_3D()` command
#' dataInfo <- rerddap::info('erdMBsstd1day')
#' parameter <- 'sst'
#' xcoord <- c(230, 231)
#' ycoord <- c(33, 34)
#' tcoord <- c('2006-01-15', '2006-01-15')
#' zcoord <- c(0., 0.)
#' MBsst <- rxtracto_3D(dataInfo, parameter, xcoord = xcoord, ycoord = ycoord,
#'                      tcoord = tcoord, zcoord = zcoord)
"MBsst"
