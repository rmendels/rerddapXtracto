#' swchl Data
#'
#' pre-Download of Pacific West Coast SST fro use in `plotTrack()` example
#' can run within CRAN Time limits
#'
#' obtained using the `rxtracto()` command
#' tagData <- Marlintag38606
#' xpos <- tagData$lon[1:20]
#' ypos <- tagData$lat[1:20]
#' tpos <- tagData$date[1:20]
#' tpos <- tagData$date[1:20]
#' zpos <- rep(0., length(xpos))
#' swchlInfo <- rerddap::info('erdSWchla8day')
#' swchl <- rxtracto(swchlInfo, parameter = 'chlorophyll', xcoord = xpos,
#'                   ycoord = ypos, tcoord = tpos, zcoord = zpos, xlen = .2, ylen = .2)
"swchl"
