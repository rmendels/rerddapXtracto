tagData <- Marlintag38606
xpos <- tagData$lon
ypos <- tagData$lat
tpos <- tagData$date
zpos <- rep(0., length(xpos))
callDims <- list(xpos, ypos, zpos, tpos)
names(callDims) <- c('longitude', 'latitude', 'altitude', 'time')
xrad <- array(.5, dim = length(xpos))
yrad <- array(.5, dim = length(xpos))
swchlInfo <- rerddap::info('erdSWchla8day')
SWchla8day_time <- readr::read_csv("~/WorkFiles/rerddapXtracto_dateline_extras/SWchla8day_time.csv", col_names = FALSE)
names(SWchla8day_time)[1] = 'time'
SWchla8day_lat <- readr::read_csv("~/WorkFiles/rerddapXtracto_dateline_extras/SWchla8day_lat.csv", col_names = FALSE)
names(SWchla8day_lat)[1] = 'latitude'
SWchla8day_lon <- readr::read_csv("~/WorkFiles/rerddapXtracto_dateline_extras/SWchla8day_lon.csv", col_names = FALSE)
names(SWchla8day_lon)[1] = 'longitude'
#dset_udt <- my_extract$time
dset_udt <- my_time
# for each time in tpos,  find the closest actual time in the dataset
# dset_udt holds the actual times from the dataset
# req_time_index will contain the index where the minimum is achieved
req_time_index <- array(NA_integer_, dim = length(tpos))
for (i in seq(1, length(tpos))) {
  temp_time <- parsedate::parse_iso_8601(tpos[i])
  req_time_index[i] <- which.min(abs(dset_udt - temp_time))
}
# unique_req_time_index contains the unique times where extracts
# need to be made
unique_req_time_index <- unique(req_time_index)
i <- 3
# this_time_index contains  the indexes of which extracts all occur
# at a given time
this_time_index <- which(req_time_index == unique_req_time_index[i])
# for positions during that time,  find the largest extent
# of any request that would be made
time_lats <- ypos[this_time_index]
time_lons <- xpos[this_time_index]
time_xrad <- max(xrad[this_time_index])
time_yrad <- max(yrad[this_time_index])
lat_bound <- c(min(time_lats), max(time_lats))
lon_bound <- c(min(time_lons), max(time_lons))
lon_bound[1] <- lon_bound[1] - (time_xrad/2)
lon_bound[2] <- lon_bound[2] + (time_xrad/2)
lat_bound[1] <- lat_bound[1] - (time_yrad/2)
lat_bound[2] <- lat_bound[2] + (time_yrad/2)
# find the nearest coordinate to the largest extent
erddapXcoord <- rep(NA_real_, 2)
erddapYcoord <- rep(NA_real_, 2)
erddapTcoord <- rep(NA, 2)
erddapZcoord <- rep(NA_real_, 2)

erddapYcoord[1] <- latitude[which.min(abs(latitude - lat_bound[1]))]  # minimum latitude
erddapYcoord[2] <- latitude[which.min(abs(latitude - lat_bound[2]))]  # maximum latitude
erddapXcoord[1] <- longitude[which.min(abs(longitude - lon_bound[1]))] # minimum longitude
erddapXcoord[2] <- longitude[which.min(abs(longitude - lon_bound[2]))] # maximum longitude
erddapTcoord[1] <- as.character(dset_udt[unique_req_time_index[i]])# minimum time
erddapTcoord[2] <- as.character(dset_udt[unique_req_time_index[i]])
erddapZcoord <- c(0., 0.)
# extract the data from that large extent
urlbase = 'https://coastwatch.pfeg.noaa.gov/erddap/'
xName <- 'longitude'
yName <- 'latitude'
zName <- 'altitude'
tName <- 'time'
parameter <-  'chlorophyll'
verbose <- TRUE
cache_remove = FALSE
my_extract <- data_extract_read(swchlInfo, callDims, urlbase,
                              xName, yName, zName, tName, parameter,
                              erddapXcoord, erddapYcoord, erddapTcoord, erddapZcoord,
                              verbose, cache_remove)
my_extract[[1]] <- drop(my_extract[[1]])
# loop over positions in that time period to get the
# specfic extract
for (ipos in this_time_index) {
    xIndex <- array(NA_integer_, dim = 2)
    yIndex <- array(NA_integer_, dim = 2)
    xmax <- xpos[ipos] + (xrad[ipos]/2)
    xmin <- xpos[ipos] - (xrad[ipos]/2)
    ymax <- ypos[ipos] + (yrad[ipos]/2)
    ymin <- ypos[itime] - (yrad[itime]/2)
    xIndex[1] <- which.min(abs(my_extract$longitude - xmin))
    xIndex[2] <- which.min(abs(my_extract$longitude - xmax))
    yIndex[1] <- which.min(abs(my_extract$latitude - ymin))
    yIndex[2] <- which.min(abs(my_extract$latitude - ymax))
    param <- my_extract[[1]][xIndex[1]:xIndex[2], yIndex[1]: yIndex[2]]
    str(param)


}
