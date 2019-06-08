dataInfo <- rerddap::info('jplMURSST41mday')
parameter <- 'sst'
xcoord <- c(179, 181)
ycoord <- c(40, 40.1)
tcoord <- c('2019-03-16', '2019-03-16')
extract <- rxtracto_3D(dataInfo, parameter, xcoord = xcoord, ycoord = ycoord,
                       tcoord = tcoord)
