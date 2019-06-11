dataInfo <- rerddap::info('jplMURSST41mday')
parameter <- 'sst'
xcoord <- c(179, 181)
ycoord <- c(40, 40.1)
tcoord <- c('2019-03-16', '2019-03-16')
extract <- rxtracto_3D(dataInfo, parameter, xcoord = xcoord, ycoord = ycoord,
                       tcoord = tcoord)


dataInfo <- rerddap::info('jplMURSST41mday')
parameter <- 'sst'
xcoord <- c(-130., -129.9)
ycoord <- c(40, 40.1)
tcoord <- c('2019-03-16', '2019-03-16')
extract <- rxtracto_3D(dataInfo, parameter, xcoord = xcoord, ycoord = ycoord,
                       tcoord = tcoord)

dataInfo <- rerddap::info('jplMURSST41mday')
parameter <- 'sst'
xcoord <- c(180.1, 181.)
ycoord <- c(40, 40.1)
tcoord <- c('2018-03-16', '2018-04-16')
xlen <- .5
ylen <- .5
extract <- rxtracto(dataInfo, parameter = parameter, xcoord = xcoord,
                    ycoord = ycoord, tcoord = tcoord,
                    xlen = xlen, ylen = ylen)

dataInfo <- rerddap::info('erdHadISST')

parameter <- 'sst'
xcoord <- c(-130.5)
ycoord <- c(40.5)
tcoord <- c('2006-01-16')
xlen <- 0.01
ylen <- 0.01
extract <- rxtracto(dataInfo, parameter = parameter, xcoord = xcoord,
                    ycoord = ycoord, tcoord = tcoord,
                    xlen = xlen, ylen = ylen)

dataInfo <- rerddap::info('jplMURSST41mday')
parameter <- 'sst'
xcoord <- c(179.7, 179.8, 179.9, 180., 180.1, 180.2, 180.3, 180.4)
ycoord <- c(40, 40, 40, 40, 40, 40, 40, 40)
tcoord <- c('2018-03-16', '2018-03-16', '2018-03-16','2018-03-16','2018-03-16','2018-03-16','2018-03-16','2018-03-16')
xlen <- .05
ylen <- .05
extract <- rxtracto(dataInfo, parameter = parameter, xcoord = xcoord,
                    ycoord = ycoord, tcoord = tcoord,
                    xlen = xlen, ylen = ylen)
