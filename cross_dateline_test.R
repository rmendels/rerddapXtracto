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

dataInfo <- rerddap::info('jplMURSST41mday')
parameter <- 'sst'
xcoord <- c(179.7, 179.8, 179.9, 180., 180.1, 180.2, 180.3, 180.4)
ycoord <- c(40, 40, 40, 40, 40, 40, 40, 40)
tcoord <- c('2018-03-16', '2018-03-16', '2018-03-16','2018-03-16','2018-03-16','2018-03-16','2018-03-16','2018-03-16')
xlen <- .4
ylen <- .4
extract <- rxtracto(dataInfo, parameter = parameter, xcoord = xcoord,
                    ycoord = ycoord, tcoord = tcoord,
                    xlen = xlen, ylen = ylen)
library(readr)
library(lubridate)
dataInfo <- rerddap::info('jplMURSST41')
Aleutian2018Stations <- read_csv("~/Documents/Years/FY19/Coastwatch/Aleutian2018Stations.csv")
lat <- Aleutian2018Stations$LAT
lon <- Aleutian2018Stations$LON
index <- lon < 0
lon_360 <- lon
lon_360[index] <- lon_360[index] + 360
my_time <- Aleutian2018Stations$DATE_TIME
my_time1 <- dmy_hms(my_time)
my_time1 <- as.character(round_date(my_time1, unit = 'day'))
extract <- rxtracto(dataInfo, parameter = 'analysed_sst', xcoord = lon_360, ycoord = lat, tcoord = my_time1, xlen = .5, ylen = .5)
require("mapdata")
library(ggplot2)
xlim <- c(170, 195)
ylim <- c(50, 60)
w1 <- map("world2Hires", xlim = xlim, ylim = ylim, fill = TRUE, plot = FALSE)
remove <- c("UK:Great Britain", "France", "Spain", "Algeria", "Mali", "Burkina Faso", "Ghana", "Togo")
w <- map("world2Hires", xlim = xlim, ylim = ylim, fill = TRUE, plot = FALSE)
w <- map("mapdata::world2Hires", regions = w$names[!(w$names %in% remove)], plot = FALSE, fill = TRUE, ylim = ylim, xlim = xlim)

w <- map_data("world2Hires", regions = w1$names[!(w1$names %in% remove)], ylim = ylim, xlim = xlim)
p <- plotTrack(aleutian, lon_360, lat, my_time1, mapData = w, animate = TRUE, cumulative = TRUE)
p1 <- p$ggplot <- p$ggplot + ggplot2::coord_sf(crs = p$crs, xlim = xlim, ylim = ylim, datum = p$datum)

p2 <- p2$ggplot <- p2$ggplot + ggplot2::coord_sf(crs = p2$crs, xlim = xlim, ylim = ylim, datum = p2$datum)
extract$lat <- lat
extract$lon <- lon_360
extract1 <- as.data.frame(extract)
names(extract1)[1] = 'sst'
z <- ggplot(extract1, aes(x = lon, y = lat)) +
  geom_point(aes(colour = sst), size = .5) +
  scale_shape_manual(values = c(19, 1))
z + geom_polygon(data = w, aes(x = long, y = lat, group = group), fill = "grey80") +
  theme_bw() +
  scale_colour_gradient( name = "SST") +
  coord_fixed(1.3, xlim = xlim, ylim = ylim) + ggtitle("MUR SST Along boat track")


inherits(try(sf::st_crs("+proj=invalid"), silent = TRUE), "try-error")
[1] TRUE
inherits(try(sf::st_crs("+proj=longlat"), silent = TRUE), "try-error")
[1] FALSE

res <- rerddap::griddap(dataInfo,
               time = tcoord,
               latitude = ycoord,
               longitude = xcoord,
               fields = parameter)
