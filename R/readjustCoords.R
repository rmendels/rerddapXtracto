readjustCoords <- function(param, dataX, dataY, xcoord, datafileID, callDims) {
  #  put longitudes back on the requestors scale
  #  reqeust is on (0,360), data is not
  if ('longitude' %in% names(callDims)) {
    if (max(xcoord) > 180.) {
      dataX <- make360(dataX)
    }
    #request is on (-180,180)
    if (min(xcoord) < 0.) {
      dataX <- make180(dataX)
    }
  }

  # some data have latitude go north to south.  Return all data with latitude
  # going south to north.  Do this by flipping latitudes, finding
  # the positon of latitude in the array, and flipping the date in
  # that dimension
  if ('latitude' %in% names(callDims)) {
    if (length(dataY) > 1) {
      if (dataY[1] > dataY[2]) {
        dataY <- rev(dataY)
        dataYLen <- length(dataY)
        #      param <- param[, rev(seq_len(dataYLen)) ,, drop = FALSE]
        paramLen <- length(names(datafileID$dim))
        latLoc <- which(rev(names(datafileID$dim)) == 'latitude')
        param <- flip2(param, latLoc)
      }
    }
  }

return(list(dataX = dataX, dataY = dataY, param = param))
}

flip <- function(a,wh){ ## a is the array; wh is the index to be reversed
  l <- lapply(dim(a),seq_len)
  l[[wh]] <- rev(l[[wh]])
  do.call(`[`, c(list(a), l, drop = FALSE))
}

flip2 <- function(a, i) {
  d <- dim(a)
  l <- as.list(rep(TRUE, length(d))) ## instead of lapply() loop
  l[[i]] <- seq.int(d[i], 1)  ## used rev() in prior version
  do.call("[", c(list(a), l, drop = FALSE))
}
