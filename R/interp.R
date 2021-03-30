erddap_interp <- function(urlbase, datasetid, parameter,
                          xcoord, ycoord, zcoord, tcoord,
                          interp_type, verbose, progress_bar){
  base_dir <- paste0(urlbase, 'convert/interpolate.csv?TimeLatLonTable=')
  #header_line <- 'latitude,longitude%0A'
  header_line <- 'time,latitude,longitude%0A'
  if (!is.null(zcoord)) {
    header_line <- paste0('altitude', ',', header_line)
  }
  #if (!is.null(tcoord)) {
  #  header_line <- paste0('time', ',', header_line)
  #}
  series_length <- length(xcoord)
  start_loc <- seq(1, series_length, 100)
  temp_length <- length(start_loc)
  if (series_length < 101) {
    end_loc <- c(series_length)
  } else {
    end_loc <- c(start_loc[2:temp_length] - 1, series_length)
  }

  extract <- NULL
  if (progress_bar){
    pb <- utils::txtProgressBar(min = 0, max = length(start_loc), style = 3)
    i_pb <- 0
  }
  for (i in seq(1, length(start_loc))) {
    if (progress_bar){
      i_pb <- i_pb + 1
      utils::setTxtProgressBar(pb, i_pb)
    }
    data_line = ''
    for (j in seq(start_loc[i], end_loc[i])){
       temp_line <- paste0(tcoord[j], ',', ycoord[j], ',', xcoord[j], '%0A')
      if (!is.null(zcoord)) {
        temp_line <- paste0(zcoord[j], ',', temp_line)
      }
      #if (!is.null(tcoord)) {
      #  temp_line <- paste0(tcoord[j], ',', temp_line)
      #}
      data_line = paste0(data_line, temp_line)
    }
    end_line <- paste0('&requestCSV=', datasetid, '/', parameter, '/',
                       interp_type[1], '/', interp_type[2])
    my_url <- paste0(base_dir, header_line, data_line, end_line)
    #r1 <-  httr::GET(my_url, verbose())
    numtries <- 10
    tryn <- 0
    goodtry <- -1
    while ((tryn <= numtries) && (goodtry == -1)) {
      tryn <- tryn + 1
      if (verbose) {
        r1 <- try( httr::GET(my_url, verbose()), silent = TRUE)
      } else {
        r1 <- try( httr::GET(my_url), silent = TRUE)
      }
      if (r1$status_code == 200) {
        goodtry <- 1
      } else{
        Sys.sleep(tryn * 0.5)
      }
    }
    if (goodtry == -1) {
      print('error while trying to do the extract')
      print('will return what has been extracted so far')
      return(extract)
    }
    interp <- suppressMessages(readr::read_csv(r1$content))
    no_interp_cols <- ncol(interp)
    col_indices <- c(no_interp_cols, seq(1, (no_interp_cols - 1)))
    interp <- interp[, col_indices]
    if (i == 1) {
      extract <- interp
    } else {
      extract <- rbind(extract, interp)
    }
  }
  if (progress_bar) {
    close(pb)
  }
  names(extract)[1]  <- parameter
  attr(extract, 'interpolation_type') <- interp_type
  attr(extract, 'datasetID') <- datasetid
return(extract)
}

check_interp <- function(interp_type, xcoord, ycoord, zcoord, tcoord){
  return_code = 0
  interp_types = c('Nearest', 'Bilinear', 'Mean', 'SD', 'Median ', 'Scaled',
                   'InverseDistance', 'InverseDistance2', 'InverseDistance4',
                   'InverseDistance6')
  neighbors <- c('1', '4', '16', '36', '8', '64', '216')
  if(is.null(tcoord)) {
    print('No time coordinate given')
    print('At present the interpolation option requires time, lat, lon')
    print('A future version may allow for just lat, lon')
    return_code = 1
    return(return_code)
  }
  if(!length(interp_type) == 2) {
    print('error in interpolation request')
    print('interpolation type must be of length two')
    print('the first element a string of the type of interpolation to use')
    print('the second a string giving the size of the spatial neighborhood')
    print('you have passed:')
    print(interp_type)
    return_code = 1
    return(return_code)
  }
  if(!(interp_type[1] %in% interp_types)) {
    print('the interpolation type is not one of the available options')
    print(paste0('You passed ', interp_type[1]))
    print('Must be one of:')
    print(interp_types)
    return_code = 1
    return(return_code)
  }
  if(!(interp_type[2] %in% neighbors)){
    print('Number of neighbors must be one of:')
    print(neighbors)
    print(paste0('You passed ', interp_type[2]))
    return_code = 1
    return(return_code)
  }
  if((interp_type[1] == 'Bilinear') && (!interp_type[2] == 4)) {
    print('Bilinear Interpolation selected but neighbor value must be 4')
    print(paste0('Neighbor value given ', interp_type[2]))
    return_code = 1
    return(return_code)
  }
 #if((length(dataCoordList) == 2) && (interp_type[2] %in% neighbor_3d)) {
 #   print('3-D interpolation chosen but there are only 2 coordinates')
 #   stop()
 # }
  return(return_code)
}
