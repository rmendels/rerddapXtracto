####
# All of these utilities are from the rerddap package
#####


check_dims <- function(dimargs, .info) {
  if (!all(names(dimargs) %in% dimvars(.info))) {
    stop(sprintf("Some input dimensions (%s) don't match those in dataset (%s)",
                 paste0(names(dimargs), collapse = ", "),
                 paste0(dimvars(.info), collapse = ", ")), call. = FALSE)
  }
}

check_lon_text <- function(dimargs) {
  if (!is.null(dimargs$longitude)) {
    if (any(sapply(dimargs$longitude, class) == "character")) {
      txt <- dimargs$longitude[sapply(dimargs$longitude, class) == "character"]
      if (!all(grepl("last", txt))) stop("Only text values allowed are 'last' & variants on that", call. = FALSE)
    }
  }
}

check_lat_text <- function(dimargs) {
  if (!is.null(dimargs$latitude)) {
    if (any(sapply(dimargs$latitude, class) == "character")) {
      txt <- dimargs$latitude[sapply(dimargs$latitude, class) == "character"]
      if (!all(grepl("last", txt))) stop("Only text values allowed are 'last' & variants on that", call. = FALSE)
    }
  }
}

is_lon_text <- function(dimargs) {
  if (!is.null(dimargs$longitude)) {
    any(sapply(dimargs$longitude, class) == "character")
  } else {
    FALSE
  }
}

is_lat_text <- function(dimargs) {
  if (!is.null(dimargs$latitude)) {
    any(sapply(dimargs$latitude, class) == "character")
  } else {
    FALSE
  }
}

check_lon_data_range <- function(dimargs, .info) {
  if (!is.null(dimargs$longitude)) {
    val <- .info$alldata$longitude[ .info$alldata$longitude$attribute_name == "actual_range", "value"]
    val2 <- as.numeric(strtrim1(strsplit(val, ",")[[1]]))
    if (!is_lon_text(dimargs)) {
      if (max(dimargs$longitude) > max(val2) || min(dimargs$longitude) < min(val2)) {
        stop(sprintf("One or both longitude values (%s) outside data range (%s)",
                     paste0(dimargs$longitude, collapse = ", "),
                     paste0(val2, collapse = ", ")), call. = FALSE)
      }
    }
  }
}

check_lat_data_range <- function(dimargs, .info) {
  if (!is.null(dimargs$latitude)) {
    val <- .info$alldata$latitude[ .info$alldata$latitude$attribute_name == "actual_range", "value"]
    val2 <- as.numeric(strtrim1(strsplit(val, ",")[[1]]))
    if (!is_lat_text(dimargs)) {
      if (max(dimargs$latitude) > max(val2) || min(dimargs$latitude) < min(val2)) {
        stop(sprintf("One or both latitude values (%s) outside data range (%s)",
                     paste0(dimargs$latitude, collapse = ", "),
                     paste0(val2, collapse = ", ")), call. = FALSE)
      }
    }
  }
}

fix_dims <- function(dimargs, .info) {
  for (i in seq_along(dimargs)) {
    tmp <- dimargs[[i]]
    nm <- names(dimargs[i])
    tmp <- grep("last+", tmp, value = TRUE, invert = TRUE)
    if (nm == "time") {
      tmp <- as.Date(tmp)
      # print(str(tmp))
    }
    val <- .info$alldata[[nm]][ .info$alldata[[nm]]$attribute_name == "actual_range", "value"]
    val2 <- as.numeric(strtrim1(strsplit(val, ",")[[1]]))
    if (length(tmp) != 0) {
      if (which.min(val2) != which.min(tmp)) {
        dimargs[[i]] <- rev(dimargs[[i]])
      }
    }
  }
  dimargs
}

parse_args <- function(.info, dim, s, dimargs, wname = FALSE){
  tmp <- if (dim %in% names(dimargs)) {
    dimargs[[dim]]
  } else {
    if (dim == "time") {
      times <- c(getvar(.info, "time_coverage_start"), getvar(.info, "time_coverage_end"))
      sprintf('[(%s):%s:(%s)]', times[1], s, times[2])
    } else {
      actrange <- foo(.info$alldata[[dim]], "actual_range")
      gsub("\\s+", "", strsplit(actrange, ",")[[1]])
    }
  }

  if (length(s) > 1) {
    if (!length(s) == length(dimvars(.info))) stop("Your stride vector must equal length of dimension variables", call. = FALSE)
    names(s) <- dimvars(.info)
    if (!wname) {
      sprintf('[(%s):%s:(%s)]', tmp[1], s[[dim]], tmp[2])
    } else {
      sprintf('%s[(%s):%s:(%s)]', dim, tmp[1], s[[dim]], tmp[2])
    }
  } else {
    if (!wname) {
      if (length(tmp) == 1) {
        tmp
      } else {
        sprintf('[(%s):%s:(%s)]', tmp[1], s, tmp[2])
      }
    } else {
      if (length(tmp) == 1) {
        tmp
      } else {
        sprintf('%s[(%s):%s:(%s)]', dim, tmp[1], s, tmp[2])
      }
    }
  }
}

getvar <- function(x, y){
  x$alldata$NC_GLOBAL[ x$alldata$NC_GLOBAL$attribute_name == y, "value"]
}

getvars <- function(x){
  vars <- names(x$alldata)
  vars[ !vars %in% c("NC_GLOBAL", "time", x$variables$variable_name) ]
}

getallvars <- function(x){
  vars <- names(x$alldata)
  vars[ !vars %in% "NC_GLOBAL" ]
}

dimvars <- function(x){
  vars <- names(x$alldata)
  vars[ !vars %in% c("NC_GLOBAL", x$variables$variable_name) ]
}

check_time_data_range <- function(dimargs, .info) {
  tmp<-as.Date(dimargs$time,origin='1970-01-01',tz= "GMT")
  tmp<-as.numeric(tmp)*86400
  if (!is.null(tmp)) {
    val <- .info$alldata$time[ .info$alldata$time$attribute_name == "actual_range", "value"]
    val2 <- as.numeric(strtrim1(strsplit(val, ",")[[1]]))
    if (!is_lat_text(dimargs)) {
      if (max(tmp) > max(val2) || min(tmp) < min(val2)) {
        stop(sprintf("One or both time values (%s) outside data range (%s)",
                     paste0(dimargs$time, collapse = ", "),
                     paste0(val2, collapse = ", ")), call. = FALSE)
      }
    }
  }
}

strextract1 <- function(str, pattern) regmatches(str, regexpr(pattern, str))

strtrim1 <- function(str) gsub("^\\s+|\\s+$", "", str)

