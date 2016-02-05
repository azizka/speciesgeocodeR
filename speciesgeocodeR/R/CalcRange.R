CalcRange <- function(x, index = c("AOO", "EOO"), eoo.value = c("area", "shape"), eoo.terrestrial = TRUE, 
                      aoo.gridsize = 4, aoo.reps = 3, aoo.xmin = NULL, aoo.xmax = NULL, aoo.ymin = NULL, 
                      aoo.ymax = NULL, verbose = FALSE) {
  
  #prepare input data
  if (is(x)[1] == "spgeoOUT") {
    dat <- data.frame(identifier = x$identifier_in, x$species_coordinates_in)
    names(dat) <- c("scientificName", "decimalLongitude", "decimalLatitude")
    dat$scientificName <- as.character(dat$scientificName)
  } else {
    dat <- x[, 1:3]
    names(dat) <- c("scientificName", "decimalLongitude", "decimalLatitude")
    dat$scientificName <- as.character(dat$scientificName)
  }
  
  #filter out duplicate records, this is necessary to have three unique points
  dat <- unique(dat)
  
  #check input data for validity
  if (!is.numeric(dat[, 2]) | !is.numeric(dat[, 3])) {
    stop("wrong input format, x must be a data.frame with three columns: scientificName, decimalLongitude, decimalLatitude.\n")
  }
  if (max(dat[, 2]) > 180 | min(dat[, 2]) < -180 | max(dat[, 3]) > 90 | min(dat[, 3]) < 
      -90) {
    stop("invallid input coordinates. Check for column order and valid coordinates.")
  }
  if ("AOO" %in% index & "EOO" %in% index & eoo.value[1] == "shape") {
    stop("combined AOO and EOO analyses are only enabled with eoo.value = 'area'")
  }
  
  # EOO calculation
  if ("EOO" %in% index) {
    
    # only use species with more than 2 occurrences for EOO
    filt <- table(dat$scientificName)
    filt <- filt[filt > 2]
    dat.filt <- subset(dat, dat$scientificName %in% as.character(names(filt)))
    sortout <- filt[filt <= 2]
    
    if (length(sortout) > 0) {
      warning("the following species have less than 3 occurrence, values set to NA:", 
              paste("\n", names(sortout)))
    }
    
    inp <- split(dat.filt, f = dat.filt$scientificName)
    
    if (eoo.value[1] == "area") {
      out <- lapply(inp, function(x) .eoo(x, clipp = T))
      out <- data.frame(do.call("rbind", out))
      names(out) <- "EOO"
      eoo.out <- rbind(out, data.frame(row.names = rownames(sortout), 
                                       EOO = rep("NA",length(sortout))))
    }
    
    if (eoo.value[1] == "shape") {
      out <- lapply(inp, function(x) .ConvHull(x))
      if (eoo.terrestrial) {
        if (!requireNamespace("geosphere", quietly = TRUE)) {
          stop("rgeos needed for eoo.terrestrial = T. Please install the package.", 
               call. = FALSE)
        }
        if (verbose == T) {
          cat("Clipping shapes to landmasses.")
        }
        eoo.out <- lapply(out, function(x) rgeos::gIntersection(x, speciesgeocodeR::landmass))
        
      }
    }
  }
  
  # AOO calcualtion
  if ("AOO" %in% index) {
    
    if (is.null(aoo.xmin)) {
      aoo.xmin <- min(dat$decimalLongitude)
      warnings("aoo.xmin not specified, guessed from data")
    }
    if (is.null(aoo.xmax)) {
      aoo.xmax <- max(dat$decimalLongitude)
      warnings("aoo.xmax not specified, guessed from data")
    }
    if (is.null(aoo.ymin)) {
      aoo.ymin<- min(dat$decimalLatitude)
      warnings("aoo.ymin not specified, guessed from data")
    }
    if (is.null(aoo.ymax)) {
      aoo.ymax <- max(dat$decimalLatitude)
      warnings("aoo.ymax not specified, guessed from data")
    }
    
    wgs84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
    caep <- CRS("+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +a=6371228 +b=6371228 +units=m +no_defs")
    
    aoo.out <- list()
    
    for (i in 1:aoo.reps) {
      tt <- SpatialPoints(matrix(c(aoo.xmin, aoo.xmax, aoo.ymin, aoo.ymax), ncol = 2), 
                          proj4string = wgs84)
      tt <- spTransform(tt, caep)
      
      tt <- SpatialPoints(coordinates(tt) + (((sqrt(aoo.gridsize) *1000) / aoo.reps) * (i - 1)))
      pp <- raster(tt, res = aoo.gridsize * 1000)
      
      occs <- split(dat, f = dat$scientificName)
      
      aoo <- lapply(occs, function(k) {
        pts <- SpatialPoints(k[, 2:3], proj4string = wgs84)
        pts <- spTransform(pts, caep)
        
        uu <- rasterize(pts, pp)
        uu[uu >= 1] <- 1
        dum <- sum(getValues(uu), na.rm = T) * aoo.gridsize
        return(dum)
      })
      
      aoo.out[[i]] <- do.call("rbind.data.frame", aoo)
      names(aoo.out[[i]])[1] <- paste("rep", i, sep = "_")
      
    }
    aoo.out <- do.call("cbind.data.frame", aoo.out)
    aoo.out <- data.frame(AOO = rowMeans(aoo.out))
    
  }
  
  if ("EOO" %in% index & "AOO" %in% index) {
    out <- merge(eoo.out, aoo.out, by = "row.names")
    rownames(out) <- out$Row.names
    out <- out[, -1]
    class(out) <- c("range.sizes", class(out))

  } else if ("EOO" %in% index) {
    out <- eoo.out
    class(out) <- c("range.sizes", class(out))
  } else if ("AOO" %in% index) {
    out <- aoo.out
    class(out) <- c("range.sizes", class(out))
  }
  
  
  return(out)
} 