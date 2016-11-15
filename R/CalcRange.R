CalcRange <- function(x, index = c("AOO", "EOO"), eoo.value = c("area", "shape"), 
                      eoo.terrestrial = TRUE, aoo.gridsize = NULL, aoo.proj = NULL,
                      aoo.reps = 3, verbose = FALSE) {

  # prepare input data
  if (is.spgeoOUT(x)) {
    dat <- x$samples[, c("species", "decimallongitude", "decimallatitude")]
    dat$species <- as.character(dat$species)
  } else {
    names(x) <- tolower(names(x))
    dat <- x[, c("species", "decimallongitude", "decimallatitude")]
    dat$species <- as.character(dat$species)
  }
  
  # filter out duplicate records, this is necessary to have three unique points
  dat <- dat[!duplicated(dat), ]
  
  # check input data for validity
  if (!is.numeric(dat[, 2]) | !is.numeric(dat[, 3])) {
    stop("input coordinates must be numeric\n")
  }
  if ("AOO" %in% index & "EOO" %in% index & eoo.value[1] == "shape") {
    stop("combined AOO and EOO analyses are only enabled with eoo.value = 'area'")
  }
  
  # EOO calculation
  if ("EOO" %in% index) {
    warning("Using Euclidean convex hull algorithm on lat/long")
    
    # only use species with more than 2 occurrences for EOO
    filt <- table(dat$species)
    sortout <- filt[filt <= 2]
    filt <- filt[filt > 2]
    if (length(filt) == 0) {
      eoo.out <- rbind(data.frame(row.names = names(sortout), 
                                  EOO = rep("NA", length(sortout))))
    } else {
      dat.filt <- subset(dat, dat$species %in% as.character(names(filt)))
      
      if (length(sortout) > 0) {
        warning("the following species have less than 3 occurrence, values set to NA:", paste("\n", names(sortout)))
      }
      
      #calculate convex hull polygons
      inp <- split(dat.filt, f = dat.filt$species)
      eoo.out <- lapply(inp, ".ConvHull")
      
      #crop to landmass
      if (eoo.terrestrial) {
        if (!requireNamespace("geosphere", quietly = TRUE)) {
          stop("rgeos needed for eoo.terrestrial = T. Please install the package.", call. = FALSE)
        }
        #create landmass mask
        cropper <- raster::extent(sp::SpatialPoints(dat.filt[,2:3]))
        cropper <- cropper + 1
        cropper <- raster::crop(speciesgeocodeR::landmass, cropper)
        
        eoo.list <- lapply(eoo.out, function(x) rgeos::gIntersection(x, cropper))
        eoo.out <- do.call(raster::bind, eoo.list)
        eoo.out <- sp::SpatialPolygonsDataFrame(Sr = eoo.out,
                                                data = data.frame(species = names(eoo.list)),
                                                FALSE)
        
      }
      if (eoo.value[1] == "area") {
        eoo.out <- lapply(eoo.out, function(k){round(geosphere::areaPolygon(k)/(1000 * 1000), 0)})
        eoo.out <- data.frame(do.call("rbind", eoo.out))
        names(eoo.out) <- "EOO"
        eoo.out <- rbind(eoo.out, data.frame(row.names = rownames(sortout), EOO = rep("NA", length(sortout))))
      }
    }
  }
  
  # AOO calculation
  if ("AOO" %in% index) {
    wgs84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
    
    if(is.null(aoo.proj)){
      aoo.proj <- CRS("+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +a=6371228 +b=6371228 +units=m +no_defs")
      warning("no CRS for AOO calculation found. Using cylindrical equal area")
    }
    if(is.null(aoo.gridsize)){
      aoo.gridsize <- 4000000
      warning("aoo.gridsize not found. Set to 4 sqkm")
    }

    # identify secies with only one record
    coun <- table(dat$species)
    occ <- dat[dat$species %in% names(coun[coun > 1]), ]
    occ$species <- as.character(occ$species)
    sings <- unique(dat[!occ$species %in% names(coun[coun > 1]), "species"])
    
    pts <- sp::SpatialPoints(occ[, 2:3], proj4string = wgs84)
    pts <- sp::spTransform(pts, aoo.proj)
    
    aoo.extent <- raster::extent(pts)
    aoo.extent <- aoo.extent + (sqrt(aoo.gridsize))
    
    # create a list of species for which to calculate AOO
    occs <- split(data.frame(species = occ[, 1], coordinates(pts)), f = occ$species)
    aoo.out <- list()
    
    for (i in 1:aoo.reps) {
      rr <- aoo.extent + (((sqrt(aoo.gridsize))/aoo.reps) * (i - 1))
      rr <- raster(rr, res = aoo.gridsize)
      
      aoo <- lapply(occs, function(k) {
        pts <- SpatialPoints(k[, 2:3])
        uu <- rasterize(pts, rr, fun = "count")
        uu[uu > 1] <- 1
        sum(getValues(uu), na.rm = T) * aoo.gridsize
      })
      
      aoo.out[[i]] <- do.call("rbind.data.frame", aoo)
      names(aoo.out[[i]])[1] <- paste("rep", i, sep = "_")
      
    }
    # find minimum value and create output object
    aoo.out <- do.call("cbind.data.frame", aoo.out)
    aoo.out <- data.frame(AOO = do.call(pmin, as.data.frame(aoo.out)))
    aoo.out <- rbind(aoo.out, data.frame(AOO = rep(aoo.gridsize, length(sings)), row.names = sings))
  }
  
  if ("EOO" %in% index & "AOO" %in% index) {
    out <- merge(eoo.out, aoo.out, by = "row.names")
    rownames(out) <- out$Row.names
    out <- out[, -1]
  } else if ("EOO" %in% index) {
    out <- eoo.out
  } else if ("AOO" %in% index) {
    out <- aoo.out
  }

  return(out)
} 