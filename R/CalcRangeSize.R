CalcRangeSize <- function(x, method = "eoo_pseudospherical", terrestrial = F, biome = NULL, eco = NULL,
                          convex.reps = 100, convex.repfrac = 0.3, convex.repsize = NULL, 
                          aoo.reps = 3, aoo.proj = NULL, aoo.gridsize = NULL, 
                          verbose = F) {
  
  if (!requireNamespace("geosphere", quietly = TRUE)) {
    stop("Package 'geosphere' not found. Please install.", call. = FALSE)
  }
  
  
  # fix different input data types data.frame
  if (is.data.frame(x)) {
    dat <- x[, c("species", "decimallongitude", "decimallatitude")]
  }
  ## spgeoOUt
  if (is.spgeoOUT(x)) {
    dat <- x$samples[, 1:3]
  }
  
  if (class(x) == "SpatialPointsDataFrame") {
    dat <- data.frame(species = x@data$species, coordinates(x))
    names(dat) <- c("species", "decimallongitude", "decimallatitude")
  }
  
  # prepare cropping to landmass
  if (terrestrial) {
    if (!requireNamespace("rgeos", quietly = TRUE)) {
      stop("Package 'rgeos' not found. Please install.", call. = FALSE)
    }
    # create landmass mask
    pts <- sp::SpatialPoints(dat[, c("decimallongitude", "decimallatitude")])
    cropper <- raster::extent(pts)
    cropper <- cropper + 1
    cropper <- raster::crop(speciesgeocodeR::landmass, cropper)
  }

  # area calculations Euclidean convex hull
  if (method == "eoo_euclidean") {
    # species with less than 3 records
    filt <- table(dat$species)
    sortout <- filt[filt <= 2]
    filt <- filt[filt > 2]
    dat.filt <- subset(dat, dat$species %in% as.character(names(filt)))
    
    if (length(sortout) > 0) {
      warning("found species with < 3 occurrences:", 
              paste("\n", names(sortout)))
    }
    if (nrow(dat.filt) == 0) {
      warning("no species with more than 2 occurrences found")
      out <- data.frame(eoo_euc = NA)
    }else{
      
      # split by species
      inp <- split(dat.filt, f = dat.filt$species)
      
      #test for occurrences spanning > 180 degrees
      test <- lapply(inp,function(k){SpatialPoints(k[,2:3])})
      test <- lapply(test, "extent")
      test <- lapply(test, function(k){(k@xmax + 180) - (k@xmin +180)})
      test <- unlist(lapply(test, function(k){k >= 180}))
      if(any(test)){
        stop("data includes species spanning >180 degrees.")
      }
      
      # calcualte polygon area
      are <- lapply(inp, ".ConvArea", reps = convex.reps, 
                    repfrac = convex.repfrac, repsize = convex.repsize, terrestrial = terrestrial, 
                    type = "euclidean", cropper = cropper, biome = biome)
      out <- do.call("rbind.data.frame", are)
      names(out) <- "eoo.euc"
      
      # add species with not enought points as NA
      miss.area <- rep("NA", length(sortout))
      miss.sp <- rownames(sortout)
      miss <- data.frame(row.names = miss.sp, area = miss.area)
      out <- rbind(out, miss)
      names(out) <- "eoo_euc"
    }
  }
  
  # Pseudospherical
  if (method == "eoo_pseudospherical") {
    # species with less than 3 records
    filt <- table(dat$species)
    sortout <- filt[filt <= 2]
    filt <- filt[filt > 2]
    dat.filt <- subset(dat, dat$species %in% as.character(names(filt)))
    
    if (length(sortout) > 0) {
      warning("found species with < 3 occurrences, excluded from output:",
              paste("\n", names(sortout)))
    }
    if (nrow(dat.filt) == 0) {
      warning("no species with more than 2 occurrences found")
      out <- data.frame(eoo_sph = NA)
    }else{
      # split by species
      inp <- split(dat.filt, f = dat.filt$species)
      
      #test for occurrences spanning > 180 degrees
      test <- lapply(inp,function(k){SpatialPoints(k[,2:3])})
      test <- lapply(test, "extent")
      test <- lapply(test, function(k){(k@xmax + 180) - (k@xmin +180)})
      test <- unlist(lapply(test, function(k){k >= 180}))
      if(any(test)){
        stop("data includes species spanning >180 degrees.")
      }
      
      # calcualte polygon area
      are <- lapply(inp, ".ConvArea", reps = convex.reps, 
                    repfrac = convex.repfrac, repsize = convex.repsize, terrestrial = terrestrial, 
                    type = "pseudospherical", cropper = cropper, biome = biome)
      out <- do.call("rbind.data.frame", are)
      names(out) <- "eoo.sph"
      
      # add species with not enought points as NA
      miss.area <- rep("NA", length(sortout))
      miss.sp <- rownames(sortout)
      miss <- data.frame(row.names = miss.sp, area = miss.area)
      out <- rbind(out, miss)
      names(out) <- "eoo_sph"
    }
  }
  
  # AOO
  if (method == "aoo") {
    wgs84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
    
    if (is.null(aoo.proj)) {
      aoo.proj <- CRS("+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +a=6371228 +b=6371228 +units=m +no_defs")
      warning("no CRS for AOO calculation found. Using cylindrical equal area")
    }
    if (is.null(aoo.gridsize)) {
      aoo.gridsize <- 4e+06
      warning("aoo.gridsize not found. Set to 4 sqkm")
    }
    
    if (terrestrial) {
      pts <- SpatialPoints(dat[, c("decimallongitude", "decimallatitude")])
      test <- sp::over(pts, cropper)
      dat <- dat[!is.na(test[, 1]), ]
    }
    
    # identify secies with only one record
    coun <- table(dat$species)
    occ <- dat[dat$species %in% names(coun[coun > 1]), ]
    occ$species <- as.character(occ$species)
    sings <- dat[dat$species %in% names(coun[coun == 1]), "species"]
    
    if(nrow(occ) != 0){
    pts <- sp::SpatialPoints(occ[, 2:3], proj4string = wgs84)
    pts <- sp::spTransform(pts, aoo.proj)
    
    aoo.extent <- raster::extent(pts)
    aoo.extent <- aoo.extent + (sqrt(aoo.gridsize))
    
    # create a list of species for which to calculate AOO
    occs <- split(data.frame(species = occ[, 1], coordinates(pts)), f = occ$species)
    aoo.out <- list()
    
    for (i in 1:aoo.reps) {
      rr <- aoo.extent + (((sqrt(aoo.gridsize))/aoo.reps) * (i - 1))
      rr <- raster(rr)
      res(rr) <- sqrt(aoo.gridsize)
      
      aoo <- lapply(occs, function(k) {
        pts <- SpatialPoints(k[, 2:3])
        uu <- rasterize(pts, rr, fun = "count")
        uu[uu > 1] <- 1
        out <- sum(getValues(uu), na.rm = T) * aoo.gridsize
        return(out)
      })
      aoo.out[[i]] <- do.call("rbind.data.frame", aoo)
      names(aoo.out[[i]])[1] <- paste("rep", i, sep = "_")
    }
    # find minimum value and create output object
    aoo.out <- do.call("cbind.data.frame", aoo.out)
    aoo.out <- data.frame(AOO = do.call(pmin, as.data.frame(aoo.out)))
    aoo.out <- rbind(aoo.out, 
                     data.frame(AOO = rep(aoo.gridsize, length(sings)), 
                                row.names = sings))
    # add species names
    nams <- sapply(occs, "[", 1)
    nams <- unlist(lapply(nams, "[", 1))
    rownames(aoo.out) <- nams
    out <- aoo.out
    out <- round(out / (1000 * 1000), 0)
    }else{
      out <- data.frame(AOO = rep(round(aoo.gridsize / (1000 * 1000), 0), length(sings)), row.names = sings) 
    }
  }
  
  # maxdist
  if (method == "maxdist") {
    # species with less than 3 records
    filt <- table(dat$species)
    sortout <- filt[filt <= 2]
    filt <- filt[filt > 2]
    dat.filt <- subset(dat, dat$species %in% as.character(names(filt)))
    
    if (length(sortout) > 0) {
      warning("found species with < 3 occurrences, excluded from output:",
              paste("\n", names(sortout)))
    }
    if (nrow(dat.filt) == 0) {
      warning("no species with more than 2 occurrences found")
      out <- data.frame(row.names = names(sortout), 
                        maxdist = rep("NA", length(sortout)))
    }else{
      if (terrestrial) {
        pts <- SpatialPoints(dat.filt[, c("decimallongitude", "decimallatitude")])
        test <- sp::over(pts, cropper)
        dat.filt <- dat.filt[!is.na(test[, 1]), ]
      }
      
      # split by species
      inp <- split(dat.filt, f = dat.filt$species)
      
      #distance calculation
      out <- lapply(inp, function(k) {
        geosphere::distm(x = k[, 2:3], fun = distHaversine)
      })
      out <- data.frame(unlist(lapply(out, "max")))
      names(out) <- "maxdist"
      out <- round(out / 1000, 0)
    }
  }
  
  # quantile distance
  if (method == "qdist") {
    # species with less than 3 records
    filt <- table(dat$species)
    sortout <- filt[filt <= 2]
    filt <- filt[filt > 2]
    dat.filt <- subset(dat, dat$species %in% as.character(names(filt)))
    
    if (length(sortout) > 0) {
      warning("found species with < 3 occurrences, excluded from output:",
              paste("\n", names(sortout)))
    }
    if (nrow(dat.filt) == 0) {
      warning("no species with more than 2 occurrences found")
      out <- data.frame(row.names = names(sortout), 
                              qdist25 = rep("NA", length(sortout)),
                              qdist75 = rep("NA", length(sortout)))
    }else{
      
      if (terrestrial) {
        pts <- SpatialPoints(dat.filt[, c("decimallongitude", "decimallatitude")])
        test <- sp::over(pts, cropper)
        dat.filt <- dat.filt[!is.na(test[, 1]), ]
      }
      
      # split by species
      inp <- split(dat.filt, f = dat.filt$species)
      
      #distance calculation
      out <- lapply(inp, function(k) {
        geosphere::distm(x = k[, 2:3], fun = geosphere::distHaversine)
      })
      out.qntd <- lapply(out, function(k) {
        apply(k, 2, "max")
      })  #take the maximum distance per point
      out.qntd <- lapply(out.qntd, function(k) {
        stats::quantile(k, probs = c(0.25, 0.75))
      })
      out <- do.call("rbind.data.frame", out.qntd)
      names(out) <- c("qdist25", "qdist75")
      out <- round(out / 1000, 0) 
    }
  }
  
  if (method == "ecoregion"){
    if (!requireNamespace("rgeos", quietly = TRUE)) {
      stop("Package 'rgeos' not found. Please install.", call. = FALSE)
    }
    if(is.null(eco)){
      warning("'eco' not specified, dovnloading wwf ecoregions")
    }
    pts <- sp::SpatialPoints(dat[c("decimallongitude", "decimallatitude")])
    eco.calc <- raster::crop(eco, raster::extent(pts))
    if(is.null(eco.calc)){
      eco.calc <- eco
    }
    
    # split by species
    inp <- split(dat, f = dat$species)
    
    out <- lapply(inp, function(k){
      pts <- SpatialPoints(k[, c("decimallongitude", "decimallatitude")])
      out <- over(pts, eco.calc)
      if(all(is.na(out))){
        out <- NA
      }else{
        out <- sum(areaPolygon(eco[eco$ECO_ID %in% out$ECO_ID,]))
      }
    })
    
    out <- do.call("rbind.data.frame", out)
    names(out) <- "ecoregion.size"
    out <- round(out / (1000 * 1000), 0) 
  }
  
  return(out)
}