CalcRange <- function(x, method = "pseudospherical", terrestrial = F, 
                      rare = "buffer", buffer.width = 10000) {
  # x = object of class data.frame, spgeOUT, SpatialPOints, method = c('euclidean', 'pseudospherical'), terrestrial = logical,
  
  base::match.arg(arg = method, 
                  choices = c("euclidean", "pseudospherical"))
  base::match.arg(arg = rare, 
                  choices = c("buffer", "drop"))
  #projection
  wgs84 <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  warning("Assuming lat/long wgs84 coordinates")
  
  # check for geosphere package
  if (!requireNamespace("geosphere", quietly = TRUE)) {
    stop("Package 'geosphere' not found. Please install the package.", call. = FALSE)
  }
  
  # fix different input data types
  ## data.frame
  if (is.data.frame(x)) {
    names(x) <- tolower(names(x))
    dat <- x[, c("species", "decimallongitude", "decimallatitude")]
  }
  ## spgeoOUt
  if (is.spgeoOUT(x)) {
    dat <- x$samples[, 1:3]
  }
  
  # check for species with less than 3 records
  filt <- table(dat$species)
  sortout <- filt[filt <= 2]
  filt <- filt[filt > 2]
  
  dat.filt <- droplevels(subset(dat, dat$species %in% as.character(names(filt))))
  
  #check for species where all lat or long ar identical, to prevent line polygons
  ##longitude
  test <- split(dat.filt, f = dat.filt$species)
  test <- sapply(test, function(k){
    length(unique(k$decimallongitude))
  })
  sortout2 <- names(test[test == 1])
  sortout <- c(sortout, sortout2)
  dat.filt <- droplevels(subset(dat.filt, !dat.filt$species %in% sortout))
  
  #latitude
  test <- split(dat.filt, f = dat.filt$species)
  test <- sapply(test, function(k){
    length(unique(k$decimallatitude))
  })
  sortout2 <- names(test[test == 1])
  sortout <- c(sortout, sortout2)
  dat.filt <- droplevels(subset(dat.filt, !dat.filt$species %in% sortout))
  
  if (length(sortout) > 0) {
    warning("found species with < 3 occurrences:", paste("\n", names(sortout)))
  }
  if (nrow(dat.filt) == 0) {
    stop("no species with more than 2 occurrences found")
  }
  
  #calculate convex hulls
  inp <- split(dat.filt, f = dat.filt$species)
  
  #test for occurrences spanning > 180 degrees
  test <- lapply(inp, function(k){SpatialPoints(k[,2:3])})
  test <- lapply(test, "extent")
  test <- lapply(test, function(k){(k@xmax + 180) - (k@xmin +180)})
  test <- unlist(lapply(test, function(k){k >= 180}))
  if(any(test)){
    stop("data includes species spanning >180 degrees.")
  }

  # calculate ranges based on method euclidean
  if (method == "euclidean") {
    out <- lapply(inp, function(k) .ConvHull(k, type = "euclidean"))
    nam <- names(out)
    if(length(out) == 1){
      names(out) <- NULL
      out <- SpatialPolygonsDataFrame(out[[1]], 
                                      data = data.frame(species = nam, 
                                                        row.names = paste(nam, "_convhull", sep = "")))
      suppressWarnings(proj4string(out) <- wgs84)
    }
    if(length(out > 1)){
     names(out) <- NULL
     out <- do.call(bind, out)
     out <- SpatialPolygonsDataFrame(out, data = data.frame(species = nam))
     suppressWarnings(proj4string(out) <- wgs84)
    }
  }
  
  # pseudospherical
  if (method == "pseudospherical") {
    out <- lapply(inp, function(k) .ConvHull(k, type = "pseudospherical"))
    nam <- names(out)
    if(length(out) == 1){
      names(out) <- NULL
      out <- SpatialPolygonsDataFrame(out[[1]], 
                                      data = data.frame(species = nam, 
                                                        row.names = paste(nam, "_convhull", sep = "")))
      suppressWarnings(proj4string(out) <- wgs84)
    }else{
      names(out) <- NULL
      out <- do.call(bind, out)
      out <- SpatialPolygonsDataFrame(out, data = data.frame(species = nam))
      suppressWarnings(proj4string(out) <- wgs84)
    }
  }
  
  #calculate buffer if rare  == buffer
  if(rare == "buffer" & length(sortout) > 0){
    rar <- droplevels(subset(dat, dat$species %in% as.character(names(sortout))))
    
    cea <- sp::CRS("+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +a=6371228 +b=6371228 +units=m +no_defs")
    
    rar <- sp::SpatialPointsDataFrame(rar[, c("decimallongitude", "decimallatitude")],
                                       data = rar[,"species", drop = FALSE], proj4string = wgs84)
    rar.cea <- sp::spTransform(rar, cea)
    rar.cea <- rgeos::gBuffer(rar.cea, width = buffer.width, byid=TRUE)
    rar <- sp::spTransform(rar.cea, wgs84)
    out <- rbind(out, rar)
    warning(sprintf("using buffer based range for species with <3 records, bufferradius = %s", 
                    buffer.width))
  }
  if(rare == "drop" & length(sortout) > 0){
    warning("species with < 3 records dropped from output")
  }

  # cut to landmass
  if (terrestrial) {
    if (!requireNamespace("rgeos", quietly = TRUE)) {
      stop("Package 'rgeos' not found. Please install.", call. = FALSE)
    }
    
    # create landmass mask
    cropper <- raster::extent(sp::SpatialPoints(dat[, 2:3]))
    cropper <- cropper + 1
    cropper <- raster::crop(speciesgeocodeR::landmass, cropper)

    out2 <- rgeos::gIntersection(out, cropper, byid = T)
    dat.add <- out@data
    rownames(dat.add) <- getSpPPolygonsIDSlots(out2)
    out <- SpatialPolygonsDataFrame(out2, data = dat.add)
  }
  return(out)
}