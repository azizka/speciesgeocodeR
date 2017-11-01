.CapitalCoordinates <- function(x, testdist = 0.1, buffer = 1, referencedat = NULL) {
  dat <- sp::SpatialPoints(x)
  if (is.null(referencedat)) {
    referencedat <- speciesgeocodeR::capitals
  }else{
    proj4string(referencedat) <- ""
    warning("assuming lat/lon for centroids.ref")
  }
  
  limits <- raster::extent(dat) + buffer
  
  # subset of testdatset according to limits
  referencedat <- raster::crop(SpatialPoints(referencedat[, c("longitude", "latitude")]), limits)
  
  if(is.null(referencedat)){ # incase no capitals are found in the study area
    out <- rep(TRUE, nrow(x))
  }else{
    referencedat <- rgeos::gBuffer(referencedat, width = testdist, byid = T)
    out <- is.na(sp::over(x = dat, y = referencedat))
  }
  
  return(out)
}

.CentroidCoordinates <- function(x, testdist = 0.1, buffer = 1, testtype = c("both", "country", "provinces"), referencedat = NULL) {
  dat <- sp::SpatialPoints(x)
  if (is.null(referencedat)) {
    referencedat <- speciesgeocodeR::centroids
    
    if (testtype[1] == "country") {
      referencedat <- referencedat[referencedat$type == "country", ]
    }
    if (testtype[1] == "province") {
      referencedat <- referencedat[referencedat$type == "province", ]
    }
  }else{
    proj4string(referencedat) <- ""
    warning("assuming lat/lon for centroids.ref")
  }
  
  limits <- raster::extent(dat) + buffer
  
  # subset of testdatset according to limits
  referencedat <- raster::crop(SpatialPoints(referencedat[, c("longitude", "latitude")]), limits)
  if(is.null(referencedat)){ # incase no capitals are found in the study area
    out <- rep(TRUE, nrow(x))
  }else{
    referencedat <- rgeos::gBuffer(referencedat, width = testdist, byid = T)
    out <- is.na(sp::over(x = dat, y = referencedat))
  }
  
  return(out)
}

.CountryCheck <- function(x, countries, poly = NULL) {
  if (!requireNamespace("rnaturalearth", quietly = TRUE)) {
    stop("rnaturalearth needed for countries test option. Please install", 
         call. = FALSE)
  }
  pts <- SpatialPoints(x)
  
  if (is.null(poly)) {
    testpolys <- rnaturalearth::ne_countries(scale = "medium")
  }else{
    testpolys <- poly
    
    warning("assuming lat/lon for country.ref")
  }
  
  proj4string(testpolys) <- ""
  testpolys <- crop(testpolys, extent(pts))
  
  country <- sp::over(x = pts, y = testpolys)[, "iso_a3"]
  out <- as.character(country) == as.character(countries)
  out[is.na(out)] <- TRUE
  
  return(out)
}

.OutlierCoordinates <- function(x, species, mltpl, tdi, outl.method) {
  
  #split up into species
  splist <- split(x, f = as.character(species))
  
  #remove duplicate records and make sure that there are at least two records left
  test <- lapply(splist, "duplicated")
  test <- lapply(test, "!")
  test <- as.vector(unlist(lapply(test, "sum")))
  splist <- splist[test > 2]
  
  #loop over species and run outlier test
  flags <- lapply(splist, function(k) {
    test <- nrow(k[!duplicated(k), ])
    
    if(test >2){
      
      #absolute distance test with mean interpoint distance
      if (outl.method == "dist.distance") {
        dist <- geosphere::distm(k, fun = geosphere::distHaversine)
        dist[dist == 0] <- NA
        
        mins <- apply(dist, 1, min, na.rm = T)
        out <- which(mins > tdi * 1000)
      }
      
      #Quantile based test, with mean interpoint distances
      if (outl.method == "dist.quantile") {
        dist <- geosphere::distm(k, fun = geosphere::distHaversine)
        dist[dist == 0] <- NA
        
        mins <- apply(dist, 1, mean, na.rm = T)
        quo <- quantile(mins, c(0.25, 0.75), na.rm = T)
        out <- which(mins < quo[1] - IQR(mins) * mltpl | mins > quo[2] + IQR(mins) * mltpl)
      }
      
      #MAD (Median absolute deviation) based test, calculate the mean distance to all other points for each point, and then take the mad of this
      if (outl.method == "dist.mad") {
        dist <- geosphere::distm(k, fun = geosphere::distHaversine)
        dist[dist == 0] <- NA
        
        mins <- apply(dist, 1, mean, na.rm = T)
        quo <- median(mins)
        tester <- mad(mins)
        out <- which(mins < quo - tester * mltpl | mins > quo + tester * mltpl)
      }
      # These did  not work, mostly because the centroid function does give longitudes > 360
      # #Absolute distance from centroid, only recommended for local scale data
      # if (outl.method == "cent.distance"){
      #   cent <- geosphere::centroid(k)
      #   dist <- geosphere::distHaversine(k, cent)
      #   
      #   out <- which(dist > tdi * 1000)
      # }
      # 
      # #Absolute distance from centroid, only recommended for local scale data
      # if (outl.method == "cent.quantile"){
      #   cent <- geosphere::centroid(k)
      #   dist <- geosphere::distHaversine(k, cent)
      #   
      #   quo <- quantile(dist, c(0.25, 0.75), na.rm = T)
      #   out <- which(dist < (quo[1] - IQR(mins) * mltpl) | mins > (quo[2] + IQR(dist) * mltpl))
      # }
    }
    
    #create output object
    if (length(out) == 0) {
      ret <- NA
    }
    if (length(out) > 0) {
      ret <- rownames(k)[out]
    }
    return(ret)
  })
  
  flags <- as.numeric(as.vector(unlist(flags)))
  flags <- flags[!is.na(flags)]
  
  out <- rep(TRUE, nrow(x))
  out[flags] <- FALSE
  
  return(out)
}

.UrbanCoordinates <- function(x, poly = NULL) {
  pts <- SpatialPoints(x)
  limits <- extent(pts) + 1
  
  if (is.null(poly)) {
    stop("No referencepolygons found. Set 'urban.ref'")
  }else{
    proj4string(poly) <- ""
    warning("assumning lat/lon for urban.ref")
  }
  
  poly <- crop(poly, limits)
  
  urban <- over(x = pts, y = poly)[, 1]
  out <- is.na(urban)
  
  return(out)
}

.GBIF <- function(x) {
  pts <- sp::SpatialPoints(x)
  poly <- rgeos::gBuffer(SpatialPoints(cbind(12.58, 55.67)), width = 0.5)
  warning("running GBIF test, flagging records around Copenhagen")
  
  out <- sp::over(x = pts, y = poly)
  out <- is.na(out)
  
  return(out)
}

.ValidCoordinates <- function(x) {
  out <- list(is.na(x$decimallongitude), is.na(x$decimallatitude), suppressWarnings(is.na(as.numeric(as.character(x$decimallongitude)))), suppressWarnings(is.na(as.numeric(as.character(x$decimallatitude)))), 
              suppressWarnings(as.numeric(as.character(x$decimallongitude))) < -180, suppressWarnings(as.numeric(as.character(x$decimallongitude))) > 180, suppressWarnings(as.numeric(as.character(x$decimallatitude))) < 
                -90, suppressWarnings(as.numeric(as.character(x$decimallatitude))) > 90)
  
  out <- !Reduce("|", out)
  return(out)
}

.WaterCoordinates <- function(x, poly = NULL) {
  pts <- SpatialPoints(x)
  
  if (length(poly) == 0) {
    testpolys <- speciesgeocodeR::landmass
    testpolys <- crop(testpolys, extent(pts) + 1)
  } else {
    proj4string(poly) <- ""
    warning("Assuming lat/lon for seas.ref")
    testpolys <- poly
  }

  land <- over(x = pts, y = testpolys)[, 1]
  out <- !is.na(land)
  
  return(out)
}

.ZeroCoordinates <- function(x, pointlim = 0.5) {
  pts <- SpatialPoints(x)
  out <- rep(T, nrow(x))
  
  # plain zero in coordinates
  out[which(x$decimallongitude == 0 | x$decimallatitude == 0)] <- FALSE
  
  # radius around point 0/0
  test <- rgeos::gBuffer(sp::SpatialPoints(cbind(0, 0)), width = pointlim)
  out[which(!is.na(over(y = test, x = pts)))] <- FALSE
  
  # lat == long
  out[which(x$decimallongitude == x$decimallatitude)] <- FALSE
  
  # #abs lat == abs lon
  # out[which(x$decimallongitude == x$decimallatitude)] <- FALSE
  
  return(out)
} 

.Institutions <- function(x, testdist = 0.001, buffer = 1, referencedat = NULL){
  dat <- sp::SpatialPoints(x)
  if (is.null(referencedat)) {
    referencedat <- speciesgeocodeR::institutions
  }
  
  limits <- raster::extent(dat) + buffer
  
  # subset of testdatset according to limits
  referencedat <- referencedat[!is.na(referencedat$decimallongitude) &
                                 !is.na(referencedat$decimallatitude),]
  referencedat <- raster::crop(SpatialPoints(referencedat[, c("decimallongitude", "decimallatitude")]), limits)
  
  if(is.null(referencedat)){ # in case no bdinstitutions
    out <- rep(TRUE, nrow(x))
  }else{
    referencedat <- rgeos::gBuffer(referencedat, width = testdist, byid = T)
    out <- is.na(sp::over(x = dat, y = referencedat))
  }
  
}
