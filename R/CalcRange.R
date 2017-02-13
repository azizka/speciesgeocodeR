CalcRange <- function(x, method = "pseudospherical", terrestrial = F) {
  # x = object of class data.frame, spgeOUT, SpatialPOints, method = c('euclidean', 'pseudospherical'), terrestrial = logical,
  
  base::match.arg(arg = method, 
                  choices = c("euclidean", "pseudospherical"))
  
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
  
  # check for species with less htan 3 records
  filt <- table(dat$species)
  sortout <- filt[filt <= 2]
  filt <- filt[filt > 2]
  
  dat.filt <- droplevels(subset(dat, dat$species %in% as.character(names(filt))))
  
  if (length(sortout) > 0) {
    warning("found species with < 3 occurrences, excluded from output:", paste("\n", names(sortout)))
  }
  if (length(dat.filt) == 0) {
    stop("no species with more than 2 occurrences found")
  }
  
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
    names(out) <- NULL
    out <- do.call(bind, out)
    out <- SpatialPolygonsDataFrame(out, data = data.frame(species = nam))
  }
  
  # pseudospherical
  if (method == "pseudospherical") {
    out <- lapply(inp, function(k) .ConvHull(k, type = "pseudospherical"))
    nam <- names(out)
    names(out) <- NULL
    out <- do.call(bind, out)
    out <- SpatialPolygonsDataFrame(out, data = data.frame(species = nam))
  }
  
  # cut to landmass
  if (terrestrial) {
    if (!requireNamespace("rgeos", quietly = TRUE)) {
      stop("Package 'rgeos' not found. Please install.", call. = FALSE)
    }
    
    # create landmass mask
    cropper <- raster::extent(sp::SpatialPoints(dat.filt[, 2:3]))
    cropper <- cropper + 1
    cropper <- raster::crop(speciesgeocodeR::landmass, cropper)
    
    out2 <- rgeos::gIntersection(out, cropper, byid = T)
    dat.add <- out@data
    rownames(dat.add) <- getSpPPolygonsIDSlots(out2)
    out <- SpatialPolygonsDataFrame(out2, data = dat.add)
  }
  return(out)
}