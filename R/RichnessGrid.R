RichnessGrid <- function(x, ras, reso = 1, type = "spnum") {
  # Input spgeoOUT objects
  if (is.spgeoOUT(x)) {
    x <- x$samples
  }
  # Input data.frame
  if (is.data.frame(x)) {
    names(x) <- tolower(names(x))
    if(!c("species", "decimallongitude", "decimallatitude") %in% names(x)){
      x <- x[, 1:3]
      names(x) <- c("species", "decimallongitude", "decimallatitude")
    }else{
      x <- subset(x, select = c("species", "decimallongitude", "decimallatitude"))
    }
    
  }
  
  # Input character string, downloading from gbif
  if (is.character(x)) {
    if (!requireNamespace("rgbif", quietly = TRUE)) {
      stop("rgbif needed for species name option. Please install it.", call. = FALSE)
    }
    coords <- rgbif::occ_search(scientificName = x, 
                                return = "data", 
                                limit = 2e+05, 
                                hasCoordinate = T, 
                                hasGeospatialIssue = F, 
                                fields = c("species", "decimalLongitude", "decimalLatitude"))
    if(length(x) == 1){
      coords <- do.call("cbind.data.frame", coords)
    }else{
      coords <- do.call("rbind.data.frame", coords)
    }
    coords <- data.frame(coords[complete.cases(coords), ])
    names(coords) <- tolower(names(coords))
    x <- coords
    warning(paste(nrow(x), 
                  "geo-referenced records found in GBIF.", sep = " "))
  }
  
  # Create raster
  if (missing(ras)) {
    pts <- SpatialPoints(x[, c("decimallongitude", "decimallatitude")])
    e <- extent(pts)
    ras <- raster(e)
    res(ras) <- reso
  }
  ras <- setValues(ras, 0)
  
  out <- switch(type, spnum = {
    inp <- split(x, f = as.character(x$species))
    lapply(inp, function(k) .rasterSum(k, ras, type = "div"))
  }, abu = .rasterSum(pts, ras, type = ""))
  
  out <- Reduce("+", out)
  out[out == 0] <- NA
  
  return(out)
}