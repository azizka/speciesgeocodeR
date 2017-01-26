RichnessGrid <- function(x, ras, reso = 1, type = "spnum") {
  # Input spgeoOUT objects
  if (is.spgeoOUT(x)) {
    x <- x$samples
  }
  # Input data.frame
  if (is.data.frame(x)) {
    names(x) <- tolower(names(x))
    if(!all(c("species" %in% names(x),
              "decimallongitude" %in% names(x),
              "decimallatitude" %in% names(x)))){
      x <- x[, 1:3]
      names(x) <- c("species", "decimallongitude", "decimallatitude")
    }else{
      x <- subset(x, select = c("species", "decimallongitude", "decimallatitude"))
    }
  }
  
  # Create raster
  if (missing(ras)) {
    pts <- sp::SpatialPoints(x[, c("decimallongitude", "decimallatitude")])
    e <- raster::extent(pts)
    ras <- raster::raster(e)
    raster::res(ras) <- reso
  }
  ras <- raster::setValues(ras, 0)
  
  out <- switch(type, 
                spnum = {
                  inp <- split(x, f = as.character(x$species))
                  inp <- lapply(inp, function(k) .rasterSum(k, ras, type = "div"))
                  Reduce("+", inp)},
                abu = {.rasterSum(x, ras, type = "abu")})

  
  out[out == 0] <- NA
  
  return(out)
}