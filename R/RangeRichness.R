RangeRichness <- function(x, ras, reso = 1, buffer = 1, terrestrial = FALSE) {
  
  #enable spgeoOUT object as input
  if(is.spgeoOUT(x)){
    x <- CalcRange(x, index = "EOO", eoo.value = "shape")
  }

  # create backround raster
  if (missing(ras)) {
    ras <- raster(extent(x) + buffer)
    res(ras) <- reso
  }
  
  # rasterize range polygons
  out <- lapply(x@polygons, function(k) {
    tt <- rasterize(SpatialPolygons(list(k)), ras, background = 0)
  })
  
  # sum up the raster values
  out <- Reduce("+", out)
  
  # crop to land surface
  if (terrestrial) {
    test <- rasterize(speciesgeocodeR::landmass, out)
    out <- mask(out, test)
  }
  
  return(out)
}