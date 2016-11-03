RangeRichness <- function(ra, limits = c(-180, 180, -90, 90), reso = 1, terrestrial = FALSE){
  
  limits <- extent(limits)
  xmin <- limits[1]
  xmax <- limits[2]
  ymin <- limits[3]
  ymax <- limits[4]
  if (xmin * xmax > 0) {
    cols <- abs(abs(slot(limits, "xmax")) - abs(slot(limits, "xmin")))
  } else {
    cols <- abs(abs(slot(limits, "xmax")) + abs(slot(limits, "xmin")))
  }
  if (ymin * ymax > 0) {
    rows <- abs(abs(slot(limits, "ymax")) - abs(slot(limits, "ymin")))
  } else {
    rows <- abs(abs(slot(limits, "ymax")) + abs(slot(limits, "ymin")))
  }
  #reso <- 60/reso
  rasto <- raster(limits, ncol = cols* reso, nrow = rows * reso, vals = 1)

  rang.ras <- lapply(ra, function(x) rasterize(x, rasto))
  
  for(i in 1:length(rang.ras)){
    rang.ras[[i]][is.na(rang.ras[[i]])] <-0
  }
  
  rang.ras <- stack(rang.ras)
  div.ras <- sum(rang.ras)
  div.ras[div.ras == 0] <- NA
  
  if(terrestrial){
    test <- rasterize(speciesgeocodeR::landmass, div.ras)
    div.ras <- mask(div.ras, test)
  }
  
  return(div.ras)
}