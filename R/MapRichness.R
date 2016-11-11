MapRichness <- function(x, buffer = 1, show.occ = F) {
  if (!is.spgeoOUT(x)) {
    stop("this function is only defined for class \"spgeoOUT\"")
  }
  # background map
  e <- raster::extent(x$polygons) + buffer
  bgmap <- speciesgeocodeR::landmass
  bgmap <- raster::crop(bgmap, e)
  bgmap <- ggplot2::fortify(bgmap)
  
  # link species number information with polygons
  pol <- ggplot2::fortify(model = x$polygons)
  pol <- merge(pol.f, x$polygons@data, by.x = "id", by.y = "row.names")
  pol$sppol <- as.integer(pol$sppol)
  
  #create plot
  plo.out <- ggplot2::ggplot()+
    ggplot2::geom_polygon(data = bgmap, 
                          aes_string(x = "long", y = "lat", group = "group"), 
                          col = "grey60")+
    ggplot2::geom_polygon(data = pol, 
                          aes_string(x = "long", y = "lat", group = "group",
                                     fill = "sppol"))+
    viridis::scale_fill_viridis(option = "viridis", name = "Species")+
    ggplot2::coord_fixed()+
    ggplot2::theme_bw()+
    ggplot2::theme(
      axis.title = element_blank()
    )
  
  if (show.occ) {
    plo.out+
      ggplot2::geom_point(data = x$samples, 
                          aes_string(x = "decimallongitude", y = "decimallatitude"),
                 col = "red")
  }
  return(plo.out)
}