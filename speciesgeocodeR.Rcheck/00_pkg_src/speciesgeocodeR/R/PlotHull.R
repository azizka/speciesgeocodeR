PlotHull <- function(dat, select = "all", buffer = 1, bgmap,
                     col = rgb(255, 0, 0, 10, maxColorValue = 255)) {
  
  # create backgroundmap
  e <- raster::extent(dat) + buffer
  
  if (missing(bgmap)) {
    bgmap <- speciesgeocodeR::landmass
    bgmap <- raster::crop(bgmap, e)
  }
  
  bgmap <- ggplot2::fortify(bgmap)
  plo <- ggplot2::ggplot()+
    ggplot2::geom_polygon(data = bgmap, 
                          aes_string(x = "long", y = "lat", group = "group"), 
                          fill = "white", color = "grey20")+
    ggplot2::coord_fixed()+ 
    ggplot2::theme_bw()
  
  # plot polygons
  if (select == "all") {
    dat.plo <- fortify(dat)
    plo+
      ggplot2::geom_polygon(data = dat.plo,
                            aes_string(x = "long", y = "lat",
                                       group = "group"), fill = col)
    
  } else {
    dat.plo <- ggplot2::fortify(subset(dat, dat$species %in% select))
    plo <- plo+ 
      ggplot2::geom_polygon(data = dat.plo, 
                            aes_string(x = "long", y = "lat", group = "group"), 
                            fill = col)
    if (length(select) == 1) {
      plo <- plo + ggplot2::ggtitle(select)
    }
    plo
  }

}