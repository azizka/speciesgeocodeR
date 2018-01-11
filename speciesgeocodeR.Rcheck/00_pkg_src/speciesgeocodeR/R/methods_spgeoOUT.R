as.data.frame.spgeoOUT <- function(x, ...){
  data.frame(x$spec_table)
}

is.spgeoOUT <- function(x){
  inherits(x, "spgeoOUT")
}

plot.spgeoOUT <- function(x, type = "summary", mode = "percent", ...) {
  switch(type,
         summary = .MapAll(x),
         species = .BarChartSpec(x, mode = mode),
         polygons = .BarChartPoly(x),
         speciesrichness = .PlotSpPoly(x),
         mapspecies = .MapPerSpecies(x),
         mappolygons = .MapPerPoly(x),
         mapunclassified = .MapUnclassified(x),
         mapall = .MapAll(x)
  )
} 

summary.spgeoOUT <- function(object, ...) {
  #General summary
  spnum <- length(unique(object$samples$species))
  sampnum <- nrow(object$samples)
  polnum <- length(object$polygons)
  
  suma <- c(spnum, sampnum, polnum)
  
  #Coordinate summary
  coords <- round(object$samples[,c("decimallongitude", "decimallatitude")], 2)
  coords <- c(min(coords$decimallongitude), max(coords$decimallongitude), round(mean(coords$decimallongitude), 2),
              min(coords$decimallatitude), max(coords$decimallatitude), round(mean(coords$decimallatitude), 2))
  
  #polygon names
  if (is.na(object$areanam)){
    polys <- unlist(lapply(slot(object$polygons, "polygons"), function(x) slot(x, "ID")))
  }else{
    polys <- unique(as.character(data.frame(object$polygons)[, object$areanam]))
  }
  
  #number of species per polygon
  sppol <- c(median(object$polygon_table),
             min(object$polygon_table),
             max(object$polygon_table))
  
  #not classified species
  nc <- c(object$polygon_table[["not_classified"]], sum(object$samples$homepolygon == "not_classified"))
  
  out <- list(suma = suma, coords = coords, polys = polys, sppol = sppol, nc = nc)

  class(out) <- c("summary.spgeoOUT", class(out))
  
  return(out)
} 

print.summary.spgeoOUT <- function(x,...){
  #general summary
  gs <- sprintf("%s species with %s occurrences classified to %s input polygons \n\n", x$suma[1], 
            x$suma[2], x$suma[3])
  
  #Coordinate summary
  cs <- sprintf("Longitudinal range: %s to %s, mean %s\nLatitudinal range: %s to %s, mean %s \n\n", 
                x$coords[1], x$coords[2], x$coords[3],
                x$coords[4], x$coords[5], x$coords[6])
  
  #Number of species per polygon
  sp <- sprintf("Median number of species per polygon = %s; min = %s, max = %s\n\n", 
                x$sppol[1], x$sppol[2], x$sppol[3])
  
  #not classified records and species
  nc <-sprintf("%s species (%s records) not classified to any polygon\n", x$nc[1], x$nc[2])
  
  cat(gs)
  cat(cs)
  cat(paste("Polygon names = ", paste(x$polys, collapse = " "), "\n\n"))
  cat(sp)
  cat(nc)
}
