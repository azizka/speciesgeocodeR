as.data.frame.spgeoOUT <- function(x, ...){
  data.frame(x$spec_table)
}

is.spgeoOUT <- function(x){
  inherits(x, "spgeoOUT")
}

plot.spgeoOUT <- function(x, plottype = "summary", mode = c("percent", "total")) {
  switch(plottype,
         summary = {
           layout(matrix(c(1, 1, 2, 2), 2, 2))
           .MapAll(x)
           .PlotSpPoly(x)
           layout(matrix(1, 1, 1))
         },
         species = .BarChartSpec(x, mode = mode),
         polygons = .BarChartPoly(x),
         speciesrichness = .PlotSpPoly(x),
         mapspecies = .MapPerSpecies(x),
         mappolygond = .MapPerPoly(x, areanames = x$areanam),
         mapunclassified = .MapUnclassified(x),
         mapall = .MapAll(x)
  )
} 

summary.spgeoOUT <- function(object, ...) {
  suma <- paste(length(unique(object$species_in)), " species with ", dim(object$species_coordinates_in)[1], 
                " occurrence points and ", length(object$polygons), " input polygons.", sep = "")
  coords <- summary(object$species_coordinates)
  if (is.na(areanames) & length(object$areanam) != 0){
    areanames <- object$areanam
  }  
  if (is.na(object$areanam)){
    polys <- unlist(lapply(slot(object$polygons, "polygons"), function(x) slot(x, "ID")))
  }else{
    polys <- unique(as.character(data.frame(object$polygons)[, object$areanam]))
  }
  
  inf <- list(overall = suma, 
              species_coordinates = coords, 
              polygons = polys,
              species_number_per_polygon = t(data.frame(Mean = mean(as.numeric(object$polygon_table)),
                                                        Median = median(as.numeric(object$polygon_table)), 
                                                        Max = max(object$polygon_table), 
                                                        Min = min(object$polygon_table))), 
              not_classified_samples = paste(dim(object$not_classified_samples)[1], " occurrences did not fall in any polygon", sep = ""))
  return(inf)
} 


