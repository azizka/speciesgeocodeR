summary.DESin <- function(object, ...) {
  ares <- split(object[[1]], f = object[[1]]$higherGeography)
  
  list(Number_of_areas = length(ares), 
       Data = data.frame(row.names = c("Timerange_min", "Timerange_max", "Number of records", 
                                       "Mean record age", "Number of taxa",  "Mean taxon age"), 
                         Area_1 = c(min(ares[[1]]$midpointAge), max(ares[[1]]$midpointAge), 
                                    nrow(ares[[1]]), round(mean(ares[[1]]$midpointAge), 1), 
                                    length(unique(ares[[1]]$scientificName)), 
                                    round(mean(aggregate(ares[[1]]$midpointAge, by = list(ares[[1]]$scientificName),min)$x), 1)),
                         Area_2 = c(min(ares[[2]]$midpointAge), max(ares[[2]]$midpointAge),
                                    nrow(ares[[2]]), round(mean(ares[[2]]$midpointAge), 1), length(unique(ares[[2]]$scientificName)),
                                    round(mean(aggregate(ares[[2]]$midpointAge, by = list(ares[[2]]$scientificName), min)$x), 1))), 
       Number_of_Replicates = length(object[[3]]))
}