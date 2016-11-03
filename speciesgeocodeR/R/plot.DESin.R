plot.DESin <- function(x, plottype = c("all", "samplelocations", "inputviz", "replicates"), 
                       xlim = c(-180, 180), ylim = c(-90, 90), pch = 1, ...) {
  match.arg(plottype)
  
  if(!all(c("decimalLongitude", "decimalLatitude") %in% names(x[[1]]))){
    warning("no coordinates found, no locations are plotted")
  }
  
  if ("all" %in% plottype) {
    .SampleLocations(x, ...)
    .InputData(x, ...)
    .ReplicateAges(x)
  } else {
    
    if ("samplelocations" %in% plottype) {
      .SampleLocations(x, ...)
    }
    if ("occurrencetimes" %in% plottype) {
      .InputData(x, ...)
    }
    if ("inputviz" %in% plottype) {
      .InputData(x, ...)
    }
    if ("replicates" %in% plottype) {
      .ReplicateAges(x)
    }
  }
}
