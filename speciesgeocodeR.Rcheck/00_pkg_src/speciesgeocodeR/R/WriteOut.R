WriteOut <- function(x, writetype = c("all", "BioGeoBEARS", "coexistence", "graphs", 
                                      "maps", "nexus", "statistics"), areanames = NULL) {

  if (is.list(x)) {
    if (length(areanames) == 0) {
      areanam <- x[[1]]$areanam
    } else {
      areanam <- areanames
    }
    if ("all" %in% writetype) {
      .NexusOut(x)
      for (i in 1:length(x)) {
        .WriteTablesSpGeo(x[[i]], prefix = names(x)[i])
      }
      if (length(dim(x[[1]]$coexistence_classified)) == 0) {
        warning("no coexistence matrix found")
      } else {
        for (i in 1:length(x)) {
          .OutHeatCoEx(x[[i]], prefix = names(x)[i])
        }
      }
      for (i in 1:length(x)) {
        Spgc2BioGeoBEARS(x[[i]], file = paste(names(x)[i], "_BioGeoBEARS.txt", 
                                         sep = ""))
        .OutPlotSpPoly(x[[i]], prefix = names(x)[i])
        .OutBarChartPoly(x[[i]], prefix = names(x)[i])
        .OutBarChartSpec(x[[i]], prefix = names(x)[i])
        .OutMapAll(x[[i]], prefix = names(x)[i])
        .OutMapPerSpecies(x[[i]], prefix = names(x)[i])
        .OutMapPerPoly(x[[i]], prefix = names(x)[i])
      }
    }
    if ("graphs" %in% writetype) {
      for (i in 1:length(x)) {
        .OutPlotSpPoly(x[[i]], prefix = names(x)[i])
        .OutBarChartPoly(x[[i]], prefix = names(x)[i])
        .OutBarChartSpec(x[[i]], prefix = names(x)[i])
      }
    }
    if ("maps" %in% writetype) {
      for (i in 1:length(x)) {
        .OutMapAll(x[[i]], prefix = names(x)[i])
        .OutMapPerSpecies(x[[i]], prefix = names(x)[i])
        .OutMapPerPoly(x[[i]], areanames = areanames, prefix = names(x)[i])
      }
    }
    if ("statistics" %in% writetype) {
      for (i in 1:length(x)) {
        .WriteTablesSpGeo(x[[i]], prefix = names(x)[i])
      }
    }
    if ("BioGeoBEARS" %in% writetype) {
      for (i in 1:length(x)) {
        Spgc2BioGeoBEARS(x, file = paste(names(x)[i], "BioGeoBEARS.txt", sep = ""))
      }
    }
    if ("nexus" %in% writetype) {
      .NexusOut(x)
    }
    if ("coexistence" %in% writetype) {
      if (length(dim(x[[1]]$coexistence_classified)) == 0) {
        print("No coexistence matrix found")
      } else {
        for (i in 1:length(x)) {
          .OutHeatCoEx(x[[i]], prefix = names(x)[i])
        }
      }
    }
  } else {
    if (length(areanames) == 0) {
      areanam <- x$areanam
    } else {
      areanam <- areanames
    }
    if ("all" %in% writetype) {
      .NexusOut(x)
      .WriteTablesSpGeo(x)
      
      if (length(dim(x$coexistence_classified)) == 0) {
        warning("No coexistence matrix found")
      } else {
        .OutHeatCoEx(x, prefix = "")
      }
      Spgc2BioGeoBEARS(x, file = "BioGeoBEARS.txt")
      .OutPlotSpPoly(x, prefix = "")
      .OutBarChartPoly(x, prefix = "")
      .OutBarChartSpec(x, prefix = "")
      .OutMapAll(x, prefix = "")
      .OutMapPerSpecies(x, prefix = "")
      .OutMapPerPoly(x, areanames = areanam, prefix = "")
    }
    if ("graphs" %in% writetype) {
      .OutPlotSpPoly(x, prefix = "")
      .OutBarChartPoly(x, prefix = "")
      .OutBarChartSpec(x, prefix = "")
    }
    if ("maps" %in% writetype) {
      .OutMapAll(x, prefix = "")
      .OutMapPerSpecies(x, prefix = "")
      .OutMapPerPoly(x, areanames = areanames, prefix = "")
    }
    
    if ("statistics" %in% writetype) {
      .WriteTablesSpGeo(x)
    }
    if ("nexus" %in% writetype) {
      .NexusOut(x)
    }
    if ("BioGeoBEARS" %in% writetype) {
      Spgc2BioGeoBEARS(x, file = "BioGeoBEARS.txt")
    }
    
    if ("coexistence" %in% writetype) {
      if (length(dim(x$coexistence_classified)) == 0) {
        print("No coexistence matrix found")
      } else {
        .OutHeatCoEx(x, prefix = "")
      }
    }
  }
}