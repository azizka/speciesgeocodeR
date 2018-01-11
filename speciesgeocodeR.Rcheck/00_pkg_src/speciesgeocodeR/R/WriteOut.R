WriteOut <- function(x, type = "all") {
  
  if (!is.spgeoOUT(x)) {
    switch(type, all = {
      .NexusOut(x)
      for (i in 1:length(x)) {
        .WriteTablesSpGeo(x[[i]], prefix = names(x)[i])
        Spgc2BioGeoBEARS(x[[i]], file = paste(names(x)[i], "_BioGeoBEARS.txt", sep = ""))
        .OutBayArea(x[[i]], prefix = names(x)[i])
        .OutPlotSpPoly(x[[i]], prefix = names(x)[i])
        .OutBarChartPoly(x[[i]], prefix = names(x)[i])
        .OutBarChartSpec(x[[i]], prefix = names(x)[i])
        .OutMapAll(x[[i]], prefix = names(x)[i])
        .OutMapPerSpecies(x[[i]], prefix = names(x)[i])
        .OutMapPerPoly(x[[i]], prefix = names(x)[i])
      }
    }, graphs = {
      for (i in 1:length(x)) {
        .OutPlotSpPoly(x[[i]], prefix = names(x)[i])
        .OutBarChartPoly(x[[i]], prefix = names(x)[i])
        .OutBarChartSpec(x[[i]], prefix = names(x)[i])
      }
    }, maps = {
      for (i in 1:length(x)) {
        .OutMapAll(x[[i]], prefix = names(x)[i])
        .OutMapPerSpecies(x[[i]], prefix = names(x)[i])
        .OutMapPerPoly(x[[i]], prefix = names(x)[i])
      }
    }, stats = {
      for (i in 1:length(x)) {
        .WriteTablesSpGeo(x[[i]], prefix = names(x)[i])
      }
    }, BioGeoBEARS = {
      for (i in 1:length(x)) {
        Spgc2BioGeoBEARS(x, file = paste(names(x)[i], "_BioGeoBEARS.txt", sep = ""))
      }
    }, 
    BayArea = .OutBayArea(x, prefix = names(x)[i]),
    nexus = {
      .NexusOut(x)
    })
  } else {
    switch(type, all = {
      .NexusOut(x)
      .WriteTablesSpGeo(x)
      suppressMessages(Spgc2BioGeoBEARS(x, file = "BioGeoBEARS.txt"))
      .OutBayArea(x, prefix = "")
      .OutPlotSpPoly(x, prefix = "")
      .OutBarChartPoly(x, prefix = "")
      .OutBarChartSpec(x, prefix = "")
      .OutMapAll(x, prefix = "")
      .OutMapPerSpecies(x, prefix = "")
      .OutMapPerPoly(x, prefix = "")
    }, graphs = {
      .OutPlotSpPoly(x, prefix = "")
      .OutBarChartPoly(x, prefix = "")
      .OutBarChartSpec(x, prefix = "")
    }, maps = {
      .OutMapAll(x, prefix = "")
      .OutMapPerSpecies(x, prefix = "")
      .OutMapPerPoly(x, prefix = "")
    }, stats = {
      .WriteTablesSpGeo(x)
    }, nexus = {
      .NexusOut(x)
    }, BioGeoBEARS = {
      Spgc2BioGeoBEARS(x, file = "BioGeoBEARS.txt")
    }, BayArea = .OutBayArea(x, prefix = ""))
  }
}