.adjFormat <- function(x) {
  x <- x[, 1:3]
  names(x) <- c("species", "decimallongitude", "decimallatitude")
  return(x)
}

#barchart of occurrence per polygon per species
.BarChartPoly <- function(x) {
    liste <- names(x$spec_table[-1])
    leng <- length(liste)
    if (leng == 0) {
        cat("No point fell in any polygon")
    } else {
        for (i in 2:leng) {
            subs <- subset(x$spec_table, x$spec_table[, i] > 0)
            datsubs <- subs[order(subs[, i]), ]
            if (nrow(subs) == 0) {
                plot(1:10, 1:10, type = "n", xlab = "", ylab = "Number of occurences")
                text(3, 6, labels = "No species occurred in this polygon.", adj = 0)
                title(liste[i])
            } else {
              ggplot()+
                ggplot2::geom_bar(data = datsubs, 
                                  aes_string(x = "rownames(datsubs)", y = "datsubs[,i]"),
                                  stat = "identity")+
                ggplot2::theme_bw()+
                ggplot2::xlab("Species")+
                ggplot2::ylab("Number of occurrences")+
                ggplot2::ggtitle(liste[i])
            }
        }
    }
}

.BarChartSpec <- function(x, mode = "percent") {
  match.arg(mode)
  switch(mode, 
         percent = {
    dat.plo <- x$spec_table/rowSums(x$spec_table) * 100
    leng <- length(rownames(dat.plo))
    for (i in 1:leng) {
      dat.sub <- as.numeric(as.vector(dat.plo[i, ]))
      ggplot2::ggplot()+
        ggplot2::geom_bar(data = data.frame(dat.sub), 
                          aes_string(x = "names(x$spec_table)", 
                              y = "dat.sub"), stat = "identity")+
        ggplot2::theme_bw()+
        ggplot2::ylab("Percent of occurrences")+ 
        ggtitle(rownames(dat.plo[i, ]))+
        theme(axis.title.x = element_blank())
    }
  }, 
  total = {
    dat.plo <- x$spec_table
    leng <- length(rownames(dat.plo))
    
    for (i in 1:leng) {
      dat.sub <- as.numeric(as.vector(dat.plo[i, ]))
      ggplot2::ggplot()+
        ggplot2::geom_bar(data = data.frame(dat.sub),
                          aes_string(x = "names(x$spec_table)",
                              y = "dat.sub"), stat = "identity")+
        ggplot2::theme_bw()+
        ggplot2::ylab("Number of occurrences")+ 
        ggtitle(rownames(dat.plo[i, ]))+
        theme(axis.title.x = element_blank())
    }
  })
}

.ConvHull <- function(x){
  conv.hull <- chull(x$decimallongitude, x$decimallatitude)
  dat2 <- x[conv.hull, ]
  dat2 <- rbind(dat2[, c(2, 3)], dat2[1, c(2, 3)])
  poly <- Polygons(list(Polygon(dat2)), ID = paste(x[1, 1], "_convhull", sep = ""))
  poly <- SpatialPolygons(list(poly), 
                          proj4string = CRS("+proj=longlat +datum=WGS84"))
  return(poly)
}


.ConvertPoly <- function(x) {
  x <- read.table(x, sep = "\t")
  
  out2 <- vector()
  
  for (j in 1:dim(x)[1]) {
    aa <- as.character(x[j, ])
    ff <- t(aa)
    bb <- unlist(strsplit(ff[1], split = ":"))
    bb <- c(bb[1], unlist(strsplit(bb[2], split = " ")))
    
    out <- c(1, 1, 1)
    
    for (i in 2:length(bb)) {
      dd <- c(bb[1], unlist(strsplit(as.character(bb[i]), split = ",")))
      out <- rbind(out, dd)
    }
    out2 <- rbind(out2, out[-c(1, 2), ])
  }
  
  colnames(out2) <- c("species", "decimallongitude", "decimallatitude")
  rownames(out2) <- NULL
  out2 <- as.data.frame(out2)
  return(out2)
}

.Cord2Polygon <- function(x) {
  if (is.character(x)) {
    tt <- read.table(x, sep = "\t")
  } else {
    tt <- x
  }
  dat <- split(tt, f = tt[, 1])
  col <- lapply(dat, function(k) {
    pp <- Polygon(k[, 2:3])
    po <- Polygons(list(pp), ID = unique(k[, 1]))
  })
  polys <- SpatialPolygons(col)
  pol.data <- data.frame(unique(tt[, 1]), row.names = unique(tt[, 1]))
  names(pol.data) <- names(x)[1]
  polys <- SpatialPolygonsDataFrame(polys, data = pol.data)
  return(polys)
}

.getEle <- function(x) {
  ele <- try(getData("SRTM", lon = round(as.numeric(x[2]), 2), lat = round(as.numeric(x[3]), 2)))
  if (class(ele) == "try-error") {
    elevation <- "NA"
  } else {
    if (!is.na(extract(ele, SpatialPoints(data.frame(round(as.numeric(x[2]), 2), round(as.numeric(x[3]), 2)))))) {
      elevation <- extract(ele, data.frame(round(as.numeric(x[2]), 2), round(as.numeric(x[3]), 2)))
    } else {
      elevation <- "NA"
    }
  }
  return(elevation)
}

.GetElevation <- function(x) {
  if (is.data.frame(x)) {
    inp <- x
  }
  if (class(x) == "spgeoOUT") {
    inp <- data.frame(species = x$species, decimallongitude = x$species_coordinates[, 1], decimallatitude = x$species_coordinates[, 2])
  }
  if (is.character(x) & length(grep(".txt", x)) == 0) {
    if (!requireNamespace("rgbif", quietly = TRUE)) {
      stop("rgbif needed for species name option. Please install it.",
           call. = FALSE)
    }  
    coords <- rgbif::occ_search(scientificName = x, return = "data", 
                                limit = 200000, hasCoordinate = T, spatialIssues = F,
                                fields = c("species", "decimalLongitude","decimalLatitude"))
    coords <- do.call("rbind", coords)
    names(coords) <- c("species", "decimallongitude", "decimallatitude")
    coords <- data.frame(coords[complete.cases(coords),])
    warning(paste(dim(inp)[1], "geo-referenced records found in GBIF; no data cleaning was performed", sep = " "))
  }
  if (is.character(x) & length(grep(".txt", x)) > 0) {
    inp <- read.table(x, sep = "\t", header = T)
    names(inp) <- c("species", "decimallongitude", "decimallatitude")
  }
  
  tt <- list()
  for(i in 1:dim(inp)[1]){
    tt[[i]] <- .getEle(inp[i,])
  }
  
  ele.vector <- suppressWarnings(as.numeric(unlist(tt)))
  
  if (is.character(x) & length(grep(".txt", x)) == 0) {
    ele.vector <- cbind(inp, ele.vector)
    return(ele.vector)
  } else {
    return(ele.vector)
  }
} 

.MapAll <- function(x, buffer = 1) {
      dat <- x$samples
      dat$homepolygon <- as.character(dat$homepolygon)
      dat$homepolygon[dat$homepolygon != "not_classified"] <- "classified"
  
        # prepare background
        e <- raster::extent(SpatialPoints(x$samples[, 2:3])) + buffer
        
        bgmap <- speciesgeocodeR::landmass
        bgmap <- raster::crop(bgmap, e)
        bgmap <- ggplot2::fortify(bgmap)
        
        pols <- ggplot2::fortify(x$polygons)
        
        #plot results
          ggplot2::ggplot()+
          ggplot2::geom_polygon(data = bgmap, 
                                aes_string(x = "long", y = "lat", group = "group"),
                                fill = "grey60")+
          ggplot2::geom_polygon(data = pols,  
                                aes_string(x = "long", y = "lat", group = "group"), 
                                fill = rgb(0, 100,0, 100, maxColorValue = 255))+
          ggplot2::geom_point(data = dat, 
                              aes_string(x = "decimallongitude", y = "decimallatitude", color = "homepolygon"))+
          ggplot2::scale_colour_manual(values = c("blue", "red"))+
          ggplot2::coord_fixed()+ 
          ggplot2::theme_bw()+
          theme(
            legend.title = element_blank()
          )
}

.MapPerPoly <- function(x, areanames, buffer = 1) {
  #background plot data
  e <- raster::extent(SpatialPoints(x$samples[, 2:3])) + buffer
  
  bgmap <- speciesgeocodeR::landmass
  bgmap <- raster::crop(bgmap, e)
  bgmap <- ggplot2::fortify(bgmap)
  
  #backgroundplot
  plo <- ggplot2::ggplot()+
    ggplot2::geom_polygon(data = bgmap, 
                          aes_string(x = "long", y = "lat", group = "group"),
                          fill = "grey60")+
    ggplot2::coord_fixed()+
    ggplot2::theme_bw()
  
  #per polygon plots
  liste <- unique(as.character(x$polygons@data[,areanames]))
  
  outp.li <- list()
  for(i in liste){
    #select polygon
    pols <- x$polygons[x$polygons[[areanames]] == i,]
    pols <- ggplot2::fortify(pols)
    
    #select points
    pts <- x$samples[x$samples$homepolygon == i,]
    
    #plot
    plo2 <- plo+
      ggplot2::geom_polygon(data = pols,
                            aes_string(x = "long", y = "lat", group = "group"), 
                            fill = "grey90")+
      ggplot2::geom_point(data = pts,
                          aes_string(x = "decimallongitude", y = "decimallatitude", colour = "species"))+
      ggplot2::ggtitle(i)+
      theme(legend.position = "none")
    
    outp.li[[i]] <- plo2
  }
  outp.li
}

.MapPerSpecies <- function(x, buffer = 1) {
  # create background plot
  e <- raster::extent(SpatialPoints(x$samples[, 2:3])) + buffer
  
  bgmap <- speciesgeocodeR::landmass
  bgmap <- raster::crop(bgmap, e)
  bgmap <- ggplot2::fortify(bgmap)
  
  pols <- ggplot2::fortify(x$polygons)
  
  # plot results
  plo <- ggplot2::ggplot()+
    ggplot2::geom_polygon(data = bgmap, 
                          aes_string(x = "long",y = "lat", group = "group"), 
                          fill = "grey60")+
    ggplot2::geom_polygon(data = pols,
                          aes_string(x = "long", y = "lat", group = "group"), 
                          fill = rgb(0, 100, 0, 100, maxColorValue = 255))+
    ggplot2::coord_fixed()+
    ggplot2::theme_bw()+ 
    theme(legend.title = element_blank())
  
  # create per species plots
  inp <- x$samples
  inp$homepolygon <- as.character(inp$homepolygon)
  inp$homepolygon[inp$homepolygon != "not_classified"] <- "classified"
  liste <- sort(unique(inp$species))
  for (i in liste) {
    dat <- inp[inp$species == i, ]
    plo2 <- plo+
      ggplot2::geom_point(data = dat,
                          aes_string(x = "decimallongitude", y = "decimallatitude", color = "homepolygon"))+
      ggplot2::scale_colour_manual(values = c("blue", "red"))+
      ggplot2::ggtitle(i)
    print(plo2)
  }
}

.MapUnclassified <- function(x, buffer = 1) {
  
  #pick unclassified species
  dat <- x$samples[x$samples$homepolygon == "not_classified", ]
  if (nrow(dat) == 0) {
    plot(c(1:20), c(1:20), type = "n", axes = F, xlab = "", ylab = "")
    text(10, 10, labels = paste("All points fell into the polygons and were classified.\n", 
                                "No unclassified points", sep = ""))
  } else {
    # prepare background
    e <- raster::extent(SpatialPoints(x$samples[, 2:3])) + buffer
    
    bgmap <- speciesgeocodeR::landmass
    bgmap <- raster::crop(bgmap, e)
    bgmap <- ggplot2::fortify(bgmap)
    
    pols <- ggplot2::fortify(x$polygons)
    pts <- subset(x$samples, x$sampleshomepolygon == "not_classified")
    
    #plot results
    plo <- ggplot2::ggplot()+
      ggplot2::geom_polygon(data = bgmap, 
                            aes_string(x = "long", y = "lat", group = "group"),
                            fill = "grey60")+
      ggplot2::geom_polygon(data = pols,  
                            aes_string(x = "long", y = "lat", group = "group"), 
                            fill = rgb(0, 100,0, 100, maxColorValue = 255))+
      ggplot2::geom_point(data = pts, 
                          aes_string(x = "decimallongitude", y = "decimallatitude", color = "species"))+
      ggplot2::coord_fixed()+ 
      ggplot2::theme_bw()
    return(plo)
  }
}

.NexusOut <- function(dat, verbose = FALSE) {
  if (!is.spgeoOUT(dat)) {
    tablist <- lapply(dat, function(x) x$spec_table)
    for (i in 1:length(tablist)) {
      names(tablist[[i]])[-1] <- paste(names(tablist)[i], names(tablist[[i]][-1]), sep = "_")
    }
    speciestab <- Reduce(function(x, y) merge(x, y, all = TRUE), tablist)
  } else {
    speciestab <- dat$spec_table
  }
  if (!verbose) {
    sink("species_classification.nex")
  }
  if (verbose) {
    sink("species_classification_verbose.nex")
  }
  cat("#NEXUS \n")
  cat("\n")
  cat("begin data; \n")
  cat(paste("\tdimensions ntax=", dim(speciestab)[1], " nchar=", dim(speciestab)[2] - 1, ";", sep = ""))
  cat("\n")
  cat("\tformat datatype=standard symbols=\"01\" gap=-;")
  cat("\n")
  cat("\tCHARSTATELABELS")
  cat("\n")
  if (length(speciestab) == 0) {
    cat("No point fell in any of the polygons specified")
    sink(NULL)
  } else {
    aa <- gsub(" ", "_", names(speciestab))
    aa <- gsub("&", "_", aa)
    aa <- gsub("__", "_", aa)
    aa <- gsub("__", "_", aa)
    bb <- seq(1, length(aa))
    
    cat(paste("\t", bb[-length(bb)], " ", aa[-length(aa)], ",\n", sep = ""))
    cat("\t", paste(bb[length(bb)], " ", aa[length(aa)], ";\n", sep = ""))
    cat("\n")
    cat("\tmatrix\n")
    
    dd <- as.matrix(speciestab)
    dd[dd > 0] <- 1
    
    if (ncol(dd) > 1) {
      dd <- data.frame(dd)
      dd$x <- apply(dd[, names(dd)], 1, paste, collapse = "")
    } else {
      dd <- data.frame(dd, x = dd)
    }
    ff <- gsub(" ", "_", rownames(speciestab))
    
    if (!verbose) {
      ee <- paste("\t\t", ff, "\t", dd$x, "\n", sep = "")
      cat(ee)
    }
    if (verbose) {
      gg <- vector()
      jj <- speciestab
      for (i in 1:ncol(jj)) {
        hh <- paste(dd[, i], "[", jj[, i], "]", sep = "")
        gg <- data.frame(cbind(gg, hh))
      }
      gg$x <- apply(gg[, names(gg)], 1, paste, collapse = "")
      ee <- paste("\t\t", ff, "\t", gg$x, "\n", sep = "")
      cat(ee)
    }
    cat("\t;\n")
    cat("end;")
    sink(NULL)
  }
}

.OutBarChartPoly <- function(x, path, prefix, verbose = FALSE, ...) {
    if (verbose) {
        cat("Creating barchart per polygon: barchart_per_polygon.pdf. \n")
    }
  if(missing(path)){
    path <- getwd()
  }
    pdf(file = file.path(path, paste(prefix, "barchart_per_polygon.pdf", sep = "")),
        paper = "special", width = 10.7, height = 7.2, onefile = T)
    .BarChartPoly(x)
    dev.off()
}

.OutBarChartSpec <- function(x, path, prefix, verbose = FALSE, ...) {
    if (verbose) {
        cat("Creating barchart per species: barchart_per_species.pdf. \n")
    }
  if(missing(path)){
    path <- getwd()
  }
    pdf(file = file.path(path, paste(prefix, "barchart_per_species.pdf", sep = "")),
        paper = "special", width = 10.7, height = 7.2, onefile = T)
    .BarChartSpec(x, mode = "percent")
    dev.off()
}

.OutMapAll <- function(x, path, prefix, areanames = "", verbose = FALSE, ...) {
    if (verbose) {
        cat("Creating overview map: map_samples_overview.pdf. \n")
    }
  if(missing(path)){
    path <- getwd()
  }
    pdf(file = file.path(path, paste(prefix, "map_samples_overview.pdf", sep = "")),
        paper = "special", width = 10.7, height = 7.2, onefile = T, 
        ...)
    .MapAll(x, ...)
    .MapUnclassified(x, ...)
    dev.off()
}

.OutMapPerPoly <- function(x, path, prefix, verbose = FALSE, ...) {
    if (verbose) {
        cat("Creating map per polygon: map_samples_per_polygon.pdf. \n")
    }
  if(missing(path)){
    path <- getwd()
  }
    pdf(file.path(path, file = paste(prefix, "map_samples_per_polygon.pdf", sep = "")),
        paper = "special", width = 10.7, height = 7.2, onefile = T)
    .MapPerPoly(x, ...)
    dev.off()
}

.OutMapPerSpecies <- function(x, path, prefix, verbose = FALSE, ...) {
    if (verbose) {
        cat("Creating map per species: map_samples_per_species.pdf. \n")
    }
  if(missing(path)){
    path <- getwd()
  }
    pdf(file = file.path(path, paste(prefix, "map_samples_per_species.pdf", sep = "")),
        paper = "special", width = 10.7, height = 7.2, onefile = T)
    .MapPerSpecies(x, ...)
    dev.off()
}

.OutPlotSpPoly <- function(x, path, prefix, verbose = FALSE, ...) {
    if (verbose) {
        cat("Creating species per polygon barchart: number_of_species_per_polygon.pdf. \n")
    }
  if(missing(path)){
    path <- getwd()
  }
    pdf(file = file.path(path, paste(prefix, "number_of_species_per_polygon.pdf", sep = "")),
        paper = "special", width = 10.7, height = 7.2, 
        onefile = T)
    .PlotSpPoly(x)
    dev.off()
}

.PipSamp <- function(x, columnname, verbose = FALSE) {

    if (class(x$polygons) == "SpatialPolygonsDataFrame") {
        if (any(is.na(x$polygons@data[, columnname]))) {
            stop("area names contain missing data (#N/A). Renamed to  unnamed. This migh cause problems")
        }
      occ <- SpatialPoints(x$species_coordinates[, c(1, 2)])

        outp <- as.character(over(occ, x$polygon)[, columnname])
        outp[is.na(outp)] <- "not_classified"
        return(outp)
    } else {
        if (verbose) {
            cat("Performing point in polygon test \n")
        }
        pip <- over(occ, x$polygons)
        if (verbose) {
            cat("Done \n")
        }
        for (i in 1:length(names(x$polygons))) {
            pip$homepolygon[pip$homepolygon == i] <- names(x$polygons)[i]
        }
        return(pip)
    }
}

.PlotSpPoly <- function(x) {
  dat.plo <- data.frame(x$polygon_table)
  ggplot()+
    ggplot2::geom_bar(data = dat.plo, 
                      aes_string(x = "rownames(dat.plo)", y = "dat.plo[,1]"),
                      stat = "identity")+
    ggplot2::theme_bw()+ 
    ggplot2::ylab("Species")+
    ggplot2::theme(axis.title.x = element_blank(),
                   axis.text.x = element_text(angle = 90, hjust = 1))
}

.rasterSum <- function(x, ras, type) {
    po <- SpatialPoints(x[, 2:3])
    ras_sub <- rasterize(po, ras, fun = "count")
    if(type == "div"){
      ras_sub[ras_sub >= 1] <- 1
    }
    ras_sub[is.na(ras_sub)] <- 0
    return(ras_sub)
}

.ReadPoints <- function(x, y, areanames = NA, verbose = FALSE) {
  res <- list()
  
  if (all(!is.character(x),!is.data.frame(x))) {
    stop(sprintf("function not defined for class %s", dQuote(class(x))))
  }
  
  if (is.character(x) & length(grep(".txt", x)) == 0) {
    if (!requireNamespace("rgbif", quietly = TRUE)) {
      stop("rgbif needed for species name option. Please install it.", 
           call. = FALSE)
    }
    coords <- rgbif::occ_search(scientificName = x, return = "data", limit = 2e+05, 
                                hasCoordinate = T, spatialIssues = F, fields = c("species", "decimalLongitude", 
                                                                                 "decimalLatitude"))
    coords <- do.call("rbind", coords)
    coords <- data.frame(coords[complete.cases(coords), ])
  }
  
  if (is.character(x) & length(grep(".txt", x)) > 0) {
    coords <- read.table(x, sep = "\t", header = T, row.names = NULL)
  }
  
  if (is.data.frame(x)) {
    names(x) <- tolower(names(x))
    if(ncol(x) > 3 & all(c("species", "decimallatitude", "decimallongitude") %in% names(x))){
      coords <- x[, c("species", "decimallongitude", "decimallatitude")]
      rownames(coords) <- 1:nrow(coords)
    }else{
      coords <- x
      rownames(coords) <- 1:nrow(coords)
    }
  if(ncol(coords) < 3){
    stop(paste("wrong input format: \n", "Inputfile for coordinates must have at least three columns", 
               sep = ""))
  }
  }
  
  if (is.character(y) | is.data.frame(y)) {
    if (is.character(y) & length(grep(".shp", y)) > 0) {
      poly <- maptools::readShapeSpatial(y)
    } else {
      if (is.character(y)) {
        polycord <- read.table(y, sep = "\t", header = T)
      }
      if (is.data.frame(y)) {
        polycord <- y
      }
      if (ncol(polycord) != 3) {
        stop("Wrong input format;\ninputfile for polygons must be a tab-delimited text file with three columns")
      }
      if (!is.numeric(polycord[, "decimallongitude"]) || !is.numeric(polycord[, "decimallatitude"])) {
        stop("wrong input format:\nInput polygon coordinates (columns 2 and 3) must be numeric.")
      }
      if (!is.character(polycord[, "species"]) && !is.factor(polycord[, "species"])) {
        warning("polygon identifier (column 1) should be a string or a factor")
      }
      if (max(polycord[, "decimallongitude"]) > 180) {
        warning(sprintf("check polygon input coordinates; file contains longitude values outside possible range in row: \n                      %s\n Coordinates set to maximum: 180.\n", 
                        rownames(polycord[polycord[, 2] > 180, ])))
        polycord[polycord[, "decimallongitude"] > 180, "decimallongitude"] <- 180
      }
      if (min(polycord[, "decimallongitude"]) < -180) {
        warning(paste("check polygon input coordinates. File contains longitude values outside possible range in row: ", 
                      rownames(polycord[polycord[, "decimallongitude"] < -180, ]), "\n", "Coordinates set to minimum: -180", 
                      sep = ""))
        polycord[polycord[, "decimallongitude"] < -180, ] <- -180
        
      }
      if (max(polycord[, "decimallatitude"]) > 90) {
        warning(paste("check polygon input coordinates. File contains latitude values outside possible range in row:", 
                      rownames(polycord[polycord[, "decimallatitude"] > 90, ]), "\n", "Coordinates set to maximum: 90", 
                      sep = ""))
        polycord[polycord[, "decimallatitude"] > 90, "decimallatitude"] <- 90
      }
      if (min(polycord[, "decimallatitude"]) < -90) {
        warning(paste("check polygon input coordinates. File contains latitude values outside possible range in row:", 
                      rownames(polycord[polycord[, "decimallatitude"] < -90, ]), "\n", "Coordinates set to minimum: -90", 
                      sep = ""))
        polycord[polycord[, "decimallatitude"] < -90, "decimallatitude"] <- -90
      }
      poly <- .Cord2Polygon(polycord)
    }
  }
  
  if (class(y) == "SpatialPolygonsDataFrame") {
    poly <- y
  }
  
  if (!is.numeric(coords[, "decimallongitude"]) || !is.numeric(coords[, "decimallatitude"])) {
    stop(paste("wrong input format: \n", "Input point coordinates (columns 2 and 3) must be numeric", 
               sep = ""))
  }
  
  if (max(coords[, "decimallongitude"]) > 180) {
    warning(paste("longitude values outside possible range in row:", 
                  rownames(coords[coords[, "decimallongitude"] > 180, ]), ". ", "Row deleted", sep = ""))
    coords <- coords[!coords[, "decimallongitude"] > 180, ]
  }
  if (min(coords[, "decimallongitude"]) < -180) {
    warning(paste("longitude values outside possible range in row: ", 
                  rownames(coords[coords[, "decimallongitude"] < -180, ]), ". ", "Row deleted", sep = ""))
    coords <- coords[!coords[, "decimallongitude"] < -180, ]
  }
  if (max(coords[, "decimallatitude"]) > 90) {
    warning(paste("latitude values outside possible range in row:", 
                  rownames(coords[coords[, "decimallatitude"] > 90, ]), ". ", "Row deleted", sep = ""))
    coords <- coords[!coords[, "decimallatitude"] > 90, ]
  }
  if (min(coords[, "decimallatitude"]) < -90) {
    warning(paste("latitude values outside possible range in row:", 
                  rownames(coords[coords[, "decimallatitude"] < -90, ]), ". ", "Row deleted", sep = ""))
    coords <- coords[!coords[, "decimallatitude"] < -90, ]
  }
  if (!is.character(coords[, "species"]) && !is.factor(coords[, "species"])) {
    warning("species name (column 1) should be a string or a factor")
  }
  
  coords[, 1] <- as.factor(coords[, "species"])
  coordi <- coords[, c(2, 3)]
  names(coordi) <- c("decimallongitude", "decimallatitude")
  
  areanam <- areanames
  
  res <- list(species = coords[, 1], species_coordinates = coordi, polygons = poly, 
              areanam = areanam)
  class(res) <- "spgeoIN"
  return(res)
  
} 

.SpGeoCodH <- function(x, areanames = NULL, occ.thresh = 0) {
    if (class(x$polygons) == "SpatialPolygonsDataFrame") {
      if (!areanames %in% names(x$polygons@data)){
        stop(sprintf("column '%s' not found", areanames))
      }  
      nam.test <- as.vector(unlist(x$polygons@data[, areanames]))
      if ("NA" %in% nam.test) {
        warning("the polygondata contain a polygon named NA. this can cause problems. Please rename")
      }
    }
    #point in polygon test
    kkk <- .PipSamp(x, columnname = areanames)
    
    #number of records per species per polygon, everything belo occ.thresh is discarded
    spsum <- .SpSumH(kkk, y = x$species, occ.thresh = occ.thresh)
    
    if (length(spsum) == 0) {
      namco <- c("species", names(x$polygons))
      fill <- matrix(0, nrow = length(unique(kkk)), ncol = length(names(x$polygons)))
      fill <- data.frame(fill)
      spsum <- data.frame(cbind(as.character(unique(kkk)), fill))
      names(spsum) <- namco
    }
    
    #species number per polygon
    sppol <- spsum
    sppol[sppol > 1] <- 1
    sppol <- colSums(sppol)
    
    #SpatialPolygonsDataFrame with species number per polygon based on areanam
    pol.df <- as(x$polygons, "data.frame")
    nums <- data.frame(sppol)
    pol.df.m <- merge(pol.df, nums, sort = FALSE, by.x = areanames,
                    by.y = "row.names", all.x = TRUE)
    pol.df.m <- pol.df.m[match(pol.df[, areanames], pol.df.m[, areanames]),]
    rownames(pol.df.m) <- rownames(pol.df)
    pol.df.m[is.na(pol.df.m$sppol), "sppol"] <- 0
    pol <- SpatialPolygonsDataFrame(as(x$polygons, "SpatialPolygons"), data = pol.df.m)
    
    #create output
    out <- list(samples = data.frame(species = x$species,
                                     decimallongitude = x$species_coordinates[,1],
                                     decimallatitude = x$species_coordinates[,2],
                                     homepolygon = kkk),
                polygons = pol, 
                spec_table = spsum, 
                polygon_table = sppol,
                areanam = areanames)
    class(out) <- "spgeoOUT"
    return(out)
} 

.SpSumH <- function(x, y, occ.thresh = occ.thresh) {
  liste <- as.character(na.omit(unique(x)))
  if (length(liste) == 0) {
    spec_sum <- NULL
  } else {
    dat <- data.frame(x,y)
    spec_sum <- as.data.frame.matrix(t(table(dat)))

    if (occ.thresh > 0) {
      filtperc <- apply(spec_sum, 2, function(k){k / rowSums(spec_sum) * 100})
      spec_sum[filtperc < occ.thresh] <- 0
    }
  }
  return(spec_sum)
} 

.WriteTablesSpGeo <- function(x, path, prefix = "", verbose = FALSE) {
      if(missing(path)){
        path <- getwd()
      }
        write.table(x$samples, file = file.path(path, paste(prefix, "sample_classification_to_polygon.txt", sep = "")),
                    row.names = FALSE, sep = "\t")
        write.table(x$spec_table, file = file.path(path, paste(prefix, "species_occurences_per_polygon.txt", sep = "")), 
                    row.names = FALSE, sep = "\t")
        write.table(x$polygon_table, file = file.path(path, paste(prefix, "speciesnumber_per_polygon.txt", sep = "")),
                    row.names = FALSE, sep = "\t")
        write.table(x$samples[x$samples$homepolygon == "not_classified",], 
                    file = file.path(path, paste(prefix, "unclassified samples.txt", sep = "")), row.names = FALSE, sep = "\t")
        write.table(x$coexistence_classified, file = file.path(path, paste(prefix, "species_coexistence_matrix.txt", sep = "")),
                    row.names = FALSE, sep = "\t")
} 
