.adjFormat <- function(x) {
  x <- x[, 1:3]
  names(x) <- c("species", "decimallongitude", "decimallatitude")
  return(x)
}

#barchart of occurrence per polygon per specie
.BarChartPoly <- function(x, plotout = F, verbose = FALSE, ...) {
    if (plotout) {
        par(ask = F)
    }
    if (!plotout) {
      par(ask = T)
    }
    liste <- names(x$spec_table)
    leng <- length(liste)
    if (length(names(x$spec_table)) == 0) {
        cat("No point fell in any polygon")
    } else {
        for (i in 2:leng) {
            subs <- subset(x$spec_table, x$spec_table[, i] > 0)
            datsubs <- subs[order(subs[, i]), ]
            if (dim(subs)[1] == 0) {
                plot(1:10, 1:10, type = "n", xlab = "", ylab = "Number of occurences")
                text(3, 6, labels = "No species occurred in this polygon.", adj = 0)
                title(liste[i])
            } else {
                barplot(datsubs[, i], names.arg = datsubs$species, 
                        las = 2, ylab = "Number of occurences", cex.names = 0.7,
                        ylim = c(0, (max(datsubs[, i]) + max(datsubs[, i])/10)))  #, ...)
                box("plot")
                title(liste[i])
            }
        }
    }
    par(ask = F)
    
}

.BarChartSpec <- function(x, mode = c("percent", "total"), plotout = FALSE, verbose = FALSE, ...) {
    match.arg(mode)
    if (length(x$spec_table) == 0) {
        cat("No point was found inside the given polygons")
    } else {
        if (!plotout) {
            par(ask = T)
        }
        if (any(mode == "total")) {
            liste <- x$spec_table$species
            leng <- length(liste)
            for (i in 1:leng) {
                if (verbose) {
                  cat(paste("Creating barchart for species ", i, "/", leng, ": ", liste[i], "\n", sep = ""))
                }
                spsub <- as.matrix(subset(x$spec_table, x$spec_table$species == liste[i])[, 2:dim(x$spec_table)[2]])
                if (sum(spsub) > 0) {
                  barplot(spsub, las = 2, ylim = c(0, (max(spsub) + max(spsub)/10)), ylab = "Number of occurrences", ...)
                  title(liste[i])
                  box("plot")
                }
            }
        }
        if (any(mode == "percent")) {
            percent <- x$spec_table[, -1]
            anzpoly <- length(names(x$spec_table)[-1])
            if (anzpoly > 1) {
                percent2 <- percent/rowSums(percent) * 100
            } else {
                percent2 <- percent/sum(percent) * 100
            }
            percent2[percent2 == "NaN"] <- 0
            percent2 <- data.frame(species = x$spec_table[, 1], percent2)
            
            liste <- x$spec_table$species
            leng <- length(liste)
            leng2 <- length(colnames(percent2))

            for (i in 1:leng) {
                if (verbose) {
                  cat(paste("Creating barchart for species ", i, "/", leng, ": ", liste[i], "\n", sep = ""))
                }
                if (anzpoly > 1) {
                  spsub <- as.matrix(subset(percent2, percent2$species == liste[i])[, 2:leng2])
                } else {
                  spsub <- as.matrix(percent2[percent2$species == liste[i], ][, 2:leng2])
                  names(spsub) <- names(x$spec_table)[-1]
                }
                if (sum(spsub) > 0) {
                  barplot(spsub, las = 2, ylim = c(0, (max(spsub) + max(spsub)/10)), ylab = "Percent of occurrences", names.arg = names(spsub), 
                    ...)
                  title(liste[i])
                  box("plot")
                }
            }
        }
        par(ask = F)
    }
}

.ConvHull <- function(x){
  conv.hull <- chull(x$decimallongitude, x$decimallatitude)
  dat2 <- x[conv.hull, ]
  dat2 <- rbind(dat2[, c(2, 3)], dat2[1, c(2, 3)])
  poly <- SpatialPolygons(list(Polygons(list(Polygon(dat2)), ID = paste(x[1, 1], "_convhull", sep = ""))), proj4string = CRS("+proj=longlat +datum=WGS84"))
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
        if (nrow(tt) != 3) {
            stop(paste("wrong input format: \n", "Inputobject must be a tab-delimited text file or a data.frame with three columns", 
                sep = ""))
        }
        if (!is.numeric(tt[, 2]) || !is.numeric(tt[, 3])) {
            stop(paste("wrong input format: \n", "Input coordinates (columns 2 and 3) must be numeric", sep = ""))
        }
        if (!is.character(tt[, 1]) && !is.factor(tt[, 1])) {
            warning("input species (column 1) should be a string or a factor")
        }
        names(tt) <- c("species", "lon", "lat")
        liste <- levels(tt$species)
        col <- list()
        for (i in 1:length(liste)) {
            pp <- subset(tt, tt$species == liste[i])[, c(2, 3)]
            pp <- Polygon(pp)
            po <- Polygons(list(pp), ID = liste[i])
            col[[i]] <- po
        }
        polys <- SpatialPolygons(col, proj4string = CRS("+proj=longlat +datum=WGS84"))
    } else {
        tt <- x
        if (dim(tt)[2] != 3) {
            stop(paste("wrong input format: \n", "Inputobject must be a tab-delimited text file or a data.frame with three columns", 
                sep = ""))
        }
        if (!is.numeric(tt[, 2]) || !is.numeric(tt[, 3])) {
            stop(paste("wrong input format: \n", "Input coordinates (columns 2 and 3) must be numeric", sep = ""))
        }
        if (!is.character(tt[, 1]) && !is.factor(tt[, 1])) {
            warning("input species (column 1) should be a string or a factor")
        }
        names(tt) <- c("species", "lon", "lat")
        liste <- levels(tt$species)
        col <- list()
        for (i in 1:length(liste)) {
            pp <- subset(tt, tt$species == liste[i])[, c(2, 3)]
            pp <- Polygon(pp)
            po <- Polygons(list(pp), ID = liste[i])
            col[[i]] <- po
        }
        polys <- SpatialPolygons(col, proj4string = CRS("+proj=longlat +datum=WGS84"))
    }
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

.HeatPlotCoEx <- function(x, verbose = FALSE, ...) {
  if (class(x) == "spgeoOUT") {
    dat <- x$coexistence_classified
  } else {
    dat <- x
  }
  if (dim(dat)[1] > 40) {
    warning("more than 40 species in coexistence matrix. Plot might be unreadable")
  }
  if (class(dat) != "data.frame") {
    stop("wrong input format. Input must be a \"data.frame\"")
  }
  if (dim(dat)[2] != (dim(dat)[1] + 1)) {
    warning("suspicicous data dimensions, check input file")
  }
  ymax <- dim(dat)[1]
  xmax <- dim(dat)[2]
  colo <- rev(heat.colors(10))
  numer <- rev(1:ymax)
  
  layout(matrix(c(rep(1, 9), 2), ncol = 1, nrow = 10))
  par(mar = c(0, 10, 10, 0))
  plot(0, xlim = c(0, xmax - 1), ylim = c(0, ymax), type = "n", axes = F, xlab = "", ylab = "")
  for (j in 2:xmax) {
    for (i in 1:ymax) {
      if (i == (j - 1)) {
        rect(j - 2, numer[i] - 1, j - 1, numer[i], col = "black")
      } else {
        ind <- round(dat[i, j]/10, 0)
        if (ind == 0) {
          rect(j - 2, numer[i] - 1, j - 1, numer[i], col = "white")
        } else {
          rect(j - 2, numer[i] - 1, j - 1, numer[i], col = colo[ind])
        }
      }
    }
  }
  axis(side = 3, at = seq(0.5, (xmax - 1.5)), labels = colnames(dat)[-1], las = 2, cex.axis = 0.7, pos = ymax)
  axis(2, at = seq(0.5, ymax), labels = rev(dat$species), las = 2, cex.axis = 0.7, pos = 0)
  title("Species co-occurrence", line = 9)
  
  par(mar = c(0.5, 10, 0, 0))
  plot(c(1, 59), c(1, 12), type = "n", axes = F, ylab = "", xlab = "")
  text(c(13, 13), c(10, 7), c("0%", "10%"))
  text(c(20, 20), c(10, 7), c("20%", "30%"))
  text(c(27, 27), c(10, 7), c("40%", "50%"))
  text(c(34, 34), c(10, 7), c("60%", "70%"))
  text(c(41, 41), c(10, 7), c("80%", "90%"))
  text(c(48), 10, "100%")
  rect(c(9, 9, 16, 16, 23, 23, 30, 30, 37, 37, 44), c(rep(c(10.7, 7.7), 5), 10.7), 
       c(11, 11, 18, 18, 25, 25, 32, 32, 39, 39, 46), c(rep(c(8.7, 
                                                                                                                                          5.7), 5), 8.7), col = c("white", colo))
  rect(7, 5, 51, 12)
  layout(matrix(1, 1, 1))
} 

.MapAll <- function(x, polyg, moreborders = FALSE, verbose = FALSE, ...) {
    # data('wrld_simpl', envir = environment())
    if (class(x) == "spgeoOUT") {
        xmax <- min(max(x$species_coordinates_in[, 1]) + 2, 180)
        xmin <- max(min(x$species_coordinates_in[, 1]) - 2, -180)
        ymax <- min(max(x$species_coordinates_in[, 2]) + 2, 90)
        ymin <- max(min(x$species_coordinates_in[, 2]) - 2, -90)
        difx <- sqrt(xmax^2 + xmin^2)
        dify <- sqrt(ymax^2 + ymin^2)
        if (difx > 90) {
            xmax <- min(xmax + 10, 180)
            xmin <- max(xmin - 10, -180)
            ymax <- min(ymax + 10, 90)
            ymin <- max(ymin - 10, -90)
        }
        if (verbose == TRUE) {
            warning("creating map of all samples")
        }
        map("world", xlim = c(xmin, xmax), ylim = c(ymin, ymax))
        axis(1)
        axis(2)
        box("plot")
        title("All samples")
        # if (moreborders == T) {plot(wrld_simpl, add = T)}
        if (verbose) {
            warning("adding polygons")
        }
        plot(x$polygons, col = "grey60", border = "grey40", add = T, ...)
        if (verbose) {
            warning("adding sample points")
        }
        points(x$species_coordinates_in[, 1], x$species_coordinates_in[, 2], cex = 0.7, pch = 3, col = "blue", ...)
    }
    if (is.matrix(x)|| is.data.frame(x)) {
        if (!is.numeric(x[, 1]) || !is.numeric(x[, 2])) {
            stop(paste("wrong input format:\n", "Point input must be a \"matrix\" or \"data.frame\" with 2 columns.\n", "Column order must be lon - lat", 
                sep = ""))
        }
        if (class(polyg) != "SpatialPolygons") {
            warning("to plot polygons, polyg must be of class \"SpatialPolygons\"")
        }
        x <- as.data.frame(x)
        nums <- sapply(x, is.numeric)
        x <- x[, nums]
        xmax <- min(max(x[, 2]) + 2, 180)
        xmin <- max(min(x[, 2]) - 2, -180)
        ymax <- min(max(x[, 1]) + 2, 90)
        ymin <- max(min(x[, 1]) - 2, -90)
        if (ymax > 92 || ymin < -92) {
            warning("column order must be lon-lat, not lat - lon. Please check")
        }
        map("world", xlim = c(xmin, xmax), ylim = c(ymin, ymax))
        axis(1)
        axis(2)
        title("All samples")
        box("plot")
        # if (moreborders == T) {plot(wrld_simpl, add = T, ...)}
        if (is.list(polyg)) 
            
        plot(polyg, col = "grey60", add = T, ...)
        
        points(x[, 2], x[, 1], cex = 0.5, pch = 3, col = "blue", ...)
        dat <- data.frame(x$not_classified_samples)
        points(dat$decimallongitude, dat$decimallatitude, cex = 0.5, pch = 3, col = "red", ...)
    }
}

.MapPerPoly <- function(x, areanames = NULL, plotout = FALSE) {
    if (!class(x) == "spgeoOUT") {
        stop("this function is only defined for class \"spgeoOUT\"")
    }
    dum <- x$polygons
    
    if (class(dum) == "SpatialPolygonsDataFrame") {
      if (length(areanames) == 0) {
        areanames <- x$areanam
      }
        len <- length(unique(x$polygons@data[, areanames]))
    } else {
        len <- length(names(dum))
    }
    for (i in 1:len) {
        if (class(dum) == "SpatialPolygonsDataFrame") {
            chopo <- unique(x$polygons@data[, areanames])[i]
            
            xmax <- min(max(bbox(subset(x$polygons, x$polygons@data[, areanames] == unique(x$polygons@data[, areanames])[i]))[1, 
                2]) + 5, 180)
            xmin <- max(min(bbox(subset(x$polygons, x$polygons@data[, areanames] == unique(x$polygons@data[, areanames])[i]))[1, 
                1]) - 5, -180)
            ymax <- min(max(bbox(subset(x$polygons, x$polygons@data[, areanames] == unique(x$polygons@data[, areanames])[i]))[2, 
                2]) + 5, 90)
            ymin <- max(min(bbox(subset(x$polygons, x$polygons@data[, areanames] == unique(x$polygons@data[, areanames])[i]))[2, 
                1]) - 5, -90)
        } else {
            chopo <- names(dum)[i]
            
            xmax <- min(max(bbox(x$polygons[i])[1, 2]) + 5, 180)
            xmin <- max(min(bbox(x$polygons[i])[1, 1]) - 5, -180)
            ymax <- min(max(bbox(x$polygons[i])[2, 2]) + 5, 90)
            ymin <- max(min(bbox(x$polygons[i])[2, 1]) - 5, -90)
        }
        po <- x$samples[,c(4,2:3)]
        subpo <- subset(po, as.character(po$homepolygon) == as.character(chopo))
        
        subpo <- subpo[order(subpo$species), ]
        
        liste <- unique(subpo$species)
        leng <- length(liste)
        
        rain <- rainbow(leng)
        ypos <- vector(length = leng)
        yled <- (ymax - ymin) * 0.025
        for (k in 1:leng) {
            ypos[k] <- ymax - yled * k
        }
        
        layout(matrix(c(1, 1, 1, 1, 1, 2, 2), ncol = 7, nrow = 1))
        par(mar = c(3, 3, 3, 0))
        te <- try(map("world", xlim = c(xmin, xmax), ylim = c(ymin, ymax)), silent = T)
        if (class(te) == "try-error") {
            map("world")
        }
        axis(1)
        axis(2)
        box("plot")
        title(chopo)
        if (class(dum) == "SpatialPolygonsDataFrame") {
            plot(subset(x$polygons, x$polygons@data[, areanames] == unique(x$polygons@data[, areanames])[i]), col = "grey60", 
                add = T)
        } else {
            plot(x$polygons[i], col = "grey60", add = T)
        }
        for (j in 1:leng) {
            subsub <- subset(subpo, subpo$species == liste[j])
            points(subsub[, 3], subsub[, 4], cex = 1, pch = 3, col = rain[j])
        }
        par(mar = c(3, 0, 3, 0), ask = F)
        plot(c(1, 50), c(1, 50), type = "n", axes = F)
        if (leng == 0) {
            yset <- 25
            xset <- 1
        }
        if (leng == 1) {
            yset <- 25
            xset <- rep(4, leng)
        }
        if (leng > 1) {
            yset <- rev(sort(c(seq(25, 25 + max(ceiling(leng/2) - 1, 0)), seq(24, 24 - leng/2 + 1))))
            xset <- rep(4, leng)
        }
        points(xset - 2, yset, pch = 3, col = rain)
        if (leng == 0) {
            text(xset, yset, labels = "No species found in this polygon", adj = 0)
        } else {
            text(xset, yset, labels = liste, adj = 0, xpd = T)
            rect(min(xset) - 4, min(yset) - 1, 50 + 1, max(yset) + 1, xpd = T)
        }
        
        if (plotout == FALSE) {
            par(ask = T)
        }
    }
    par(ask = F)
    layout(matrix(1,1,1))
}

.MapPerSpecies <- function(x, moreborders = FALSE, plotout = FALSE, verbose = FALSE, ...) {
    if (!class(x) == "spgeoOUT") {
        stop("this function is only defined for class \"spgeoOUT\"")
    }
    # if (moreborders == T) {data('wrld_simpl', envir = environment())}
    layout(matrix(1, ncol = 1, nrow = 1))
    if (plotout == FALSE) {
        par(ask = T)
    }
    dat <- x$samples[,c(1,4,2,3)]
    liste <- unique(dat$species)
    
    
    for (i in 1:length(liste)) {
        kk <- subset(dat, dat$species == liste[i])
        
        inside <- kk[!is.na(kk$homepolygon), ]
        outside <- kk[is.na(kk$homepolygon), ]
        
        xmax <- min(max(dat$decimallongitude) + 2, 180)
        xmin <- max(min(dat$decimallongitude) - 2, -180)
        ymax <- min(max(dat$decimallatitude) + 2, 90)
        ymin <- max(min(dat$decimallatitude) - 2, -90)
        
        map("world", xlim = c(xmin, xmax), ylim = c(ymin, ymax))
        axis(1)
        axis(2)
        title(liste[i])
        plot(x$polygons, col = "grey60", add = T)
        
        if (length(inside) > 0) {
            points(inside$decimallongitude, inside$decimallatitude, cex = 0.7, pch = 3, col = "blue")
        }
        if (length(outside) > 0) {
            points(outside$decimallongitude, outside$decimallatitude, cex = 0.7, pch = 3, col = "red")
        }
        box("plot")
    }
    par(ask = F)
    layout(matrix(1,1,1))
}

.MapUnclassified <- function(x, moreborders = FALSE, verbose = FALSE, ...) {
    if (!class(x) == "spgeoOUT") {
        stop("This function is only defined for class \"spgeoOUT\"")
    }
    dat <- x$samples[x$samples$homepolygon == "not_classified",]
    if (dim(dat)[1] == 0) {
        plot(c(1:20), c(1:20), type = "n", axes = F, xlab = "", ylab = "")
        text(10, 10, labels = paste("All points fell into the polygons and were classified.\n", "No unclassified points", sep = ""))
    } else {
        xmax <- min(max(dat$decimallongitude) + 2, 180)
        xmin <- max(min(dat$decimallongitude) - 2, -180)
        ymax <- min(max(dat$decimallatitude) + 2, 90)
        ymin <- max(min(dat$decimallatitude) - 2, -90)
        
        map("world", xlim = c(xmin, xmax), ylim = c(ymin, ymax), ...)
        axis(1)
        axis(2)
        title("Samples not classified to polygons \n")

        if (verbose) {
            warning("adding polygons")
        }
        if (is.list(x$polygons)) {
            plota <- function(x) {
                plot(x, add = T, col = "grey60", border = "grey40")
            }
            lapply(x$polygons, plota)
        } else {
            plot(x$polygons, col = "grey60", border = "grey40", add = T, ...)
        }
        if (verbose) {
            warning("adding sample points")
        }
        points(dat$decimallongitude, dat$decimallatitude, cex = 0.5, pch = 3, col = "red", ...)
        box("plot")
    }
}

.NexusOut <- function(dat, verbose = FALSE) {
  if (is.list(dat)) {
    tablist <- lapply(dat, function(x) x$spec_table)
    for (i in 1:length(tablist)) {
      names(tablist[[i]])[-1] <- paste(names(tablist)[i], names(tablist[[i]][-1]), sep = "_")
    }
    speciestab <- Reduce(function(x, y) merge(x, y, all = TRUE), tablist)
  } else {
    speciestab <- dat$spec_table
  }
  if (verbose == FALSE) {
    sink("species_classification.nex")
  }
  if (verbose == TRUE) {
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
    aa <- gsub(" ", "_", names(speciestab)[-1])
    aa <- gsub("&", "_", aa)
    aa <- gsub("__", "_", aa)
    aa <- gsub("__", "_", aa)
    bb <- seq(1, length(aa))
    
    cat(paste("\t", bb[-length(bb)], " ", aa[-length(aa)], ",\n", sep = ""))
    cat("\t", paste(bb[length(bb)], " ", aa[length(aa)], ";\n", sep = ""))
    cat("\n")
    cat("\tmatrix\n")
    
    dd <- as.matrix(speciestab[, -1])
    dd[dd > 0] <- 1
    
    if (dim(dd)[2] > 1) {
      dd <- data.frame(dd)
      dd$x <- apply(dd[, names(dd)], 1, paste, collapse = "")
    } else {
      dd <- data.frame(dd, x = dd)
    }
    ff <- gsub(" ", "_", speciestab[, 1])
    
    if (verbose == F) {
      ee <- paste("\t\t", ff, "\t", dd$x, "\n", sep = "")
      cat(ee)
    }
    if (verbose) {
      gg <- vector()
      jj <- speciestab[, -1]
      for (i in 1:dim(jj)[2]) {
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

.OutBarChartPoly <- function(x, prefix, verbose = FALSE, ...) {
    if (verbose) {
        cat("Creating barchart per polygon: barchart_per_polygon.pdf. \n")
    }
    pdf(file = paste(prefix, "barchart_per_polygon.pdf", sep = ""), paper = "special", width = 10.7, height = 7.2, onefile = T)
    .BarChartPoly(x, plotout = T, cex.axis = 0.8, ...)
    dev.off()
}

.OutBarChartSpec <- function(x, prefix, verbose = FALSE, ...) {
    if (verbose) {
        cat("Creating barchart per species: barchart_per_species.pdf. \n")
    }
    pdf(file = paste(prefix, "barchart_per_species.pdf", sep = ""), paper = "special", width = 10.7, height = 7.2, onefile = T)
    .BarChartSpec(x, plotout = T, mode = "percent", ...)
    dev.off()
}

.OutMapAll <- function(x, prefix, areanames = "", verbose = FALSE, ...) {
    if (verbose) {
        cat("Creating overview map: map_samples_overview.pdf. \n")
    }
    pdf(file = paste(prefix, "map_samples_overview.pdf", sep = ""), paper = "special", width = 10.7, height = 7.2, onefile = T, 
        ...)
    .MapAll(x, ...)
    .MapUnclassified(x, ...)
    dev.off()
}

.OutMapPerPoly <- function(x, prefix, verbose = FALSE, ...) {
    if (verbose) {
        cat("Creating map per polygon: map_samples_per_polygon.pdf. \n")
    }
    pdf(file = paste(prefix, "map_samples_per_polygon.pdf", sep = ""), paper = "special", width = 10.7, height = 7.2, onefile = T)
    .MapPerPoly(x, plotout = T)
    dev.off()
}

.OutMapPerSpecies <- function(x, prefix, verbose = FALSE, ...) {
    if (verbose) {
        cat("Creating map per species: map_samples_per_species.pdf. \n")
    }
    pdf(file = paste(prefix, "map_samples_per_species.pdf", sep = ""), paper = "special", width = 10.7, height = 7.2, onefile = T)
    .MapPerSpecies(x, plotout = T, ...)
    dev.off()
}

.OutPlotSpPoly <- function(x, prefix, verbose = FALSE, ...) {
    if (verbose) {
        cat("Creating species per polygon barchart: number_of_species_per_polygon.pdf. \n")
    }
    pdf(file = paste(prefix, "number_of_species_per_polygon.pdf", sep = ""), paper = "special", width = 10.7, height = 7.2, 
        onefile = T)
    .PlotSpPoly(x, ...)
    dev.off()
}

.PipSamp <- function(x, columnname, verbose = FALSE) {

    if (class(x$polygons) == "SpatialPolygonsDataFrame") {
        if (any(is.na(x$polygons@data[, columnname]))) {
            stop("area names contain missing data (#N/A). Renamed to  unnamed. This migh cause problems")
        }
      occ <- SpatialPoints(x$species_coordinates[, c(1, 2)])

        outp <- as.character(over(occ, x$polygon)[,1])
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

.PlotSpPoly <- function(x, ...) {
    if (class(x) == "spgeoOUT") {
        num <- length(names(x$polygon_table))
        dat <- sort(x$polygon_table)
        counter <- num/10
        if (length(x$polygon_table) != 0) {
          if (length(x$polygon_table) == 1){
            barplot(as.matrix(dat[1, ]),
                    ylim = c(0, round((max(dat) + max(c(max(dat)/4, 1))), 0)),
                    ylab = "Number of Species per Polygon", 
                    names.arg = names(x$polygon_table),
                    las = 2, ...)
          }else{
            barplot(as.matrix(dat[1, ]), 
                    ylim = c(0, round((max(dat) + max(c(max(dat)/4, 1))), 0)), 
                    ylab = "Number of Species per Polygon", 
                    las = 2, ...)
          }
          box("plot")
        } else {
            cat("No point in any polygon")
        }
    } else {
        stop("this function is only defined for class \"spgeoOUT\"")
    }
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
  
  if (!is.character(x) && !is.data.frame(x)) {
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
    names(coords) <- c("identifier", "XCOOR", "YCOOR")
    coords <- data.frame(coords[complete.cases(coords), ])
  }
  
  if (class(x) == "character" & length(grep(".txt", x)) > 0) {
    coords <- read.table(x, sep = "\t", header = T, row.names = NULL)
  }
  
  if (class(x) == "data.frame") {
    coords <- x
    rownames(coords) <- 1:dim(coords)[1]
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
      if (dim(polycord)[2] != 3) {
        stop("Wrong input format;\ninputfile for polygons must be a tab-delimited text file with three columns")
      }
      if (!is.numeric(polycord[, 2]) || !is.numeric(polycord[, 3])) {
        stop("wrong input format:\nInput polygon coordinates (columns 2 and 3) must be numeric.")
      }
      if (!is.character(polycord[, 1]) && !is.factor(polycord[, 1])) {
        warning("polygon identifier (column 1) should be a string or a factor")
      }
      if (max(polycord[, 2]) > 180) {
        warning(sprintf("check polygon input coordinates; file contains longitude values outside possible range in row: \n                      %s\n Coordinates set to maximum: 180.\n", 
                        rownames(polycord[polycord[, 2] > 180, ])))
        polycord[polycord[, 2] > 180, 2] <- 180
      }
      if (min(polycord[, 2]) < -180) {
        warning(paste("check polygon input coordinates. File contains longitude values outside possible range in row: ", 
                      rownames(polycord[polycord[, 2] < -180, ]), "\n", "Coordinates set to minimum: -180", 
                      sep = ""))
        polycord[polycord[, 2] < -180, ] <- -180
        
      }
      if (max(polycord[, 3]) > 90) {
        warning(paste("check polygon input coordinates. File contains latitude values outside possible range in row:", 
                      rownames(polycord[polycord[, 3] > 90, ]), "\n", "Coordinates set to maximum: 90", 
                      sep = ""))
        polycord[polycord[, 3] > 90, 3] <- 90
      }
      if (min(polycord[, 3]) < -90) {
        warning(paste("check polygon input coordinates. File contains latitude values outside possible range in row:", 
                      rownames(polycord[polycord[, 3] < -90, ]), "\n", "Coordinates set to minimum: -90", 
                      sep = ""))
        polycord[polycord[, 3] < -90, 3] <- -90
      }
      poly <- .Cord2Polygon(polycord)
    }
  }
  
  if (class(y) == "SpatialPolygonsDataFrame" | class(y) == "SpatialPolygons") {
    poly <- y
  }
  
  if (ncol(coords) != 3) {
    if (all(c("scientificName", "decimalLatitude", "decimalLongitude") %in% names(coords))) {
      coords <- data.frame(identifier = coords$scientificName, XCOOR = coords$decimalLongitude, 
                           YCOOR = coords$decimalLatitude)
      warning("more than 3 columns in point input. Assuming DarwinCore formatted file")
    } else {
      stop(paste("wrong input format: \n", "Inputfile for coordinates must have three columns", 
                 sep = ""))
    }
  }
  
  if (!is.numeric(coords[, 2]) || !is.numeric(coords[, 3])) {
    stop(paste("wrong input format: \n", "Input point coordinates (columns 2 and 3) must be numeric", 
               sep = ""))
  }
  
  if (max(coords[, 2]) > 180) {
    warning(paste("longitude values outside possible range in row:", 
                  rownames(coords[coords[, 2] > 180, ]), ". ", "Row deleted", sep = ""))
    coords <- coords[!coords[, 2] > 180, ]
  }
  if (min(coords[, 2]) < -180) {
    warning(paste("longitude values outside possible range in row: ", 
                  rownames(coords[coords[, 2] < -180, ]), ". ", "Row deleted", sep = ""))
    coords <- coords[!coords[, 2] < -180, ]
  }
  if (max(coords[, 3]) > 90) {
    warning(paste("latitude values outside possible range in row:", 
                  rownames(coords[coords[, 3] > 90, ]), ". ", "Row deleted", sep = ""))
    coords <- coords[!coords[, 3] > 90, ]
  }
  if (min(coords[, 3]) < -90) {
    warning(paste("latitude values outside possible range in row:", 
                  rownames(coords[coords[,3] < -90, ]), ". ", "Row deleted", sep = ""))
    coords <- coords[!coords[, 3] < -90, ]
  }
  if (!is.character(coords[, 1]) && !is.factor(coords[, 1])) {
    warning("coordinate identifier (column 1) should be a string or a factor")
  }
  coords[, 1] <- as.factor(coords[, 1])
  coordi <- coords[, c(2, 3)]
  names(coordi) <- c("XCOOR", "YCOOR")
  
  areanam <- areanames
  
  res <- list(species = coords[, 1], species_coordinates = coordi, polygons = poly, 
              areanam = areanam)
  class(res) <- "spgeoIN"
  return(res)
  
} 

.SpGeoCodH <- function(x, areanames = NULL, occ.thresh = 0) {
  if (class(x) == "spgeoIN") {
    if (class(x$polygons) == "SpatialPolygons") {
      if ("NA" %in% names(x$polygons)) {
        warning("the polygondata contain a polygon named NA. this can cause problems. Please rename")
      }
    }
    if (class(x$polygons) == "SpatialPolygonsDataFrame") {
      if (!areanames %in% names(x$polygons@data)){
        stop(sprintf("column '%s' not found", "areanames"))
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
    nums$names <- gsub("[[:punct:]]", " ", as.character(rownames(nums)))
    pol.df <- merge(pol.df, nums, sort=FALSE, by.x = areanames,
                    by.y = "row.names", all.x = TRUE)
    pol.df <- subset(pol.df, select = -c(names))
    pol <- SpatialPolygonsDataFrame(as(x$polygons, "SpatialPolygons"), data = pol.df)
    
    #create output
    out <- list(samples = data.frame(species = x$species,
                                     decimallongitude = x$species_coordinates[,1],
                                     decimallatitude = x$species_coordinates[,2],
                                     homepolygon = kkk),
                polygons = pol, 
                spec_table = spsum, 
                polygon_table = sppol,
                coexistence_classified = "NA", 
                areanam = areanames)
    class(out) <- "spgeoOUT"
    return(out)
  } else {
    stop("Function is only defined for class spgeoIN")
  }
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

.testcordcap <- function(x, reftab, capthresh) {
  capout <- NA
    if (is.na(as.character(unlist(x["country"]))) | is.na(suppressWarnings(as.numeric(as.character(x["decimallongitude"])))) |
          is.na(suppressWarnings(as.numeric(as.character(x["decimallatitude"])))) | as.character(unlist(x["country"])) == "") {
        capout <- NA
    } else {
        if (nchar(as.character(unlist(x["country"]))) <= 2) {
            loncap <- suppressWarnings(as.numeric(as.character(x["decimallongitude"]))) > (subset(reftab, as.character(reftab$ISO2) == 
                as.character(unlist(x["country"])))$capital_lon - capthresh) & suppressWarnings(as.numeric(as.character(x["decimallongitude"]))) < 
                (subset(reftab, as.character(reftab$ISO2) == as.character(unlist(x["country"])))$capital_lon + capthresh)
            latcap <- suppressWarnings(as.numeric(as.character(x["decimallatitude"]))) > (subset(reftab, as.character(reftab$ISO2) == 
                as.character(unlist(x["country"])))$capital_lat - capthresh) & suppressWarnings(as.numeric(as.character(x["decimallatitude"]))) < 
                (subset(reftab, as.character(reftab$ISO2) == as.character(unlist(x["country"])))$capital_lat + capthresh)
        }
        if (nchar(as.character(unlist(x["country"]))) <= 3 & !nchar(as.character(unlist(x["country"]))) <= 2) {
            loncap <- suppressWarnings(as.numeric(as.character(x["decimallongitude"]))) > (subset(reftab, as.character(reftab$ISO3) == 
                as.character(unlist(x["country"])))$capital_lon - capthresh) & suppressWarnings(as.numeric(as.character(x["decimallongitude"]))) < 
                (subset(reftab, as.character(reftab$ISO3) == as.character(unlist(x["country"])))$capital_lon + capthresh)
            latcap <- suppressWarnings(as.numeric(as.character(x["decimallatitude"]))) > (subset(reftab, as.character(reftab$ISO3) == 
                as.character(unlist(x["country"])))$capital_lat - capthresh) & suppressWarnings(as.numeric(as.character(x["decimallatitude"]))) < 
                (subset(reftab, as.character(reftab$ISO3) == as.character(unlist(x["country"])))$capital_lat + capthresh)
        }
        if (nchar(as.character(unlist(x["country"]))) > 3) {
            loncap <- T
            latcap <- T
            warning(paste("found country information for", unlist(x["species"]), "with more than 3 letters. Change country information to ISO2 or ISO3", 
                sep = " "))
        }
        ifelse(loncap & latcap, capout <- FALSE, capout <- TRUE)
    }
    return(capout)
}

.testcordcountr <- function(x, reftab, contthresh) {
  contout <- NA
    if (is.na(as.character(unlist(x["country"]))) | is.na(suppressWarnings(as.numeric(as.character(x["decimallongitude"])))) | 
          is.na(suppressWarnings(as.numeric(as.character(x["decimallatitude"])))) | as.character(unlist(x["country"])) == "") {
        contout <- NA
    } else {
        if (nchar(as.character(unlist(x["country"]))) <= 2) {
            loncont <- suppressWarnings(as.numeric(as.character(x["decimallongitude"]))) > (subset(reftab, as.character(reftab$ISO2) == 
                as.character(unlist(x["country"])))$centroid_lon - contthresh) & suppressWarnings(as.numeric(as.character(x["decimallongitude"]))) < 
                (subset(reftab, as.character(reftab$ISO2) == as.character(unlist(x["country"])))$centroid_lon + contthresh)
            latcont <- suppressWarnings(as.numeric(as.character(x["decimallatitude"]))) > (subset(reftab, as.character(reftab$ISO2) == 
                as.character(unlist(x["country"])))$centroid_lat - contthresh) & suppressWarnings(as.numeric(as.character(x["decimallatitude"]))) < 
                (subset(reftab, as.character(reftab$ISO2) == as.character(unlist(x["country"])))$centroid_lat + contthresh)
        }
        if (nchar(as.character(unlist(x["country"]))) == 3) {
            loncont <- suppressWarnings(as.numeric(as.character(x["decimallongitude"]))) > (subset(reftab, as.character(reftab$ISO3) == 
                as.character(unlist(x["country"])))$centroid_lon - contthresh) & suppressWarnings(as.numeric(as.character(x["decimallongitude"]))) < 
                (subset(reftab, as.character(reftab$ISO3) == as.character(unlist(x["country"])))$centroid_lon + contthresh)
            latcont <- suppressWarnings(as.numeric(as.character(x["decimallatitude"]))) > (subset(reftab, as.character(reftab$ISO3) == 
                as.character(unlist(x["country"])))$centroid_lat - contthresh) & suppressWarnings(as.numeric(as.character(x["decimallatitude"]))) < 
                (subset(reftab, as.character(reftab$ISO3) == as.character(unlist(x["country"])))$centroid_lat + contthresh)
        }
        if (nchar(as.character(unlist(x["country"]))) > 3) {
            loncont <- T
            latcont <- T
            warning(paste("found country information for", unlist(x["species"]), "with more than 3 letters. Change country information to ISO2 or ISO3", 
                sep = " "))
        }
        ifelse(loncont & latcont, contout <- FALSE, contout <- TRUE)
    }
    return(contout)
}

.WriteTablesSpGeo <- function(x, prefix = "", verbose = FALSE, ...) {
    if (class(x) == "spgeoOUT") {
        write.table(x$samples, file = paste(prefix, "sample_classification_to_polygon.txt", sep = ""), row.names = FALSE, sep = "\t", ...)
        write.table(x$spec_table, file = paste(prefix, "species_occurences_per_polygon.txt", sep = ""), row.names = FALSE, sep = "\t", ...)
        write.table(x$polygon_table, file = paste(prefix, "speciesnumber_per_polygon.txt", sep = ""), row.names = FALSE, sep = "\t", ...)
        write.table(x$samples[x$samples$homepolygon == "not_classified",], file = paste(prefix, "unclassified samples.txt", sep = ""), row.names = FALSE, sep = "\t", ...)
        write.table(x$coexistence_classified, file = paste(prefix, "species_coexistence_matrix.txt", sep = ""), row.names = FALSE, sep = "\t", ...)
    } else {
        stop("this function is only defined for class \"spgeoOUT\"")
    }
} 
