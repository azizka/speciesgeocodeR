# add collections argument with: Herbaria from INdex Herbariorum, GBIF institutionen, Botanical gardens from GBCI and Museums from gr bio, all where referencing is was possible from google and that to not fall into water
# add a dataset test
# include warning message in case the standard landmass is used

CleanCoordinates <- function(x, countries = NULL, species = NULL, dataset = NULL, validity = T, zeros = T, capitals = T, centroids = T, seas = T, urban = F, countrycheck = T, outliers = T, 
                             GBIF = T, duplicates = F, verbose = T, output = c("spatialvalid", "summary", "cleaned"), zeros.rad = 0.5, capitals.rad = 0.05, outliers.mtp = 25, outliers.td = NULL, centroids.rad = 0.01, 
                             capitals.ref = NULL, centroids.detail = c("both", "country", "provinces"), centroids.ref = NULL, seas.ref = NULL, urban.ref = NULL, country.ref = NULL, report = F) {
  
  match.arg(output)
  match.arg(centroids.detail)

  if (is.matrix(x) | is.data.frame(x)) {
    if (dim(x)[2] != 2) {
      warning("wrong input format, x needs to be a data.frame or matrix with two columns")
    }
    if (is.null(countries)) {
      countrycheck <- FALSE
      warning("inputformat matrix, countrycheck set to FALSE")
    }
    if (is.null(species)) {
      outliers <- FALSE
      warning("species == NULL, outliers test skipped")
    }
    
    names(x) <- c("decimallongitude", "decimallatitude")
    
    # Run tests Validity, check if coordinataes fit to lat/long system, this has to be run all the time, as otherwise the other tests don´t work
    if (verbose) {
      cat("running validity test\n")
    }
    val <- .ValidCoordinates(x)
    
    if (!all(val)) {
      stop("invalid coordinates found, clean dataset before further tests:\n", 
           paste(which(val == FALSE), "\n"))
    }
    
    ## Zero coordinates
    if (zeros) {
      if (verbose) {
        cat("running zero coordinate test\n")
      }
      zer <- .ZeroCoordinates(x, pointlim = zeros.rad)
      if (verbose) {
        cat(sprintf("flagged %s records \n", sum(!zer)))
      }
    } else {
      zer <- rep(NA, dim(x)[1])
    }
    
    ## Capitals
    if (capitals) {
      if (verbose) {
        cat("running capitals test\n")
      }
      cap <- .CapitalCoordinates(x, testdist = capitals.rad, buffer = 1, 
                                 referencedat = capitals.ref)
      if (verbose) {
        cat(sprintf("flagged %s records \n", sum(!cap)))
      }
    } else {
      cap <- rep(NA, dim(x)[1])
    }
    
    ## Centroids
    if (centroids) {
      if (verbose) {
        cat("running centroids test\n")
      }
      cen <- .CentroidCoordinates(x, testdist = centroids.rad, buffer = 1, 
                                  testtype = centroids.detail, referencedat = centroids.ref)
      if (verbose) {
        cat(sprintf("flagged %s records \n", sum(!cen)))
      }
    } else {
      cen <- rep(NA, nrow(x))
    }
    
    # Seas
    if (seas) {
      if (verbose) {
        cat("running seas test\n")
      }
      sea <- .WaterCoordinates(x, poly = seas.ref)
      if (verbose) {
        cat(sprintf("flagged %s records \n", sum(!sea)))
      }
    } else {
      sea <- rep(NA, dim(x)[1])
    }
    
    # Urban Coordinates
    if (urban) {
      if (verbose) {
        cat("running urban test\n")
      }
      urb <- .UrbanCoordinates(x, poly = urban.ref)
      if (verbose) {
        cat(sprintf("flagged %s records \n", sum(!urb)))
      }
    } else {
      urb <- rep(NA, dim(x)[1])
    }
    
    # Country check
    if (countrycheck) {
      if (verbose) {
        cat("running countrycheck test\n")
      }
      con <- .CountryCheck(x, countries, poly = country.ref)
      if (verbose) {
        cat(sprintf("flagged %s records \n", sum(!con, na.rm = T)))
      }
    } else {
      con <- rep(NA, dim(x)[1])
    }
    
    # Outliers
    if (outliers) {
      if (verbose) {
        cat("running outliers test\n")
      }
      otl <- .OutlierCoordinates(x, species = species, mltpl = outliers.mtp, tdi = outliers.td)
      if (verbose) {
        cat(sprintf("flagged %s records \n", sum(!otl)))
      }
    } else {
      otl <- rep(NA, dim(x)[1])
    }
    
    # GBIF headquaters
    if (GBIF) {
      if (verbose) {
        cat("running GBIF test\n")
      }
      
      gbf <- .GBIF(x)
      if (verbose) {
        cat(sprintf("flagged %s records \n", sum(!gbf)))
      }
    } else {
      gbf <- rep(NA, dim(x)[1])
    }
    
    # Botanical Gardens
    
    # Museums
    
    # Datasets
    
    
    # exclude duplicates
    if (duplicates) {
      cat("running duplicates test\n")
      if (is.null(species)) {
        dpl.test <- x
        warning("running duplicates test without species id, assuming single species dataset")
      } else {
        dpl.test <- data.frame(x, species)
      }
      dpl <- !duplicated(dpl.test)
      if (verbose) {
        cat(sprintf("flagged %s records \n", sum(!dpl)))
      }
    } else {
      dpl <- rep(NA, dim(x)[1])
    }
  }
  
  #prepare output data
  
  out <- list(val, zer, cap, cen, sea, urb, con, otl, gbf, dpl)
  out <- Filter(function(x) !all(is.na(x)), out)
  out <- Reduce("&", out)
  
  if (verbose) {
    if (!is.null(out)) {
      cat(sprintf("flagged %s of %s records, EQ = %s \n", sum(!out, na.rm = T), 
                  length(out), round(sum(!out, na.rm = T)/length(out), 2)))
    } else {
      cat("flagged 0 records, EQ = 0 \n")
    }
  }
  if (output[1] == "spatialvalid") {
    out <- data.frame(x, validity = val, zeros = zer, capitals = cap, centroids = cen, 
                      sea = sea, urban = urb, countrycheck = con, outliers = otl, 
                      gbif = gbf, duplicates = dpl, summary = out)
    out <- Filter(function(x) !all(is.na(x)), out)
    class(out) <- c("spatialvalid", "data.frame", class(out))
  }
  if (output[1] == "cleaned") {
    if (is.null(species)) {
      out <- data.frame(x, validity = val, zeros = zer, capitals = cap, centroids = cen, 
                        sea = sea, urban = urb, countrycheck = con, outliers = otl, 
                        gbif = gbf, duplicates = dpl, 
                        summary = out)
      out <- Filter(function(x) !all(is.na(x)), out)
      out <- out[, 1:2]
    } else {
      out <- data.frame(x, species = species, validity = val, zeros = zer, capitals = cap, 
                        centroids = cen, sea = sea, urban = urb, countrycheck = con, 
                        outliers = otl, gbif = gbf, 
                        duplicates = dpl, summary = out)
      out <- Filter(function(x) !all(is.na(x)), out)
      out <- out[, 1:3]
    }
  }
  
  if (report) {
    report <- "CleanCoordinates_report.txt"
  }
  if (is.character(report)) {
    suma <- data.frame(test = as.character(names(out[-c(1:2)])), 
                       flagged.records = colSums(!out[-c(1:2)]))
    suma <- rbind(suma, c("Error Quotient", 
                          round(sum(out$summary, na.rm = T)/length(out$summary), 2)))
    write.table(suma, report, sep = "\t", row.names = F)
  }
  
  return(out)
} 