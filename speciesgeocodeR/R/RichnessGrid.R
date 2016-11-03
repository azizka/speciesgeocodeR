RichnessGrid <- function(x, limits = c(-180, 180, -90, 90), reso, type = "spnum") {
    
  e <- extent(limits)
    
    #Input spgeoOUT objects
    if (class(x) == "spgeoOUT" ) {
        dum <- data.frame(species = x$species_in, x$species_coordinates_in)
        x <- dum
    }
  
    #Input character string, downloading from gbif
    if (is.character(x)) {
        if (!requireNamespace("rgbif", quietly = TRUE)) {
          stop("rgbif needed for species name option. Please install it.",
               call. = FALSE)
        } 
        splist <- strsplit(x, " ")
        coords <- rgbif::occ_search(scientificName = x, return = "data", 
                                    limit = 200000, hasCoordinate = T, spatialIssues = F,
                                    fields = c("species", "decimalLongitude","decimalLatitude"))
        coords <- do.call("rbind", coords)
        names(coords) <- c("species", "decimallongitude", "decimallatitude")
        coords <- data.frame(coords[complete.cases(coords),])
        x <- coords
        warning(paste(nrow(inp), "geo-referenced records found in GBIF. No data cleaning was performed", sep = " "))
    }
  
  #Create raster
    ras <- raster(e)
    reas(ras) <- reso
    ras <- setValues(ras, 0)
    
    
   out <-  switch(type,
           spnum = {inp <- split(x, f = x$species)
             lapply(inp, function(x) .rasterSum(x, ras))
           }
           abu = .rasterSum(x, ras)
             )
    
    # if (type[1] == "spnum") {
    #   inp <- split(x, f = x$species)
    #     rast <- lapply(inp, function(x) .rasterSum(x, ras, "div"))
    # }
    # if (type[1] == "abu") {
    #     rast <- lapply(inp, function(x) .rasterSum(x, ras, "abu"))
    # }
    # 
    out <- Reduce("+", rast)
    out[out == 0] <- NA

    return(out)
}
 
