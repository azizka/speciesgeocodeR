SpGeoCod <- function(x, y, areanames = "", occ.thresh = 0, elevation = FALSE, 
    threshold) {
    if (elevation) {
        ini <- .ReadPoints(x, y)
        coords <- data.frame(species = ini$species, ini$species_coordinates)
        coords$ele <- .GetElevation(coords)
        
        if (max(threshold) > max(coords$ele, na.rm = TRUE)) {
            threshold2 <- threshold[which(threshold < max(coords$ele, na.rm = TRUE))]
            warning(sprintf("maximum threshold (%s meter) is higher than maximum elevation in the dataset (%s meter); maximum threshold set to %s meter", 
                max(threshold), max(coords$ele, na.rm = TRUE), max(threshold2)))
            threshold <- threshold2
        }
        
        threshold <- unique(c(0, threshold, max(coords$ele, na.rm = TRUE) + 1))
        coords$cuts <- cut(coords$ele, breaks = threshold, labels = paste(">", 
            threshold[-length(threshold)], sep = ""))
        tt <- split(coords, coords$cuts)
        
        tt <- lapply(tt, function(x) .adjFormat(x))
        ini <- lapply(tt, function(x) .ReadPoints(x, y))
        outo <- lapply(ini, function(x) .SpGeoCodH(x, areanames, occ.thresh = occ.thresh))
        names(outo) <- gsub(">", "over_", names(outo))
        names(outo) <- paste(names(outo), "_meters", sep = "")
        return(outo)
    } else {
        ini <- .ReadPoints(x, y)
        outo <- .SpGeoCodH(ini, areanames, occ.thresh = occ.thresh)
        return(outo)
    }
}
