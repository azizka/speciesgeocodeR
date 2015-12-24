CalcRange <- function(x, mode = "EOO", value = c("area", "shape"), terrestrial = F, verbose = F) {
    if (class(x) == "spgeoOUT"){
      dat1 <- data.frame(identifier = x$identifier_in, x$species_coordinates_in)
    }else{
      dat1 <- x[, 1:3]
      names(dat1) <- c("identifier", "XCOOR", "YCOOR")
    }
    dat <- unique(dat1)
    if (!is.factor(dat[, 1]) | !is.numeric(dat[, 2]) | !is.numeric(dat[, 3])) {
        stop("wrong input format, x must be a data.frame with three columns: speciesname, longitude, latitude.\n")
    }
    if (max(dat[, 2]) > 180 | min(dat[, 2]) < -180 | max(dat[, 3]) > 90 | min(dat[, 3]) < -90) {
        stop("invallid input coordinates. Check for column order and valid coordinates.")
    }
    if (!dim(dat)[1] == dim(dat1)[1]) {
        warning((dim(dat1)[1] - dim(dat)[1]), " points were excluded due to duplicated coordinate")
    }
    filt <- tapply(dat$XCOOR, dat$identifier, length)
    filterd <- filt[filt > 2]
    dat$identifier <- as.character(dat$identifier)
    dat.filt <- subset(dat, dat$identifier %in% as.character(names(filterd)))
    
    sortout <- filt[filt <= 2]
    if (length(sortout) > 0) {
        warning("the following species have less than 3 occurrence, values set to NA:", paste("\n", names(sortout)))
    }
    
    inp <- split(dat.filt, f = dat.filt$identifier)
    
    if (mode == "EOO") {
      if (value[1] == "area"){
          out <- lapply(inp, function(x) .eoo(x, clipp = T))
          out <- data.frame(do.call("rbind", out))
          names(out) <- "EOO"
          out <- rbind(out, data.frame(row.names = rownames(sortout), EOO = rep("NA", length(sortout))))
      }
    }
    if(value[1] == "shape"){
      out <- lapply(inp, function(x) .ConvHull (x))
      if(terrestrial){
        if (!requireNamespace("geosphere", quietly = TRUE)) {
          stop("rgeos needed for terrestrial = T. Please install the package.",
               call. = FALSE)
        }  
        if(verbose == T){
          cat("Clipping shapes to landmasses.")
        }
        out <- lapply(out, function(x) rgeos::gIntersection(x, speciesgeocodeR::landmass))
        
      }
    }
    return(out)
} 
