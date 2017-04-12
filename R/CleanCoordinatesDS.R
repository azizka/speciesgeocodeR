CleanCoordinatesDS <- function(x, ddmm = TRUE, periodicity = TRUE, output = "flags", 
                               ddmm.pvalue = 0.025, ddmm.diff = 0.2, 
                               periodicity.target = "both", periodicity.thresh = 2.5, 
                               periodicity.diagnostics = FALSE, 
                               subsampling = FALSE) {
  
  # check input arguments
  match.arg(output, choices = c("detail", "flags", "minimum"))
  match.arg(periodicity.target, choices = c("lat", "lon", "both"))
  
  # prepare input data
  if (!is.data.frame(x)) {
    stop("only defined for class 'data.frame'")
  }
  if (ncol(x) > 3) {
    if ("decimallongitude" %in% names(x) & "decimallatitude" %in% names(x) & "dataset" %in% 
        names(x)) {
      dat <- x[, c("decimallongitude", "decimallatitude", "dataset")]
    } else {
      stop("inputcolumns not found. Check input format")
    }
  } else {
    if (ncol(x) == 3) {
      if ("decimallongitude" %in% names(x) & "decimallatitude" %in% names(x) & "dataset" %in% 
          names(x)) {
        dat <- x[, c("decimallongitude", "decimallatitude", "dataset")]
      } else {
       dat <- x
      }
    }
    if (ncol(x) == 2) {
      dat$dataset <- "dataset"
      warning("column 'dataset' not found, assuming lon, lat input from single dataset")
    }
  }
  
  names(dat) <- c("decimallongitude", "decimallatitude", "dataset")
  
  dat$dataset <- as.character(dat$dataset)
  
  ## kick out NAs
  if (sum(!complete.cases(dat)) > 0) {
    warning(sprintf("ignored %s cases with incomplete data", sum(!complete.cases(dat))))
  }
  dat <- dat[complete.cases(dat), ]
  if (nrow(dat) == 0) {
    stop("no complete cases found")
  }
  
  ## create test columns: decimal degrees long and lat
  dat$lon.test <- abs(dat$decimallongitude) - floor(abs(dat$decimallongitude))
  dat$lat.test <- abs(dat$decimallatitude) - floor(abs(dat$decimallatitude))
  
  # split into seperate datasets
  test <- split(dat, f = dat$dataset)
  
  # run Test 1 per dataset - ddmm to dd.dd conversion error at 0.6
  if (ddmm) {
    out.t1 <- lapply(test, function(k) {
      ## create test datasets
      dat.unique <- k[!duplicated(k[, 1:2]), ]
      
      # create input data with a raster
      r <- raster::raster(xmn = 0, xmx = 1, ymn = 0, ymx = 1)
      raster::res(r) <- 0.01
      
      dat.t1 <- raster::rasterize(SpatialPoints(dat.unique[, c("lon.test", "lat.test")]), 
                                  r, fun = "count")
      dat.t1 <- raster::as.matrix(dat.t1)
      dat.t1[is.na(dat.t1)] <- 0
      
      # Binomial test, to see if more values are below 0.6 than expected
      P_smaller_than_06 <- 59 * 59/10000  # 0.3481
      
      subt <- dat.t1[1:59, 1:59]  # subset tbl 
      p06 <- sum(subt >= 1)
      pAll <- sum(dat.t1 >= 1)
      
      B <- stats::binom.test(p06, pAll, p = P_smaller_than_06, alternative = c("greater"))
      
      # P-VALUE
      v1 <- B$p.value  
      # PERCENTAGE OF difference from expected
      v2 <- (B$estimate - P_smaller_than_06)/P_smaller_than_06  
      
      # These to thresholds could be changed
      if (v1 < ddmm.pvalue & v2 > ddmm.diff) {
        flag.t1 <- FALSE
      } else {
        flag.t1 <- TRUE
      }
      
      c(round(v1, 4), round(v2, 3), flag.t1)
    })
    
    # Reduce output to data.frame
    
    out.t1 <- do.call("rbind.data.frame", out.t1)
    names(out.t1) <- c("binomial.pvalue", "perc.difference", "pass.ddmm")
    out.t1$pass.ddmm <- as.logical(out.t1$pass.ddmm)
  } else {
    out.t1 <- data.frame(pass.ddmm = rep(NA, length(test)))
  }
  
  # Run Test 2 and 3 per dataset
  if (periodicity) {
    out.t2 <- lapply(test, function(k) {
      t2 <- k[, c("lon.test", "lat.test")]
      if (nrow(t2) > 1000 & subsampling) {
        warning("large dataset. Using subsampling for periodicity")
        t2 <- t2[sample(nrow(t2), 1000), ]
      }
      if (periodicity.target == "lon") {
        t2.res <- .AnalyzeBias(var = t2, nam = k$dataset[1], var_latlong = 1, 
                               plot_bias = periodicity.diagnostics, 
                               ratio_threshold = periodicity.thresh)
        names(t2.res) <- c("mle", "rate.ratio", "pass.lon", "zero.mle", "zero.rate.ratio", 
                           "pass.zero.lon")
        
      }
      if (periodicity.target == "lat") {
        t2.res <- .AnalyzeBias(var = t2, nam = k$dataset[1], var_latlong = 2, 
                               plot_bias = periodicity.diagnostics, 
                               ratio_threshold = periodicity.thresh)
        names(t2.res) <- c("mle", "rate.ratio", "pass.lat", "zero.mle", "zero.rate.ratio", 
                           "pass.zero.lat")
      }
      
      if (periodicity.target == "both") {
        lon <- .AnalyzeBias(t2, nam = k$dataset[1], var_latlong = 1, 
                            plot_bias = periodicity.diagnostics, 
                            ratio_threshold = periodicity.thresh)
        lat <- .AnalyzeBias(t2, nam = k$dataset[1], var_latlong = 2, 
                            plot_bias = periodicity.diagnostics, 
                            ratio_threshold = periodicity.thresh)
        
        t2.res <- cbind(lon, lat)
        
        names(t2.res) <- c("mle.lon", "rate.ratio.lon", "pass.periodicity.lon", 
                           "zero.mle.lon", "zero.rate.ratio.lon", "pass.zero.lon", 
                           "mle.lat", "rate.ratio.lat", 
                           "pass.periodicity.lat", "zero.mle.lat", 
                           "zero.rate.ratio.lat", "pass.zero.lat")
        t2.res$pass.zero.com <- t2.res$pass.zero.lon & t2.res$pass.zero.lat
        t2.res$pass.periodicity.com <- t2.res$pass.periodicity.lon & t2.res$pass.periodicity.lat
      }
      return(t2.res)
    })
    out.t2 <- do.call("rbind.data.frame", out.t2)
    
  } else {
    filler <- rep(NA, length(test))
    out.t2 <- data.frame(pass.zero.lon = filler, 
                         pass.zero.lat = filler, 
                         pass.zero.com = filler, 
                         pass.periodicity.lon = filler, 
                         pass.periodicity.lat = filler, 
                         pass.periodicity.com = filler)
  }
  # prepare output
  if (output == "detail") {
    out <- data.frame(out.t1, out.t2)
    out$summary <- out$pass.ddmm & out$pass.zero.com & out$pass.periodicity.com
    out <- Filter(function(x) !all(is.na(x)), out)
  }
  if (output == "flags") {
    out <- data.frame(pass.ddmm = out.t1$pass.ddmm, 
                      pass.zero.lon = out.t2$pass.zero.lon, 
                      pass.zero.lat = out.t2$pass.zero.lat, 
                      pass.zero.com = out.t2$pass.zero.com, 
                      pass.periodicity.lon = out.t2$pass.periodicity.lon, 
                      pass.periodicity.lat = out.t2$pass.periodicity.lat, 
                      pass.periodicity.com = out.t2$pass.periodicity.com, 
                      row.names = names(test))
    out$summary <- out$pass.ddmm & out$pass.zero.com & out$pass.periodicity.com
    out <- Filter(function(x) !all(is.na(x)), out)
  }
  if (output == "minimum") {
    out <- data.frame(pass.ddmm = out.t1$pass.ddmm, 
                      pass.zero.com = out.t2$pass.zero.com, 
                      pass.periodicity.com = out.t2$pass.periodicity.com, 
                      row.names = names(test))
    out$summary <- out$pass.ddmm & out$pass.zero.com & out$pass.periodicity.com
    out <- Filter(function(x) !all(is.na(x)), out)
  }
  return(out)
}