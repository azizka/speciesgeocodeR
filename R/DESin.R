DESin <- function(x, recent, bin.size, reps = 1, verbose = F) {
  
  # load data
  if (is.data.frame(x)) {
    dat <- x
  }else{
    dat <- read.table(x, sep = "\t", header = T, row.names = NULL)
  }
  
  nes <- c("scientificName", "earliestAge", "latestAge", "higherGeography")
  if(!all(nes %in% names(dat))){
    stop(sprintf("did not find column %s. Check input data", nes[!nes %in% names(dat)]))
  }
  
  if(! "midpointAge" %in% names(dat)){
    dat$midpointAge <- (dat$earliestAge + dat$latestAge)/2
    warning("column midpointAge not found, calculating from earliestAge and latestAge")
  }
  
  # load and prepare recent data
  if (is.data.frame(recent)) {
    rece <- recent
  }else{
    rece <- read.table(recent, header = T, sep = "\t", stringsAsFactors = F, row.names = NULL)
  }
  
  nes <- c("scientificName", "higherGeography")
  if(!all(nes %in% names(rece))){
    stop(sprintf("did not find column %s. Check input data", nes[!nes %in% names(rece)]))
  }
  
  rece$higherGeography <- as.character(rece$higherGeography)
  rece[rece$higherGeography == unique(rece$higherGeography)[1], "higherGeography"] <- 1
  rece[rece$higherGeography == unique(rece$higherGeography)[2], "higherGeography"] <- 2
  rece <- unique(rece)
  rece$higherGeography <- as.numeric(rece$higherGeography)
  rece <- aggregate(higherGeography ~ scientificName, data = rece, sum)
  
  # code fossil data
  outp <- list()
  for (i in 1:reps) {
    if (verbose) {
      print(sprintf("producing replicate %s of %s", i, reps))
    }
    
    # simulate random age between min and max
    dat$age <- sapply(seq(1, length(dat$scientificName)), function(x) stats::runif(1, max = dat$earliestAge[x], 
                                                                            min = dat$latestAge[x]))
    
    # define age class cutter and cut ages into timebins
    cutter <- seq(0, max(dat$age), bin.size)
    dat$timeint <- as.numeric(as.character(cut(dat$age, breaks = cutter, 
                                               digits = 5, labels = cutter[-length(cutter)])))
    
    # code the presence in each regions per scientificName
    dat.list <- split(dat, dat$scientificName)
    binned <- lapply(dat.list, function(x) {
      dat.out <- data.frame(timebin = cutter, higherGeography1 = rep(0, length(cutter)), 
                            higherGeography2 = rep(0, length(cutter)))
      if (length(x$higherGeography == unique(dat$higherGeography)[1]) > 0) {
        dat.out[dat.out$timebin %in% x[x$higherGeography == unique(dat$higherGeography)[1], 
                                       "timeint"], "higherGeography1"] <- 1
      }
      if (length(x$higherGeography == unique(dat$higherGeography)[2]) > 0) {
        dat.out[dat.out$timebin %in% x[x$higherGeography == unique(dat$higherGeography)[2], 
                                       "timeint"], "higherGeography2"] <- 2
      }
      presence <- rowSums(dat.out[, 2:3])
      return(presence)
    })
    
    # set timebins before first appearance to NaN
    out <- lapply(binned, function(x) {
      if (length(which(x > 0)) == 0) {
        x <- rep("nan", length(x))
        return(as.numeric(x))
      } else {
        if (max(which(x > 0)) < length(x)) {
          x[(max(which(x > 0)) + 1):length(x)] <- "nan"
          return(as.numeric(x))
        } else {
          return(x)
        }
      }
    })
    
    # output format
    out <- do.call("rbind.data.frame", out)
    names(out) <- (cutter + bin.size/2)
    out <- rev(out)
    outp[[i]] <- out
  }
  
  # combine recent and fossil data
  outp2 <- lapply(outp, function(x) {
    outo <- merge(x, rece, by.x = "row.names", by.y = "scientificName", all.x = T)
    outo$higherGeography[is.na(outo$higherGeography)] <- 0
    names(outo)[1] <- "scientificName"
    names(outo)[ncol(outo)] <- 0
    return(outo)
  })
  
  outp <- list(dat, rece, outp2, bin.size)
  class(outp) <- "DESin"
  return(outp)
}