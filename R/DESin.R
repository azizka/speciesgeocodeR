DESin <- function(x, recent, bin.size, reps = 3, verbose = FALSE) {
    
    # load data
    if (is.data.frame(x)) {
        dat <- x
    } else {
        dat <- read.table(x, sep = "\t", header = TRUE, row.names = NULL)
    }
    names(dat) <- tolower(names(dat))
    
    if ("scientificname" %in% names(dat)) {
        names(dat) <- gsub("scientificName", "species", names(dat))
    }
    
    if ("higherGeography" %in% names(dat)) {
        names(dat) <- gsub("higherGeography", "area", names(dat))
    }
    
    nes <- c("species", "earliestage", "latestage", "area")
    if (!all(nes %in% names(dat))) {
        stop(sprintf("did not find column %s. Check input data", nes[!nes %in% 
            names(dat)]))
    }
    
    # CHECK IF this is still necessary, and why the summary method still uses
    # midpoints
    
    if (!"midpointage" %in% names(dat)) {
        dat$midpointage <- (dat$earliestage + dat$latestage)/2
        warning("column midpointage not found, calculating from earliestage and latestage")
    }
    
    # load and prepare recent data
    if (is.data.frame(recent)) {
        rece <- recent
    } else {
        rece <- read.table(recent, header = TRUE, sep = "\t", stringsAsFactors = FALSE, 
            row.names = NULL)
    }
    
    names(rece) <- tolower(names(rece))
    
    nes <- c("species", "area")
    if (!all(nes %in% names(rece))) {
        stop(sprintf("did not find column %s. Check input data", nes[!nes %in% 
            names(rece)]))
    }
    
    rece$area <- as.character(rece$area)
    rece[rece$area == sort(unique(rece$area))[1], "area"] <- 1
    rece[rece$area == sort(unique(rece$area))[2], "area"] <- 2
    rece <- unique(rece)
    rece$area <- as.numeric(rece$area)
    rece <- aggregate(area ~ species, data = rece, sum)
    
    # code fossil data
    outp <- list()
    for (i in 1:reps) {
        if (verbose) {
            print(sprintf("producing replicate %s of %s", i, reps))
        }
        
        # simulate random age between min and max
        dat$age <- sapply(seq(1, nrow(dat)), function(x) stats::runif(1, max = dat$earliestage[x], 
            min = dat$latestage[x]))
        
        # define age class cutter and cut ages into timebins
        cutter <- seq(0, max(dat$age), bin.size)
        dat$timeint <- as.numeric(as.character(cut(dat$age, breaks = cutter, 
            digits = 5, labels = cutter[-length(cutter)])))
        
        # code the presence in each regions per species
        dat.list <- split(dat, dat$species)
        binned <- lapply(dat.list, function(x) {
            dat.out <- data.frame(timebin = cutter, area1 = rep(0, length(cutter)), 
                area2 = rep(0, length(cutter)))
            if (length(x$area == sort(unique(dat$area)[1])) > 0) {
                dat.out[dat.out$timebin %in% x[x$area == unique(dat$area)[1], 
                  "timeint"], "area1"] <- 1
            }
            if (length(x$area == sort(unique(dat$area)[2])) > 0) {
                dat.out[dat.out$timebin %in% x[x$area == unique(dat$area)[2], 
                  "timeint"], "area2"] <- 2
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
        outo <- merge(x, rece, by.x = "row.names", by.y = "species", all.x = TRUE)
        outo$area[is.na(outo$area)] <- 0
        rownames(outo) <- outo[, 1]
        outo <- outo[, -1]
        names(outo)[ncol(outo)] <- 0
        return(outo)
    })
    
    # make sure all replicates civer the same time spann, i.e. add additional
    # columns before the first time column
    meas <- sapply(outp2, "ncol")
    
    if (max(meas) != min(meas)) {
        numb <- which(meas < max(meas))
        for (i in numb) {
            dat.int <- outp2[[i]]
            repl <- nrow(dat.int) * (max(meas) - meas[i])  # how many NaNs are needed
            dat.comb <- c(rep(NaN, times = repl), unlist(dat.int))
            dat.int <- data.frame(matrix(dat.comb, nrow = nrow(dat.int), ncol = max(meas), 
                byrow = FALSE))
            names(dat.int) <- names(outp2[[which(meas == max(meas))[1]]])
            rownames(dat.int) <- rownames(outp2[[i]])
            outp2[[i]] <- dat.int
        }
    }
    
    # create output object
    outp <- list(input_fossils = dat, input_recent = rece, DES_replicates = outp2, 
        bin_size = bin.size)
    names(outp) <- c("input_fossils", "input_recent", "DES_replicates", "bin_size")
    class(outp) <- c("DESin", "list")
    return(outp)
}
