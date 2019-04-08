DESin <- function(x, 
                  recent, 
                  taxon = "scientificName",
                  area = "higherGeography",
                  age1 = "earliestAge",
                  age2 = "latestAge",
                  bin.size = 5, 
                  reps = 3, 
                  verbose = FALSE) {
    
    # load data
    if (is.data.frame(x)) {
        dat <- x
    } else {
        dat <- read.table(x, sep = "\t", header = TRUE, row.names = NULL)
    }

    # CHECK IF this is still necessary, and why the summary method still uses
    # midpoints
    
    if (!"midpointage" %in% names(dat)) {
        dat$midpointage <- (dat[[age1]] + dat[[age2]])/2
        warning("column midpointage not found, calculating from earliestage and latestage")
    }
    
    # load and prepare recent data
    if (is.data.frame(recent)) {
        rece <- recent
    }else {
        rece <- read.table(recent, header = TRUE, sep = "\t", stringsAsFactors = FALSE, 
            row.names = NULL)
    }
    

    rece[[area]] <- as.character(rece[[area]])
    rece[rece[[area]] == sort(unique(rece[[area]]))[1], area] <- 1
    rece[rece[[area]] == sort(unique(rece[[area]]))[2], area] <- 2
    rece <- unique(rece)
    rece[[area]] <- as.numeric(rece[[area]])
    rece <- aggregate(rece[[area]] ~ rece[[taxon]], FUN = sum)
    names(rece) <- c(taxon, area)

    # code fossil data
    outp <- list()
    for (i in 1:reps) {
        if (verbose) {
            print(sprintf("producing replicate %s of %s", i, reps))
        }
        
        # simulate random age between min and max
        dat$age <- sapply(seq(1, nrow(dat)), function(x) stats::runif(1, max = dat[[age1]][x], 
            min = dat[[age2]][x]))
        
        # define age class cutter and cut ages into timebins
        cutter <- seq(0, max(dat$age), by = bin.size)
        dat$timeint <- as.numeric(as.character(cut(dat$age, breaks = cutter, 
            digits = 5, labels = cutter[-length(cutter)])))
        
        # code the presence in each regions per species
        dat.list <- split(dat, dat[[taxon]])
        
        binned <- lapply(dat.list, function(k) {
            dat.out <- data.frame(timebin = cutter, 
                                  area1 = rep(0, length(cutter)), 
                                  area2 = rep(0, length(cutter)))
            
            if (length(k[[area]] == sort(unique(dat[[area]])[1])) > 0) {
                dat.out[dat.out$timebin %in% unlist(k[k[[area]] == unique(dat[[area]])[1], "timeint"]), "area1"] <- 1
            }
            if (length(k[[area]] == sort(unique(dat[[area]])[2])) > 0) {
                dat.out[dat.out$timebin %in% unlist(k[k[[area]] == unique(dat[[area]])[2], "timeint"]), "area2"] <- 2
            }
            presence <- rowSums(dat.out[, 2:3])
            return(presence)
        })
        
        # set timebins before first appearance to NaN
        out <- lapply(binned, function(k) {
            if (!any(k > 0)) {
                k <- rep("nan", length(k))
                return(as.numeric(k))
            } else {
                if (max(which(k > 0)) < length(k)) {
                  k[(max(which(k > 0)) + 1):length(k)] <- "nan"
                  return(as.numeric(k))
                } else {
                  return(k)
                }
            }
        })
        
        # output format
        out <- do.call("rbind.data.frame", out)
        names(out) <- (cutter + bin.size / 2)
        out <- rev(out)
        out[[taxon]] <- names(dat.list)
        outp[[i]] <- out
    }
    
    
    # combine recent and fossil data
    outp2 <- lapply(outp, function(k) {
        outo <- merge(k, rece, by = taxon, all.x = TRUE)
        outo[[area]][is.na(outo[[area]])] <- 0
        #rownames(outo) <- outo[, 1]
        #outo <- outo[, -1]
        names(outo)[ncol(outo)] <- 0
        return(outo)
    })
    
    # make sure all replicates cover the same time spann, i.e. add additional
    # columns before the first time column
    meas <- sapply(outp2, "ncol")
    
    if (max(meas) != min(meas)) {
        numb <- which(meas < max(meas))
        for (i in numb) {
            dat.int <- outp2[[i]]
            repl <- nrow(dat.int) * (max(meas) - meas[i])  # how many NaNs are needed
            dat.comb <- c(rep(NaN, times = repl), unlist(dat.int[,-1]))
            dat.int <- data.frame(matrix(dat.comb, 
                                         nrow = nrow(dat.int), 
                                         ncol = max(meas)-1, 
                byrow = FALSE))
            dat.int <- data.frame(outp[[i]][taxon],
                                  dat.int)
            names(dat.int) <- names(outp2[[which(meas == max(meas))[1]]])
            #rownames(dat.int) <- rownames(outp2[[i]])
            outp2[[i]] <- dat.int
        }
    }
    
    # create output object
    outp <- list(input_fossils = dat, 
                 input_recent = rece, 
                 DES_replicates = outp2,
                 bin_size = bin.size,
                 area = area,
                 taxon = taxon)
    names(outp) <- c("input_fossils", "input_recent", "DES_replicates", "bin_size", "area", "taxon")
    class(outp) <- c("DESin", "list")
    return(outp)
}
