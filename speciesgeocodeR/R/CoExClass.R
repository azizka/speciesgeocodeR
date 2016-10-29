CoExClass <- function(x, verbose = F) {
  if (class(x) == "spgeoOUT") {
    dat <- x$spec_table
    spnum <- length(dat$identifier)
    numpol <- length(names(dat))
    coemat <- matrix(0, nrow = spnum, ncol = spnum)
    
    for (j in 1:spnum) {
      if (verbose) {
        cat(paste("Calculate coexistence pattern for species: ", j, "/", spnum, " ", dat$identifier[j], "\n", sep = ""))
      }
      sco <- data.frame(dat$identifier)
      for (i in 2:length(names(dat))) {
        if (dat[j, i] == 0) {
          poly <- rep(0, spnum)
          sco <- cbind(sco, poly)
        }
        if (dat[j, i] > 0) {
          scoh <- dat[, i]
          if (numpol > 2) {
            totocc <- rowSums(dat[j, -1])
          } else {
            totocc <- dat[j, -1]
          }
          for (k in 1:length(scoh)) if (scoh[k] > 0) {
            scoh[k] <- dat[j, i]/totocc * 100
          } else {
            scoh[k] <- 0
          }
          sco <- cbind(sco, scoh)
        }
      }
      if (numpol > 2) {
        coex <- rowSums(sco[, -1])
        coemat[j, ] <- coex
      } else {
        coex <- sco[, -1]
        coemat[j, ] <- coex
      }
    }
    rownames(coemat) <- dat$identifier
    colnames(coemat) <- as.character(dat$identifier)
    x$coexistence_classified <- coemat
    return(x)
  } else {
    stop("function is only defined for class \"SpgeoOUT\".")
  }
}