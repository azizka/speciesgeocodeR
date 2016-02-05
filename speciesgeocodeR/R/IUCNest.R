IUCNest <- function(x, value = c("all", "AOO", "EOO", "IUCN50", "IUCN100", "IUCN500"), 
                    NT.thresh = c(30000, 3000), VU.thresh = c(20000, 2000), 
                    EN.thresh = c(5000,500), CR.thresh = c(100, 10)) {
  
  if (is(x) == "range.sizes") {
    AOO <- rep("DD", nrow(x))
    EOO <- rep("DD", nrow(x))
    both <- rep("DD", nrow(x))
    IUCN50 <- rep(0, nrow(x))
    IUCN100 <- rep(0, nrow(x))
    IUCN500 <- rep(0, nrow(x))
    
    AOO[x$AOO > 3000] <- "LC"
    AOO[x$AOO < 3000] <- "NT"
    AOO[x$AOO < 2000] <- "VU"
    AOO[x$AOO < 500] <- "EN"
    AOO[x$AOO < 10] <- "CR"
    
    EOO[x$EOO > 30000] <- "LC"
    EOO[x$EOO < 30000] <- "NT"
    EOO[x$EOO < 20000] <- "VU"
    EOO[x$EOO < 5000] <- "EN"
    EOO[x$EOO < 100] <- "CR"
    
    both[x$EOO > 30000 | x$AOO > 3000] <- "LC"
    both[x$EOO < 30000 & x$AOO < 3000] <- "NT"
    both[x$EOO < 20000 & x$AOO < 2000] <- "VU"
    both[x$EOO < 5000 & x$AOO < 500] <- "EN"
    both[x$EOO < 100 & x$AOO < 10] <- "CR"
    
    IUCN50[both == "LC"] <- 5e-05
    IUCN50[both == "NT"] <- 0.004
    IUCN50[both == "VU"] <- 0.05
    IUCN50[both == "EN"] <- 0.42
    IUCN50[both == "CR"] <- 0.97
    
    IUCN100[both == "LC"] <- 1e-04
    IUCN100[both == "NT"] <- 0.01
    IUCN100[both == "VU"] <- 0.1
    IUCN100[both == "EN"] <- 0.667
    IUCN100[both == "CR"] <- 0.999
    
    IUCN500[both == "LC"] <- 5e-04
    IUCN500[both == "NT"] <- 0.02
    IUCN500[both == "VU"] <- 0.39
    IUCN500[both == "EN"] <- 0.996
    IUCN500[both == "CR"] <- 1
    
    warning("probability based on assessment from AOO and EOO")
    
    out <- data.frame(AOO, EOO, both, IUCN50, IUCN100, IUCN500)
    
    if ("all" %in% value) {
      return(out)
    } else {
      if (!"AOO" %in% value) {
        out <- out[, -1]
      }
      if (!"EOO" %in% value) {
        out <- out[, -2]
      }
      if (!"both" %in% value) {
        out <- out[, -3]
      }
      if (!"IUCN50" %in% value) {
        out <- out[, -4]
      }
      if (!"IUCN100" %in% value) {
        out <- out[, -5]
      }
      if (!"IUCN500" %in% value) {
        out <- out[, -6]
      }
    }
  } else {
    stop("only defined for class 'range.sizes'")
  }
}