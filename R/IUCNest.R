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
    
    AOO[x$AOO > NT.thresh[2]] <- "LC"
    AOO[x$AOO < NT.thresh[2]] <- "NT"
    AOO[x$AOO < VU.thresh[2]] <- "VU"
    AOO[x$AOO < EN.thresh[2]] <- "EN"
    AOO[x$AOO < CR.thresh[2]] <- "CR"
    
    EOO[x$EOO > NT.thresh[1]] <- "LC"
    EOO[x$EOO < NT.thresh[1]] <- "NT"
    EOO[x$EOO < VU.thresh[1]] <- "VU"
    EOO[x$EOO < EN.thresh[1]] <- "EN"
    EOO[x$EOO < CR.thresh[1]] <- "CR"
    
    both[x$EOO > NT.thresh[1] | x$AOO > NT.thresh[2]] <- "LC"
    both[x$EOO < NT.thresh[1] & x$AOO < NT.thresh[2]] <- "NT"
    both[x$EOO < VU.thresh[1] & x$AOO < VU.thresh[2]] <- "VU"
    both[x$EOO < EN.thresh[1] & x$AOO < EN.thresh[2]] <- "EN"
    both[x$EOO < CR.thresh[1] & x$AOO < CR.thresh[2]] <- "CR"
    
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