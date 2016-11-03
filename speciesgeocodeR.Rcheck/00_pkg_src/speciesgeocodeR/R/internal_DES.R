.getDescend <- function(tree, node, curr = NULL) { #Code from Ruud Scharn
  if (is.null(curr)) {
    curr <- vector()
  }
  daughters <- tree$edge[which(tree$edge[, 1] == node), 2]
  curr <- c(curr, daughters)
  w <- which(daughters >= length(tree$tip))
  if (length(w) > 0) {
    for (i in 1:length(w)) {
      curr <- .getDescend(tree, daughters[w[i]], curr)
    }
  }
  return(curr)
} 

.InputData <- function(x, ...) {
  
  # Number of samples
  occ.all <- table(x[[1]]$midpointAge)
  occ.reg1 <- table(subset(x[[1]], x[[1]]$higherGeography == unique(x[[1]]$higherGeography)[1], 
                           select = "midpointAge"))
  occ.reg2 <- table(subset(x[[1]], x[[1]]$higherGeography == unique(x[[1]]$higherGeography)[2], 
                           select = "midpointAge"))
  plot(1, 1, xlim = c(max(as.numeric(names(occ.reg1))), min(as.numeric(names(occ.reg1)))),
       ylim = c(min(occ.all), max(occ.all)), xlab = "Time", 
       ylab = "Number of Records", type = "n")
  points(occ.reg1 ~ as.numeric(names(occ.reg1)), type = "b", col = "blue", ...)
  points(occ.reg2 ~ as.numeric(names(occ.reg2)), type = "b", col = "red", ...)
  legend("topleft", legend = as.character(unique(x[[1]]$higherGeography)), 
         col = c("blue", "red"), lty = 1, pch = 1)
  title("Number of samples")
  
  # Number of taxa
  dd <- split(x[[1]], f = x[[1]]$higherGeography)
  dd <- lapply(dd, function(x) aggregate(x$scientificName, by = list(x$midpointAge, 
                                                                     x$scientificName), length))
  dd <- lapply(dd, function(x) aggregate(x$Group.2, by = list(x$Group.1), 
                                         length))
  
  plot(1, 1, xlim = c(max(c(as.numeric(dd[[1]]$Group.1)), as.numeric(dd[[2]]$Group.1)), 
                      min(c(as.numeric(dd[[1]]$Group.1)), as.numeric(dd[[2]]$Group.1))), 
       ylim = c(min(c(dd[[1]]$x), dd[[2]]$x), max(c(dd[[1]]$x), dd[[2]]$x)), 
       xlab = "Time", ylab = "Number of Records", 
       type = "n")
  
  points(dd[[1]]$x ~ as.numeric(dd[[1]]$Group.1), type = "b", col = "blue")  #, ...)
  points(dd[[2]]$x ~ as.numeric(dd[[2]]$Group.1), type = "b", col = "red")  #, ...)
  legend("topleft", legend = as.character(unique(x[[1]]$higherGeography)), 
         col = c("blue", "red"), lty = 1, pch = 1)
  title("Number of taxa")
  
  # boxplot fossil ages
  boxplot(x[[1]]$midpointAge ~ x[[1]]$higherGeography, col = c("blue", "red"))
  title("Fossil ages")
  
  # Number of records per taxon
  tax.num <- aggregate(x[[1]]$midpointAge, by = list(x[[1]]$scientificName, x[[1]]$higherGeography), 
                       length)
  boxplot(tax.num$x ~ tax.num$Group.2, col = c("blue", "red"))
  title("Number of records per Taxon")
  
  #fraction of taxa per area in recent data
  frq <- round((table(x[[2]]$higherGeography)/ sum(table(x[[2]]$higherGeography))), 2)
  barplot(frq)
  box("plot")
  title("Fraction of Taxa per Area (present day)")
}


.ReplicateAges <- function(x) {
  meas <- unlist(lapply(x[[3]], length))
  
  if(isTRUE(all.equal(max(meas) , min(meas)))){
    dat <-x[[3]]
  }else{
    numb <- which(meas < max(meas))
    for(i in 1:length(numb)){
      dat <- x[[3]]
      dat[[numb[i]]] <- c(rep(0, length(numb)), dat[[numb[i]]])
      names(dat[[numb[i]]])[1] <- (max(as.numeric(names(dat[[numb[i]]])), na.rm = T) + x[[4]])
    }
    
  }
  
  reg1 <- lapply(x[[3]], function(k) {apply(k[,-1], 2, function(z){
    test <- z[as.numeric(z) == 1]
    test <- length(test[complete.cases(test)])
    return(test)})})
  are1all <- do.call("rbind.data.frame", reg1)
  names(are1all) <- names(reg1[[1]])
  
  reg2 <- lapply(x[[3]], function(k) {apply(k[,-1], 2, function(z){
    test <- z[as.numeric(z) == 2]
    test <- length(test[complete.cases(test)])
    return(test)})})
  are2all <- do.call("rbind.data.frame", reg2)
  names(are2all) <- names(reg2[[1]])
  
  reg3 <- lapply(x[[3]], function(k) {apply(k[,-1], 2, function(z){
    test <- z[as.numeric(z) == 3]
    test <- length(test[complete.cases(test)])
    return(test)})})
  are3all <- do.call("rbind.data.frame", reg3)
  names(are3all) <- names(reg3[[1]])
  
  tot <- lapply(x[[3]], function(k) {apply(k[,-1], 2, function(z){
    test <- z[as.numeric(z) %in% c(1,2,3)]
    test <- length(test[complete.cases(test)])
    return(test)})})
  totall <- do.call("rbind.data.frame", tot)
  names(totall) <- names(tot[[1]])
  
  are1.max <- apply(are1all, 2, max)
  are1.min <- apply(are1all, 2, min)
  
  are2.max <- apply(are2all, 2, max)
  are2.min <- apply(are2all, 2, min)
  
  plot(1, 1, 
       ylim = c(min(c(are1.min, are2.min)), max(c(are1.max, are2.max))), 
       xlab = "Time", ylab = "Number of Taxa", 
       type = "n")
  polygon(c(names(are1), rev(names(are1))), c(are1.min, rev(are1.max)), 
          col = rgb(0, 0, 255, 125, maxColorValue = 255), 
          border = rgb(0, 0, 255, 125, maxColorValue = 255))
  polygon(c(names(are1), rev(names(are2))), c(are2.min, rev(are2.max)), 
          col = rgb(255, 0, 0, 125, maxColorValue = 255), 
          border = rgb(255, 0, 0, 125, maxColorValue = 255))
  legend("topleft", legend = unique(x[[1]]$higherGeography), 
         fill = c(rgb(0, 0, 255, 125, maxColorValue = 255), rgb(255, 0, 0, 125, maxColorValue = 255)))
  title(sprintf("Taxon number randomized ages, replicates = %s",length(x[[3]])))
  
  
  #Faction per area through time
  reg1all <- round(are1all/totall, 2)
  reg1.min <- apply(reg1all, 2, min)
  reg1.max <- apply(reg1all, 2, max)
  
  reg2all <- round(are2all/totall, 2)
  reg2.min <- apply(reg2all, 2, min)
  reg2.max <- apply(reg2all, 2, max)
  
  reg3all <- round(are3all/totall, 2)
  reg3.min <- apply(reg3all, 2, min)
  reg3.max <- apply(reg3all, 2, max)
  
  # plot
  plot(1, 1, xlim = c(max(as.numeric(names(reg1all))), 
                      min(as.numeric(names(reg1all)))), 
       ylim = c(0, 1.1), 
       xlab = "Time", ylab = "Fraction of Taxa", 
       type = "n")
  polygon(c(names(reg1all), rev(names(reg1all))), c(reg1.min, rev(reg1.max)), 
          col = rgb(0, 0, 255, 100, maxColorValue = 255), 
          border = rgb(0, 0, 255, 100, maxColorValue = 255))
  polygon(c(names(reg2all), rev(names(reg2all))), c(reg2.min, rev(reg2.max)), 
          col = rgb(255, 0, 0, 100, maxColorValue = 255), 
          border = rgb(255, 0, 0, 100, maxColorValue = 255))
  polygon(c(names(reg3all), rev(names(reg3all))), c(reg3.min, rev(reg3.max)), 
          col = rgb(0, 255, 0, 100, maxColorValue = 255), 
          border = rgb(0, 255, 0, 100, maxColorValue = 255))
  legend("top", legend = c(as.character(unique(x[[1]]$higherGeography)), "both"), 
         fill = c(rgb(0, 0, 255, 125, maxColorValue = 255), 
                  rgb(255, 0, 0, 125, maxColorValue = 255),
                  rgb(0, 255, 0, 125, maxColorValue = 255)), ncol = 3)
  title(sprintf("Fraction of taxa per area, replicates = %s",length(x[[3]])))
}

.SampleLocations <- function(x, ...) {
  map("world", ...)
  points(x[[1]]$decimalLongitude, x[[1]]$decimalLatitude, col = x[[1]]$higherGeography, ...)
  axis(1)
  axis(2)
  title("Input fossil locations")
  legend("topright", legend = unique(x[[1]]$higherGeography), fill = c("blue", "red"))
  box("plot")
}
