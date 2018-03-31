is.DESin <- function(x) {
    inherits(x, "DESin")
}

plot.DESin <- function(x, ribbon = TRUE, ...) {
    # species in all areas
    area1 <- lapply(x[["DES_replicates"]], function(k) {
        k[is.na(k)] <- 0
        k[k != 1] <- 0
        colSums(k)
    })
    area1 <- do.call("rbind.data.frame", area1)
    
    area2 <- lapply(x[["DES_replicates"]], function(k) {
        k[is.na(k)] <- 0
        k[k != 2] <- 0
        k[k == 2] <- 1
        colSums(k)
    })
    area2 <- do.call("rbind.data.frame", area2)
    
    areaB <- lapply(x[["DES_replicates"]], function(k) {
        k[is.na(k)] <- 0
        k[k != 3] <- 0
        k[k == 3] <- 1
        colSums(k)
    })
    areaB <- do.call("rbind.data.frame", areaB)
    
    times <- as.numeric(as.character(names(x[["DES_replicates"]][[1]])))
    
    dat.plo <- data.frame(time = rep(times, 3), mean = c(round(colMeans(area1), 
        1), round(colMeans(area2), 1), round(colMeans(areaB), 1)), lwr = c(do.call(pmin, 
        data.frame(t(area1))), do.call(pmin, data.frame(t(area2))), do.call(pmin, 
        data.frame(t(areaB)))), upr = c(do.call(pmax, data.frame(t(area1))), 
        do.call(pmax, data.frame(t(area2))), do.call(pmax, data.frame(t(areaB)))), 
        area = c(rep("Area1", length(times)), rep("Area2", length(times)), rep("Both", 
            length(times))))
    
    plo <- ggplot() + geom_line(data = dat.plo, aes_string(x = "time", y = "mean", 
        group = "area", col = " area")) + scale_x_reverse() + xlab("Time") + 
        ylab("Species") + theme_bw() + theme(legend.title = element_blank())
    
    if (ribbon) {
        plo <- plo + geom_ribbon(data = dat.plo, aes_string(x = "time", ymax = "upr", 
            ymin = "lwr", group = "area", fill = "area"), alpha = 1/5)
    }
    plo
}

summary.DESin <- function(object, ...) {
    ares <- split(object[["input_fossils"]], f = object[["input_fossils"]]$area)
    
    outp.nams <- c("Minimum_age", "Maximum_age", "Number of records", "Mean record age", 
        "Number of taxa", "Mean taxon age")
    area.1 <- c(round(min(ares[[1]]$midpointage), 1), round(max(ares[[1]]$midpointage), 
        1), nrow(ares[[1]]), round(mean(ares[[1]]$midpointage), 1), length(unique(ares[[1]]$species)), 
        round(mean(aggregate(ares[[1]]$midpointage, by = list(ares[[1]]$species), 
            min)$x), 1))
    area.2 <- c(round(min(ares[[2]]$midpointage), 1), round(max(ares[[2]]$midpointage), 
        1), nrow(ares[[2]]), round(mean(ares[[2]]$midpointage), 1), length(unique(ares[[2]]$species)), 
        round(mean(aggregate(ares[[2]]$midpointage, by = list(ares[[2]]$species), 
            min)$x), 1))
    
    list(Number_of_areas = length(ares), Input_Data = data.frame(row.names = outp.nams, 
        Area_1 = area.1, Area_2 = area.2), Number_of_Replicates = length(object[["DES_replicates"]]))
}

write.DESin <- function(x, file) {
    for (i in 1:length(x[["DES_replicates"]])) {
        write.table(x[["DES_replicates"]][[i]], paste(file, "_rep", i, ".txt", 
            sep = ""), na = "NaN", sep = "\t", row.names = TRUE, quote = FALSE)
    }
}
