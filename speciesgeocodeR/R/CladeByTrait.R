# format data.frame : 2 columns, header: darwinCOre, 2 columns: 1.: species name, higherGeography: presence/absence: 1_ present, 0 = absent, NA = no info
#prefix = group name as prefix for outputfiles
CladeByTrait <- function(x, tree, prefix, min_clade_size, max_clade_size, monophyly_threshold, summary = F){
  
  ##Data preparation
  if(is.data.frame(x)){
    geo <- data.frame(trait = x[, 2], row.names = x[, 1])
  }
  
    dat.tre<- suppressWarnings(geiger::treedata(tree, geo))
    
    #records from the tree with no occrrences
    nodata <- tree$tip.label[!tree$tip.label %in% row.names(geo)]
    nodatatab <- data.frame(rep(NA, length(nodata)), row.names = nodata)
    
    #records not occurring in the study area
    outsidetab <- data.frame(subset(dat.tre$trait, dat.tre$data[, 1] == 0))
    outside <- rownames(outsidetab)
    
    #standardize names
    names(outsidetab) <- names(nodatatab) <- names(data.frame(dat.tre$trait))
    
    #combine species with data and species in the phylogeny but without data
    all.classified <- rbind(data.frame(dat.tre$dat), nodatatab)

    #species not in the phylogeny and not in the study area
    others2 <- data.frame(subset(geo, geo[, 1] == 0)[, 1], row.names = rownames(subset(geo, geo[, 1] == 0)))
    names(others2) <- names(geo)[1]

    #create summary files if desired
    if (summary) {
      #A modified input table, taking into account the phylogeny
      write.table(all.classified, paste(prefix, "_area_classification_used.txt", sep = ""), quote = F, 
                  col.names = F, sep = "\t")
      
      #the tree with the area classification as tip colour
      colo <- geo[match(tree$tip.label, rownames(geo)), ]
      colo <- as.character(factor(colo, levels = c(0, 1), labels = c("blue", "red")))
      colo[is.na(colo)] <- "grey"
      
      pdf(paste(prefix, "_phylogeny+traitdata.pdf", sep = ""), 
          height = max(round(c(length(tree$tip.label)/30, 0), 11)), 
          width = 8, 
          paper = "special")
      plot(tree, tip.color = colo, cex = 0.5)
      legend("topleft", bg = "white", fill = c("blue", "red", "grey"), legend = c("Outside", "Inside", "No occurrence data"), 
             cex = 0.5)
      dev.off()
    }
    
  ##finding trait-exclusive clades, this part was written by R. SCharn
    tr <- data$phy
    # create matrix for each node on state 1 ('not checked')
    nodenr <- matrix(nrow = length((length(tr$tip.label) + 1):max(tr$edge)), ncol = 2)
    # sets all nodes to 1 ie all can be checked
    nodenr[, 1] <- 1  
    nodenr[, 2] <- 0
    rownames(nodenr) <- (length(tr$tip.label) + 1):max(tr$edge)
    colnames(nodenr) <- c("checked_monophyletic", "clade_size_mono")
    
    # check for clades monophylethic for area 'X'
    # skip nodes identified with 0 (not monophyletic for a given area OR descendent of monophyletic clade)
    # __ # monophyly_threshold is NUMBER of species not in area of interest __ V=geo[tips(tr,i),area] __ if (length(V[V
    # == 0]) > monophyly_threshold ){ __ nodenr[as.character(i),1]=0 __ } else{
    # monophyly_threshold is percentage of species not in area of interest OUT OF CLADE SIZE
    
    for (i in (length(tr$tip.label) + 1):max(tr$edge)) {
      if (nodenr[as.character(i), 1] == 1) {
        V <- geo[geiger::tips(tr, i), area]
        if (length(V[V == 0])/length(V) >= monophyly_threshold || length(geiger::tips(tr, i)) > max_clade_size) {
          nodenr[as.character(i), 1] = 0
        } else {
          nodenr[as.character(i), 2] = length(geiger::tips(tr, i))  # number of tips in the clade
          nodes <- .getDescend(tr, i)
          nodenr[as.character(nodes[nodes > length(tr$tip.label)]), 1] = 0  # set all descendent noeds to zero (skip check)
          nodenr[as.character(i), 1] = 2  # indentifier of monophyletic group
        }
      }
    }
    
    # removes from table all nodes which are not of interest clades TABLE: column 1: 0-> discarded (there shouldn't be
    # any); 1-> untested (there shouldn't be any at the end of the loop); 2-> positive hit CALL RUUD for bug reports or
    # if you want to have a good time (ruud.scharn@gmail.com) column 2: size of the clade
    clades <- subset(nodenr, nodenr[, 2] >= min_clade_size)
    
    ##Write output
    
    #pdf with visualization of the subtrees and write subtrees as enxus files
    lengtab <- as.vector(0)
    subtreelist <- as.list(0)
    
    pdf(paste(prefix, "_subtrees.pdf", sep = ""), height = 11, width = 8, paper = "special")
    for (i in 1:length(clades[, 2])) {
      node <- as.numeric(row.names(clades)[i])
      subtree <- ape::extract.clade(tr, node, root.edge = 0, interactive = F)
      lengtab[i] <- max(picante::node.age(subtree)$ages)
      ape::write.tree(subtree, file = paste("clade_", i, "_area_", area, ".tre", sep = ""))
      
      colo <- geo[match(subtree$tip.label, rownames(geo)), ]
      colo <- gsub(1, "red", colo)
      colo <- gsub(0, "blue", colo)
      colo[is.na(colo)] <- "grey"
      plot(subtree, tip.color = colo, cex = 0.5, main = paste("Clade", i, sep = "_"))
      legend("topleft", bg = "white", fill = c("blue", "red", "grey"), legend = c("Outside", "Inside", "No occurrence data"), 
             cex = 0.7)
      ape::add.scale.bar()
    }
    dev.off()

    # log file and log to screen
    descwrite <- c("date", "input_prefix", 
                   "min_clade_size", 
                   "max_clade_size", 
                   "monophyly_threshold", 
                   "Species in Phylogeny",
                   "SPecies with traits data",
                   "Species in trait and phylogeny", 
                   "Species with no trait information", 
                   "Number of clades returned", 
                   "Mean_tip_number_per_tree",
                   "MEdian tip age")
    
    logwrite <- c(as.character(Sys.Date()), 
                  prefix, 
                  min_clade_size, 
                  max_clade_size, 
                  monophyly_threshold, 
                  ape::Ntip(tree),
                  nrow(geo),
                  ape::Ntip(tree) - length(nodata),
                  length(nodata),
                  length(clades[, 2]),
                  round(mean(clades[, 2]), digits = 2),
                  round(median(lengtab), 1))

    if(summary){
      
    #write log file
      if (!"GetAreasClades_log.txt" %in% list.files()) {
      write.table(as.vector(data.frame(t(descwrite))), "CladesByTrait_log.txt", col.names = F, row.names = F, quote = F, 
                  sep = "\t")
    }
    
    write.table(as.vector(as.data.frame(t(logwrite))), col.names = F, row.names = F, "CladesByTrait_log.txt", append = T, 
                quote = F, sep = "\t")
    
    
    #plot summary graphs
    pdf(paste(prefix, "_summary_graphs.pdf", sep = "")[1], paper = "a4")
    par(mar = c(12, 3, 3, 3))
    barplot(as.numeric(logwrite[6:11]), names.arg = paste(descwrite[6:11], " (", logwrite[6:10], ")", sep = ""), las = 2, cex.names = 0.7, col = "brown", 
            main = paste(prefix, " species numbers", sep = "")[1])
    box("plot")
    hist(lengtab, main = "Node age", xlab = "Node age", col = "brown")
    hist(clades[, 2], main = "Number of tips per clade", xlab = "Number of tips per clade", col = "brown")
    dev.off()
    
    #summary table with indeces for each clade
      clades_write <- data.frame(clades, node_nr = as.numeric(rownames(clades)), row.names = 1:length(clades[, 1]))[, c(3, 1, 2)]  #add node number as column
      write.table(clades_write, paste("clade_identity_and_size_", area, ".txt", sep = ""), sep = "\t")
      
    #log to screen
      warning(paste(descwrite, logwrite, rep("\n", length(logwrite))))
    }
}