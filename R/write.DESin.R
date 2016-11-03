write.DESin <- function(x, file) {
  for (i in 1:length(x[[3]])) {
    write.table(x[[3]][[i]], paste(file, "_rep", i, ".txt", sep = ""), na = "NaN", 
                sep = "\t", row.names = F, quote = F)
  }
}
