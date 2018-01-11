pkgname <- "speciesgeocodeR"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
base::assign(".ExTimings", "speciesgeocodeR-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('speciesgeocodeR')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("CalcRange")
### * CalcRange

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: CalcRange
### Title: Range Polygons from Occurrence Points
### Aliases: CalcRange
### Keywords: Range size

### ** Examples

occ.exmpl<- data.frame(species = sample(letters, size = 250, replace = TRUE),
                       decimallongitude = runif(n = 250, min = 42, max = 51),
                       decimallatitude = runif(n = 250, min = -26, max = -11))

CalcRange(occ.exmpl, method = "pseudospherical", terrestrial = FALSE)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("CalcRange", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("CalcRangeSize")
### * CalcRangeSize

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: CalcRangeSize
### Title: Species Range Size from Occurrence Records
### Aliases: CalcRangeSize
### Keywords: Range size

### ** Examples

occ.exmpl<- data.frame(species = sample(letters, size = 250, replace = TRUE),
                       decimallongitude = runif(n = 250, min = 42, max = 51),
                       decimallatitude = runif(n = 250, min = -26, max = -11))

CalcRangeSize(occ.exmpl, method = 'eoo_pseudospherical', terrestrial = FALSE, 
              convex.reps = 2)
CalcRangeSize(occ.exmpl, method = 'maxdist', terrestrial = FALSE)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("CalcRangeSize", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("CladeByTrait")
### * CladeByTrait

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: CladeByTrait
### Title: Extract Clades Based on Trait Values
### Aliases: CladeByTrait
### Keywords: Area classification

### ** Examples

sp.nam <-  unique(paste(sample(letters, size = 250, replace = TRUE), 
                        sample(letters, size = 250, replace = TRUE),
			                  sep = ""))
tr.dat <- sample(c(0,1), size = length(sp.nam), replace = TRUE)
trait <- data.frame(species = sp.nam, trait = tr.dat)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("CladeByTrait", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("DESin")
### * DESin

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: DESin
### Title: Create Input Files for DES-PyRate
### Aliases: DESin is.DESin
### Keywords: Area classification PyRate

### ** Examples

fos <- data.frame(species= rep(letters[1:4],25),
                  earliestAge = runif(100, min = 60, max = 100),
                  latestAge = runif(100, min = 0, max = 60),
                  area = sort(rep(c("A", "B"), 50)))

rec <- data.frame(species = c(letters[1:4], letters[1:2]),
                  area = c(rep("A",4), rep("B", 2)))

exp1 <- DESin(fos, rec, bin.size = 2, reps = 3)

summary(exp1)
#plot(exp1)

## Not run: 
##D write.DES.in(exp1, file = "Example1_DES_in")
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("DESin", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("IUCNest")
### * IUCNest

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: IUCNest
### Title: Convert Range Sizes to IUCN Assessment
### Aliases: IUCNest
### Keywords: Range size

### ** Examples

occ.exmpl<- data.frame(species = sample(letters, size = 250, replace = TRUE),
                       decimallongitude = runif(n = 250, min = 42, max = 51),
                       decimallatitude = runif(n = 250, min = -26, max = -11))

rang <- CalcRange(occ.exmpl, method = 'pseudospherical', terrestrial = FALSE)
IUCNest(rang)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("IUCNest", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("MapRichness")
### * MapRichness

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: MapRichness
### Title: Plotting Species Richness in a Set of Polygons
### Aliases: MapRichness
### Keywords: Diversity pattern Visualisation

### ** Examples

data(lemurs)
data(mdg_biomes)

outp <- SpGeoCod(lemurs, mdg_biomes, areanames = 'name')
## Not run: 
##D MapRichness(outp)
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("MapRichness", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("PlotHull")
### * PlotHull

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: PlotHull
### Title: Plotting Species Ranges
### Aliases: PlotHull
### Keywords: Range size Visualisation

### ** Examples

#simulate example occurrences
occ.exmpl<- data.frame(species = sample(letters, size = 250, replace = TRUE),
                       decimallongitude = runif(n = 250, min = 42, max = 51),
                       decimallatitude = runif(n = 250, min = -26, max = -11))


dat <- CalcRange(occ.exmpl, method = "pseudospherical", terrestrial = FALSE)
## Not run: 
##D PlotHull(dat)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("PlotHull", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("RangeRichness")
### * RangeRichness

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: RangeRichness
### Title: Species Richness Raster from Range Polygons
### Aliases: RangeRichness
### Keywords: Range size Diversity pattern

### ** Examples

#simulate example occurrences
occ.exmpl<- data.frame(species = sample(letters, size = 250, replace = TRUE),
                       decimallongitude = runif(n = 250, min = 42, max = 51),
                       decimallatitude = runif(n = 250, min = -26, max = -11))

rang <- CalcRange(occ.exmpl, method = 'pseudospherical')
sprich <- RangeRichness(rang)
plot(sprich)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("RangeRichness", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("RichnessGrid")
### * RichnessGrid

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: RichnessGrid
### Title: Species Richness and Abundance Grids from Point Records
### Aliases: RichnessGrid
### Keywords: Diversity pattern

### ** Examples

# for x = data.frame
data(lemurs)
dat <- RichnessGrid(lemurs, reso = 1, type = "spnum")




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("RichnessGrid", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("SpGeoCod")
### * SpGeoCod

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: SpGeoCod
### Title: Point to Polygon Classification
### Aliases: SpGeoCod is.spgeoOUT as.data.frame.spgeoOUT summary.spgeoOUT
### Keywords: Area classification

### ** Examples

data(lemurs)
data(mdg_biomes)

outp <- SpGeoCod(lemurs, mdg_biomes, areanames = "name")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("SpGeoCod", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("Spgc2Biogeobears")
### * Spgc2Biogeobears

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: Spgc2BioGeoBEARS
### Title: Geography Input for BioGeoBEARS
### Aliases: Spgc2BioGeoBEARS
### Keywords: Area classification

### ** Examples

## Not run: 
##D data(lemurs)
##D data(mdg_poly)
##D 
##D outp <- SpGeoCod(lemurs, mdg_poly)
##D conv <- Spgc2BioGeoBEARS(outp)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("Spgc2Biogeobears", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("WWFload")
### * WWFload

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: WWFload
### Title: Download the WWF Terrestrial Ecoregions
### Aliases: WWFload
### Keywords: Area classification

### ** Examples

## Not run: 
##D wwf_eco <- WWFload()
##D plot(wwf_eco)
##D names(wwf_eco)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("WWFload", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("WriteOut")
### * WriteOut

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: WriteOut
### Title: Write spgeoOUT Objects to the Working Directory
### Aliases: WriteOut
### Keywords: Area classification

### ** Examples

## Not run: 
##D data(lemurs)
##D data(mdg_biomes)
##D 
##D outp <- SpGeoCodH(lemurs, mdg_biomes, areanames = 'name')
##D WriteOut(outp)
##D WriteOut(outp, writetype = "graphs")
##D WriteOut(outp, writetype = "statistics")
##D WriteOut(outp, writetype = "nexus")
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("WriteOut", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("landmass")
### * landmass

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: landmass
### Title: Global Coastlines
### Aliases: landmass
### Keywords: gazetteers

### ** Examples

data("landmass")
## Not run: 
##D plot(landmass)
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("landmass", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("lemurs")
### * lemurs

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: lemurs
### Title: Distribution of Lemur Species
### Aliases: lemurs
### Keywords: Example data

### ** Examples

data(lemurs)
str(lemurs)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("lemurs", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("mdg_biomes")
### * mdg_biomes

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: mdg_biomes
### Title: WWF Biomes of Madagascar
### Aliases: mdg_biomes
### Keywords: Example data

### ** Examples

data(mdg_biomes)
str(mdg_biomes)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("mdg_biomes", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plot.DESin")
### * plot.DESin

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot.DESin
### Title: Plot Method for DESin
### Aliases: plot.DESin
### Keywords: Visualisation

### ** Examples

fos <- data.frame(species = rep(letters[1:4],25),
                  earliestAge = runif(100, min = 60, max = 100),
                  latestAge = runif(100, min = 0, max = 60),
                  area = sort(rep(c("A", "B"), 50)))

rec <- data.frame(species = c(letters[1:4], letters[1:2]),
                  area = c(rep("A",4), rep("B", 2)))

exp1 <- DESin(fos, rec, bin.size = 2, reps = 3)

plot(exp1)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plot.DESin", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plot.spgeoOUT")
### * plot.spgeoOUT

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot.spgeoOUT
### Title: Plot Method for SpgeoOUT Objects
### Aliases: plot.spgeoOUT
### Keywords: Visualisation

### ** Examples

#simulate example occurrences
data(lemurs)
data(mdg_biomes)

outp <- SpGeoCod(lemurs, mdg_biomes, areanames = 'name')
## Not run: 
##D plot(outp)
##D plot(outp, type = "speciesrichness")
##D plot(outp, type = "mapall")
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plot.spgeoOUT", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("summary.DESin")
### * summary.DESin

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: summary.DESin
### Title: Summary Method for DESin
### Aliases: summary.DESin
### Keywords: Area classification

### ** Examples

fos <- data.frame(species = rep(letters[1:4],25),
                  earliestAge = runif(100, min = 60, max = 100),
                  latestAge = runif(100, min = 0, max = 60),
                  area = sort(rep(c("A", "B"), 50)))

rec <- data.frame(species = c(letters[1:4], letters[1:2]),
                  area = c(rep("A",4), rep("B", 2)))

exp1 <- DESin(fos, rec, bin.size = 2, reps = 3)

summary(exp1)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("summary.DESin", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("write.DESin")
### * write.DESin

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: write.DESin
### Title: Write Method for DESin
### Aliases: write.DESin
### Keywords: Area classification

### ** Examples

## Not run: 
##D fos <- data.frame(species = rep(letters[1:4],25),
##D                   earliestAge = runif(100, min = 60, max = 100),
##D                   latestAge = runif(100, min = 0, max = 60),
##D                   area = sort(rep(c("A", "B"), 50)))
##D 
##D rec <- data.frame(species = c(letters[1:4], letters[1:2]),
##D                   area = c(rep("A",4), rep("B", 2)))
##D 
##D exp1 <- DESin(fos, rec, bin.size = 2, reps = 3)
##D 
##D write(exp1)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("write.DESin", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
