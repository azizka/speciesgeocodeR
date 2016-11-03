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
### Title: Range polygons, AOO and EOO from occurrence points
### Aliases: CalcRange
### Keywords: spatial

### ** Examples

data(lemurs)
CalcRange(lemurs, index = c("AOO", "EOO"), eoo.value = "area", eoo.terrestrial = FALSE)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("CalcRange", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("CleanCoordinates")
### * CleanCoordinates

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: CleanCoordinates
### Title: Geographic Cleaning of Coordinates from Biologic Collections
### Aliases: CleanCoordinates
### Keywords: datagen

### ** Examples


exmpl <- data.frame(species = sample(letters, size = 250, replace = TRUE),
                    decimallongitude = runif(250, min = 42, max = 51),
                    decimallatitude = runif(250, min = -26, max = -11))

test <- CleanCoordinates(exmpl[, 2:3],species = exmpl[, 1], verbose = FALSE)

plot(test)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("CleanCoordinates", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("CoExClass")
### * CoExClass

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: CoExClass
### Title: Species Coexistence Matrices in Given Areas.
### Aliases: CoExClass
### Keywords: spatial

### ** Examples

library(sp)
data(lemurs)
data(mdg_poly)

occ.exmpl<- data.frame(species = sample(letters, size = 250, replace = TRUE),
                       decimallongitude = runif(n = 250, min = 42, max = 51),
                       decimallatitude = runif(n = 250, min = -26, max = -11))

pol.exmpl <- 
  SpatialPolygonsDataFrame(
    SpatialPolygons(
      list(Polygons(list(Polygon(cbind(c(44, 46, 46, 44, 44),
                                       c(-24, -24, -13, -13, -24)))), ID = '1'),
           Polygons(list(Polygon(cbind(c(47, 50, 50, 47, 47),
                                       c(-24, -24, -13, -13, -24)))), ID = '2'))),            
                           data.frame(areas = c("Polygon1", "Polygon2")))

outp <- SpGeoCod(occ.exmpl, pol.exmpl, areanames = "areas")
outpcoex <- CoExClass(outp)
outpcoex$coexistence_classified



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("CoExClass", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("DESin")
### * DESin

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: DESin
### Title: Create input files for DES-PyRate
### Aliases: DESin
### Keywords: datgen

### ** Examples

fos <- data.frame(scientificName = rep(letters[1:4],25),
                  earliestAge = runif(100, min = 60, max = 100),
                  latestAge = runif(100, min = 0, max = 60),
                  higherGeography = sort(rep(c("A", "B"), 50)))

rec <- data.frame(scientificName = c(letters[1:4], letters[1:2]),
                  higherGeography = c(rep("A",4), rep("B", 2)))

exp1 <- DESin(fos, rec, bin.size = 2, reps = 3)

summary(exp1)

par(ask = TRUE)
plot(exp1)

## Not run: 
##D write.DES.in(exp1, file = "Example1_DES_in")
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("DESin", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
graphics::par(get("par.postscript", pos = 'CheckExEnv'))
cleanEx()
nameEx("IUCNest")
### * IUCNest

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: IUCNest
### Title: Convert Range Sizes to IUCN Assessment
### Aliases: IUCNest
### Keywords: spatial

### ** Examples

data(lemurs)
rang <- CalcRange(lemurs, terrestrial = F)
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
### Keywords: spatial hplot

### ** Examples

data(lemurs)
data(mdg_poly)

inp <- ReadPoints(lemurs, mdg_poly)
outp <- SpGeoCodH(inp)
MapRichness(outp)



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
### Keywords: spatial hplot

### ** Examples

data("lemurs_in")
dat <- CalcRange(data.frame(lemurs_in$species,
                            lemurs_in$species_coordinates),
                 value = "shape")
PlotHull(dat, xlim = c(-130, -100), ylim = c(30,60))



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
### Keywords: spatial

### ** Examples

data(lemurs)
rang <- CalcRange(lemurs, value = "shape")
sprich <- RangeRichness(rang)
MapGrid(sprich)



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
### Keywords: spatial

### ** Examples

# for x = data.frame
data(lemurs)
e <- c(-125, -105, 30, 50)
dat <- RichnessGrid(lemurs, e, reso = 60, type = "spnum")

# for x = character string
e <- c(-125, -105, 30, 50)

## Not run: 
##D dat <- RichnessGrid("Indri indri", e, reso = 60, type = "spnum")
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("RichnessGrid", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("SpGeoCod")
### * SpGeoCod

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: SpGeoCod
### Title: Point to Polygon Classification from Text Files
### Aliases: SpGeoCod
### Keywords: spatial

### ** Examples

data(lemurs)
data(mdg_poly)

outp <- SpGeoCod(lemurs, mdg_poly)



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
### Keywords: datgen

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
nameEx("WriteOut")
### * WriteOut

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: WriteOut
### Title: Write SpeciesgeocodeR results to the Working Directory
### Aliases: WriteOut
### Keywords: IO

### ** Examples

## Not run: 
##D data(lemurs)
##D data(mdg_poly)
##D 
##D inp <- ReadPoints(lemurs, mdg_poly)
##D outp <- SpGeoCodH(inp)
##D WriteOut(outp)
##D WriteOut(outp, writetype = "graphs")
##D WriteOut(outp, writetype = "statistics")
##D WriteOut(outp, writetype = "nexus")
##D WriteOut(outp, writetype = "coexistence")
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("WriteOut", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("WwfLoad")
### * WwfLoad

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: WwfLoad
### Title: Load the WWF Terrestrial Ecoregions
### Aliases: WwfLoad

### ** Examples

## Not run: 
##D wwf_eco <- WwfLoad()
##D plot(wwf_eco)
##D names(wwf_eco)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("WwfLoad", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("capitals")
### * capitals

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: capitals
### Title: Global Capital Locations
### Aliases: capitals
### Keywords: datasets

### ** Examples

data(capitals)
str(capitals)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("capitals", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("centroids")
### * centroids

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: centroids
### Title: Global Country and Province Centroids
### Aliases: centroids
### Keywords: datasets

### ** Examples

data(centroids)
str(centroids)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("centroids", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("countryborders")
### * countryborders

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: countryborders
### Title: Global Country Borders
### Aliases: countryborders
### Keywords: datasets

### ** Examples

data(countryborders)
str(countryborders)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("countryborders", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("countryref")
### * countryref

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: countryref
### Title: Country Centroids and Country Capitals
### Aliases: countryref
### Keywords: datasets

### ** Examples

data(countryref)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("countryref", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("landmass")
### * landmass

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: landmass
### Title: Global Coastlines
### Aliases: landmass
### Keywords: datasets

### ** Examples

data("landmass")
plot(landmass)



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
### Keywords: datasets

### ** Examples

data(lemurs)
str(lemurs)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("lemurs", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("lemurs_test")
### * lemurs_test

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: lemurs_test
### Title: Distribution of Lemur Species with Problematic Records
### Aliases: lemurs_test
### Keywords: datasets

### ** Examples

data(lemurs_test)
str(lemurs_test)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("lemurs_test", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("mdg_biomes")
### * mdg_biomes

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: mdg_biomes
### Title: WWF Biomes of Madagascar
### Aliases: mdg_biomes
### Keywords: datasets

### ** Examples

data(mdg_biomes)
str(mdg_biomes)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("mdg_biomes", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("mdg_poly")
### * mdg_poly

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: mdg_poly
### Title: WWF Biomes of Madagascar in Table Format
### Aliases: mdg_poly
### Keywords: datasets

### ** Examples

data(mdg_poly)
str(mdg_poly)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("mdg_poly", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plot.DESin")
### * plot.DESin

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot.DESin
### Title: Plot Method for DESin
### Aliases: plot.DESin
### Keywords: methods

### ** Examples

fos <- data.frame(scientificName = rep(letters[1:4],25),
                  earliestAge = runif(100, min = 60, max = 100),
                  latestAge = runif(100, min = 0, max = 60),
                  higherGeography = sort(rep(c("A", "B"), 50)))

rec <- data.frame(scientificName = c(letters[1:4], letters[1:2]),
                  higherGeography = c(rep("A",4), rep("B", 2)))

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
### Keywords: methods

### ** Examples

data(lemurs)
data(mdg_poly)

inp <- ReadPoints(lemurs, mdg_poly)
outp <- SpGeoCodH(inp)
plot(outp)
plot(outp, plottype = "species")
plot(outp, plottype = "polygons")
plot(outp, plottype = "speciesrichness")
plot(outp, plottype = "coexistence")
plot(outp, plottype = "mapspecies")
plot(outp, plottype = "mappolygons")
plot(outp, plottype = "mapunclassified")
plot(outp, plottype = "mapall")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plot.spgeoOUT", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("speciesgeocodeR-package")
### * speciesgeocodeR-package

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: speciesgeocodeR-package
### Title: Prepare Species Distributions for the Use in Phylogenetic
###   Analyses
### Aliases: speciesgeocodeR-package speciesgeocodeR
### Keywords: package

### ** Examples

## Not run: 
##D data(lemurs)
##D data(mdg_poly)
##D 
##D SpGeoCOd(lemurs, mdg_poly)
## End(Not run)

data(lemurs)
data(mdg_poly)

outp <- SpGeoCodH(inp)
e <- c(42, 52, -27, -10)
lemurs_div <- RichnessGrid(outp, e, reso = 60, type = "spnum")
MapGrid(lemurs_div)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("speciesgeocodeR-package", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("summary.DESin")
### * summary.DESin

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: summary.DESin
### Title: Summary Method for DESin
### Aliases: summary.DESin
### Keywords: methods

### ** Examples

fos <- data.frame(scientificName = rep(letters[1:4],25),
                  earliestAge = runif(100, min = 60, max = 100),
                  latestAge = runif(100, min = 0, max = 60),
                  higherGeography = sort(rep(c("A", "B"), 50)))

rec <- data.frame(scientificName = c(letters[1:4], letters[1:2]),
                  higherGeography = c(rep("A",4), rep("B", 2)))

exp1 <- DESin(fos, rec, bin.size = 2, reps = 3)

summary(exp1)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("summary.DESin", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("summary.spgeoOUT")
### * summary.spgeoOUT

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: summary.spgeoOUT
### Title: Summary Method for SpgeoOUT
### Aliases: summary.spgeoOUT
### Keywords: methods

### ** Examples

data(lemurs)
data(mdg_poly)

inp <- ReadPoints(lemurs, mdg_poly)
outp <- SpGeoCodH(inp)
summary(outp)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("summary.spgeoOUT", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("urbanareas")
### * urbanareas

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: urbanareas
### Title: Global Urban Areas
### Aliases: urbanareas
### Keywords: datasets

### ** Examples

data(urbanareas)
plot(urbanareas)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("urbanareas", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("write.DESin")
### * write.DESin

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: write.DESin
### Title: Write Method for DESin
### Aliases: write.DESin
### Keywords: methods

### ** Examples

## Not run: 
##D fos <- data.frame(scientificName = rep(letters[1:4],25),
##D                   earliestAge = runif(100, min = 60, max = 100),
##D                   latestAge = runif(100, min = 0, max = 60),
##D                   higherGeography = sort(rep(c("A", "B"), 50)))
##D 
##D rec <- data.frame(scientificName = c(letters[1:4], letters[1:2]),
##D                   higherGeography = c(rep("A",4), rep("B", 2)))
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
