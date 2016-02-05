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
CalcRange(lemurs, index = "EOO", eoo.value = "shape", terrestrial = F)
## Not run: 
##D CalcRange(lemurs, index = "EOO", eoo.value = "area", eoo.terrestrial = F)
##D CalcRange(lemurs, index = "AOO", eoo.value = "area", eoo.terrestrial = F)
##D CalcRange(lemurs, index = c("AOO", "EOO"), eoo.value = "area", eoo.terrestrial = F)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("CalcRange", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
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

data(lemurs)
data(mdg_poly)

inp <- ReadPoints(lemurs, mdg_poly)
outp <- SpGeoCodH(inp)
outpcoex <- CoExClass(outp)
outpcoex$coexistence_classified



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("CoExClass", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("GeoClean")
### * GeoClean

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: GeoClean
### Title: Automated Cleaning of Geographic Coordinates
### Aliases: GeoClean
### Keywords: spatial manipl

### ** Examples

data(lemurs_test)
require(maptools)

#run all tests
data(wrld_simpl)
data(countryref)
test <- GeoClean(lemurs_test, GBIFhead = TRUE,
                 countrycentroid = TRUE, contthresh = 0.5,
		 capitalcoords = TRUE, capthresh = 0.5,
		 countrycheck = FALSE, outp = "cleaned")

insidecountry <- GeoClean(test, isna = FALSE, isnumeric = FALSE,
                          coordinatevalidity = FALSE,
			  containszero = FALSE, zerozero = FALSE,
			  latequallong = FALSE, GBIFhead = FALSE,
			  countrycentroid = FALSE,
			  contthresh = 0.5, capitalcoords = FALSE,
			  capthresh = 0.5, countrycheck = TRUE,
			  polygons = wrld_simpl)
#outp = "detailed"
test <- GeoClean(lemurs_test, GBIFhead = TRUE,
                 countrycentroid = TRUE, contthresh = 0.5,
		 capitalcoords = TRUE, capthresh = 0.5,
		 countrycheck = FALSE, outp = "detailed")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("GeoClean", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("GetElevation")
### * GetElevation

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: GetElevation
### Title: Elevation Data for Multiple Species
### Aliases: GetElevation
### Keywords: IO spatial

### ** Examples

data(lemurs)
## Not run: 
##D GetElevation(lemurs)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("GetElevation", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
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
nameEx("MapGrid")
### * MapGrid

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: MapGrid
### Title: Plotting Rasters in the Geographical Context
### Aliases: MapGrid
### Keywords: spatial hplot

### ** Examples

data(lemurs)
data(mdg_poly)

inp <- ReadPoints(lemurs, mdg_poly)
outp <- SpGeoCodH(inp)
e <- c(42, 52, -27, -10)
ras <- RichnessGrid(outp, e, reso = 60,  "abu")
MapGrid(ras)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("MapGrid", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
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
dat <- CalcRange(data.frame(lemurs_in$identifier,
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
nameEx("ReadPoints")
### * ReadPoints

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ReadPoints
### Title: Loading Data into SpeciesgeocodeR
### Aliases: ReadPoints
### Keywords: IO

### ** Examples

data(lemurs)
data(mdg_poly)

inp <- ReadPoints(lemurs, mdg_poly)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ReadPoints", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
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
nameEx("SpGeoCodH")
### * SpGeoCodH

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: SpGeoCodH
### Title: A Standard a SpeciesgeocodeR Area Classification
### Aliases: SpGeoCodH
### Keywords: spatial

### ** Examples

data(lemurs)
data(mdg_poly)

inp <- ReadPoints(lemurs, mdg_poly)
outp <- SpGeoCodH(inp)
names(outp)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("SpGeoCodH", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("SpeciesGeoCoder")
### * SpeciesGeoCoder

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: SpeciesGeoCoder
### Title: A Complete SpeciesgeocodeR Analysis
### Aliases: SpeciesGeoCoder
### Keywords: spatial IO

### ** Examples

## Not run: 
##D data(lemurs)
##D data(mdg_poly)
##D SpeciesGeoCoder(lemurs, mdg_poly
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("SpeciesGeoCoder", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
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
nameEx("lemurs_in")
### * lemurs_in

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: lemurs_in
### Title: Example for an spgeoIN Object
### Aliases: lemurs_in
### Keywords: datasets

### ** Examples

data(lemurs_in)
summary(lemurs_in)
plot(lemurs_in)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("lemurs_in", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
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
nameEx("plot.spgeoIN")
### * plot.spgeoIN

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot.spgeoIN
### Title: Plot Method for SpgeoIN Objects
### Aliases: plot.spgeoIN
### Keywords: methods

### ** Examples

data(lemurs)
data(mdg_poly)

inp <- ReadPoints(lemurs, mdg_poly)
plot(inp)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plot.spgeoIN", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
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
##D SpeciesGeoCoder(lemurs, mdg_poly)
## End(Not run)

data(lemurs)
data(mdg_poly)
inp <- ReadPoints(lemurs, mdg_poly)

outp <- SpGeoCodH(inp)
e <- c(42, 52, -27, -10)
ivesia_abu <- RichnessGrid(outp, e, reso = 60, type = "abu")

outp <- SpGeoCodH(inp)
e <- c(42, 52, -27, -10)
lemurs_div <- RichnessGrid(outp, e, reso = 60, type = "spnum")
MapGrid(lemurs_div)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("speciesgeocodeR-package", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("summary.spgeoIN")
### * summary.spgeoIN

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: summary.spgeoIN
### Title: Summary Method for SpgeoIN
### Aliases: summary.spgeoIN
### Keywords: methods

### ** Examples

data(lemurs)
data(mdg_poly)
inp <- ReadPoints(lemurs, mdg_poly)
summary(inp)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("summary.spgeoIN", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
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
