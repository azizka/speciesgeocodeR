#speciesgeocodeR v. 2.0

SpeciesgeocodeR is an R-package for the preparation for geographic point occurrence data in biogeographic analyses. A major focus is on securing data quality and providing ready to use output for biogeographic software. The main functions include:

* Automated coordinate cleaning
* Point-in-polygon classification
* Data visualization
* Species richness maps
* Range size calculations

Short instructions are given below, see the [wiki pages](https://github.com/azizka/speciesgeocodeR/wiki) for more information and detailed tutorials. For comments, questions and bug reports, please use [speciesgeocodeRatgooglegroups](speciesgeocodeR@googlegroups).

#Automated cleaning of geographic coordinates
Biological collection data from public databases are prone to various common geographic errors resulting from erroneous data entry or imprecise geo-referencing. The `CleanCoordinates` function of speciesgeocodeR can automatically flag some of the most common problems, if supplied with a data.frame with lat/lon coordinates and optionally species and country information.  

```{r, evaluate = F}
library(speciesgeocodeR)
data(lemurs)
data(mdg_biomes)

#see the function help for a vast set of options
flags <- CleanCoordinates(lemurs, species = "species")

#visualize the results
summary(flags)
plot(flags)

# to exclude all flagged records (make sure this is what you want)
cleaned.df <- lemurs[flags$summary, ]
```
#Point to Polygon classification
Most biogeographic methods require a discrete area classification of species. speciesgeocodeR, enables the quick and reproducible classification of point occurrences into discrete areas based on a `data.frame` with species names and lat/long coordinates and a `spatialPolygonsDataFrame` with the target areas using the `SpGeoCod` function. Elevation and a minimum occurrence threshold can optionally be included. A vast set of visualizations are available via the `type` argument of the `plot` method. The results can be exported in various formats suitable for biogeographic analyses software using the `WriteOut` function.

```{r, evaluate = F}
sp.class <- SpGeoCod(lemurs, mdg_biomes, areanames = "name")

summary(sp.class)
plot(sp.class)
plot(sp.class, type = "speciesrichness")
WriteOut(sp.class, type = "nexus")
```

#Species Richness
Species richness in each polygon can be plotted using the `MapRichness` function and the number of occurrence records or species in a `raster` format can be calculated using the `RichnessGrid` function. 

```{r, evaluate = F}
MapRichness(class)

sp.ras <- RichnessGrid(class, reso = 1)
plot(sp.ras)
```
#Range size calculation
On a *local to regional* scale speciesgeocodeR can calculate species range size as a alpha hull based on a `data.frame` of point occurrences. The `CalcRange` function can return range polygons for each species in the dataset, or calculate range sizes in sqkm (Extent of Occurrence and Area of Occupancy). The output can be used to calculate a species richness grid based on the range sizes using the `RangeRichness` function.

```{r, evaluate = F}
#calculate range shapes
rang <- CalcRange(lemurs, method = "pseudospherical")
plot(rang)

#species richness from ranges
sp.rich <- RangeRichness(rang, reso = 0.1)
plot(sp.rich)

#calculate range size
rang <- CalcRangeSize(lemurs, method = "eoo_pseudospherical")
head(rang)
```
#Input for the Pyrates DES model
SpeciesgeocodeR can create ready-to-use input files for the [Silvestro et al.] DES model, based on a `data.frame` with fossil occurrence and a `data.frame` with recent range size of the study species using the `DESin` function.

```{r, evaluate = F}
#simulate the input data
fos <- data.frame(scientificName = rep(letters[1:4],25),
                  earliestAge = runif(100, min = 60, max = 100),
                  latestAge = runif(100, min = 0, max = 60),
                  higherGeography = sort(rep(c("A", "B"), 50)))

rec <- data.frame(scientificName = c(letters[1:4], letters[1:2]),
                  higherGeography = c(rep("A",4), rep("B", 2)))

#create DES input object
exp1 <- DESin(fos, rec, bin.size = 2, reps = 3)

#explore data
summary(exp1)

#write data to disk for use in pyrate
write.DESin(exp1, file = "Example1_DES_in")
```
#More
Other versions of speciesgeocodeR include:
1. A web interface that allows the analysis of data online: https://portal.bils.se/speciesgeocoder/tool
2. A equivalent python package written by Mats T\"opel https://github.com/mtop/speciesgeocoder

#Citation
Töpel M, Zizka A, Calió MF, Scharn R, Silvestro D, Antonelli A (2016) SpeciesGeoCoder: Fast Categorisation of Species Occurrences for Analyses of Biodiversity, Biogeography, Ecology and Evolution. Systematic Biology.

