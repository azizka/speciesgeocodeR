[![Build Status](https://travis-ci.org/azizka/speciesgeocodeR.svg?branch=master)](https://travis-ci.org/azizka/speciesgeocodeR)
[![codecov](https://codecov.io/gh/azizka/speciesgeocodeR/branch/master/graph/badge.svg)](https://codecov.io/gh/azizka/speciesgeocodeR)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/speciesgeocodeR)](https://cran.r-project.org/package=speciesgeocodeR)


# speciesgeocodeR v. 2.0-10

*NOTE: All coordinate cleaning functions have been moved to the new [CoordinateCleaner package](https://github.com/azizka/CoordinateCleaner)!*

An R-package for the preparation for geographic point occurrence data in biogeographic analyses. A major focus is on securing data quality and providing ready to use output for biogeographic software. The main functions include:

* Point-in-polygon classification
* Distibution range estimation
* Species richness maps
* Range size calculations
* Input for PyRate DES
* Automated conservation assessment

# Documentation
Short instructions are given below, see the [wiki pages](https://github.com/azizka/speciesgeocodeR/wiki) for more information and detailed tutorials. For comments, questions and bug reports, please use [speciesgeocodeRatgooglegroups](speciesgeocodeR@googlegroups).

# Installation
## Stable from CRAN

```
install.packages("speciesgeocodeR")
library(speciesgeocodeR)
```

## Developemental using devtools

```
devtools::install_github("azizka/speciesgeocodeR")
library(speciesgeocodeR)
```

# Usage
## Point to Polygon classification

```{r, evaluate = F}
sp.class <- SpGeoCod(lemurs, mdg_biomes, areanames = "name")

summary(sp.class)
plot(sp.class)
plot(sp.class, type = "speciesrichness")
WriteOut(sp.class, type = "nexus")
```

## Distibution range estimation
```
data(lemurs)
rang <- CalcRange(lemurs)
plotHull(rang)
```

## Species Richness maps

```{r, evaluate = F}
data(lemurs)
sp.ras <- RichnessGrid(lemurs, reso = 1)
plot(sp.ras)
```

## Range size calculation
On a *local to regional* scale speciesgeocodeR can calculate species range size as a alpha hull based on a `data.frame` of point occurrences. The `CalcRange` function can return range polygons for each species in the dataset, or calculate range sizes in sqkm (Extent of Occurrence and Area of Occupancy). The output can be used to calculate a species richness grid based on the range sizes using the `RangeRichness` function.

```{r, evaluate = F}
data(lemurs)
rang <- CalcRange(lemurs)
```

## Species richness from ranges
```
data(lemurs)
rang <- CalcRange(lemurs)
sp.rich <- RangeRichness(rang, reso = 0.1)
plot(sp.rich)
```

## Calculate range size
```
data(lemurs)
rang <- CalcRangeSize(lemurs, method = "eoo_pseudospherical")
head(rang)
```
## Input for the Pyrates DES 

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

## Automated conservation assessment
```
occ.exmpl<- data.frame(species = sample(letters, size = 250, replace = TRUE),
                       decimallongitude = runif(n = 250, min = 42, max = 51),
                       decimallatitude = runif(n = 250, min = -26, max = -11))

rang <- CalcRange(occ.exmpl, method = 'pseudospherical', terrestrial = FALSE)
IUCNest(rang)
```
# More
Other versions of speciesgeocodeR include:
1. A web interface that allows the analysis of data online: https://portal.bils.se/speciesgeocoder/tool
2. A equivalent python package written by Mats Töpel https://github.com/mtop/speciesgeocoder

# Citation
Töpel M, Zizka A, Calió MF, Scharn R, Silvestro D, Antonelli A (2016) SpeciesGeoCoder: Fast Categorisation of Species Occurrences for Analyses of Biodiversity, Biogeography, Ecology and Evolution. Systematic Biology.

