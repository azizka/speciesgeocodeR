#What is SpeciesGeoCoder?

SpeciesGeoCodeR is an R package for the preparation for geographic occurrence data in Phylogenetic anaylsis. It contains functions for data cleaning and data exploration and linker function for the BiogeoBEARS and DES biogeographic models.

#Which versions are avaialble?

1. The one available here is written in R
2. A web interface that allows the analysis of data online: https://portal.bils.se/tools/speciesgeocoder
3. A similar package written solely in python: https://github.com/mtop/speciesgeocoder

#Description

Preparation of species occurrences and distribution data for the use in phylogenetic analyses. SpeciesgeocodeR is built for data cleaning, data exploration and data analysis and especially suited for biogeographical and ecological questions on large datasets. The package includes the easy creation of summary-tables and -graphs and geographical maps, the automatic cleaning of geographic occurrence data, the calculating of coexistence matrices and species ranges (EOO) as well as mapping diversity in geographic areas.

#Examples

Example datasets and demo scripts for the major functionalities are provided in the examples folder. A vignette with further exmplanations is available in the vignettes folder.

#For the impatient

```R
data(lemurs)
data(mdg_poly)

SpeciesGeoCoder(lemurs, mdg_poly)

```

#Citation

Zizka A & Antonelli A (2015) speciesgeocodeR: An R package for linking species occurrences, user-defined regions and phylogenetic trees for biogeography, ecology and evolution. bioRxiv. doi: http://dx.doi.org/10.1101/032755.

Töpel M, Calió MF, Zizka A, SCharn R, Silvestro D, Antonelli A (2014) SpeciesGeoCoder: Fast categorisation of species occurrences for analyses of biodiversity, biogeography, ecology and evolution. bioRxiv. doi: http://dx.doi.org/10.1101/009274.

