\name{plot.spgeoOUT}
\alias{plot.spgeoOUT}

\title{
Plot Method for SpgeoOUT Objects
}
\description{
plot.spgeoOUT is the plotting method for objects of the class spgeoOUT.  Depending on the plottype argument the function can be used to get an overview over the data or plot all elements of the object.
}
\usage{
\method{plot}{spgeoOUT}(x,  plottype = "summary", plotout = F,
                        mode = c("percent", "total"),
			moreborders = F, areanames = NULL, ...)
}

\arguments{
  \item{x}{
an object of the class spgeoOUT
}
  \item{plottype}{
a character string. Controls which data shall be plotted. Default is to \dQuote{summary}. See details for more information.
}
  \item{plotout}{
logical.  If TRUE, no user-input is needed between the plots (par(ask = FALSE)), if  FALSE, user input is needed to browse between plots (par(ask = TRUE)).
}
  \item{mode}{
a character string.  Can be \dQuote{percent} or \dQuote{total} controls if percentage or total is shown for \code{plottype} = \dQuote{species}.  Default is \dQuote{percent}.
}
  \item{moreborders}{
logical.  If TRUE additional borders are added to the maps. Default is FALSE.
}
  \item{areanames}{
character. character string. If the polygons of the spgeoOUT object were derived from a shape file, this column indicates the name of the column used for area names.  See the SpeciesgeocodeR documentation for more information, if the polygons of the spgeoOUT object were derived from a text file leave as default (Default = "").
}
  \item{\dots}{
arguments to be passed to methods, such as graphical parameters (see \code{\link{par}}).
}
}
\details{
The output will be different depending on the plottype argument. \code{plottype} = \dQuote{summary}: a map with all occurrence points and polygons (the red points were not classified to any polygon), the species number for all polygons, and if available, a heatplot visualizing the coexistence matrix; \code{plottype} = \dQuote{species}: a bar chart for each input species showing the number of occurrences in each polygon; \code{plottype} = \dQuote{polygons}: a bar chart for each input-polygon showing the number of occurrences for each species in the polygon; \code{plottype} = \dQuote{speciesrichness}: a bar chart showing species number per polygon; \code{plottype} = \dQuote{coexistence}: a heatplot visualizing the coexistence matrix. The heatcolors code for the percent of occurrences that are shared (per rows); \code{plottype} = \dQuote{mapspecies}: one map per species showing all occurrence points in all polygons; \code{plottype} = \dQuote{mappolygons}: a series of maps, showing each polygon and its close environment, with all samples classified to this polygon. Species are color-coded; \code{plottype} = \dQuote{mapunclassified}: a map with all sample points  that could not be classified to any polygon; \code{plottype} = \dQuote{mapall}: a map of all occurrence points in the object.
}

\seealso{
\code{\link{summary.spgeoOUT}}
}
\examples{
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
}
\keyword{ methods }