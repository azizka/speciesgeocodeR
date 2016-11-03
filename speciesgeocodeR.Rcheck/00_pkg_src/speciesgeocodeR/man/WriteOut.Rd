\name{WriteOut}
\alias{WriteOut}

\title{
Write SpeciesgeocodeR results to the Working Directory
}
\description{
This function writes the results of a speciesgeocodeR analysis to the working directory. Output statistics as .txt files, maps and plots as .pdf and a nexus file for use in phylogenetic analyses.
}
\usage{
WriteOut(x,writetype = c("all", "BioGeoBEARS", "coexistence", "graphs", 
                         "maps", "nexus","statistics"), areanames = NULL)
}

\arguments{
  \item{x}{
an object of the class spgeoOUT
}
  \item{writetype}{
a character string. Defines which results should be written to disk. Default is to \dQuote{all}, see details for other options
}
  \item{areanames}{
a character string. If the polygons of the spgeoOUT object were derived from a shape file, this column indicates the name of the column used for area names. See the SpeciesgeocodeR documentation for more information, if the polygons of the spgeoOUT object were derived from a text file leave as default.
}
}
\details{
There are different options for the \code{writetype} argument: \dQuote{all} = all summary tables, plots, maps and a heatplot of species-coexistence if calculated (see \code{\link{CoExClass}}); \dQuote{BioGeoBEARS} = a text file in the geography input format of BioGeoBEARS; \dQuote{coexistence} = the coexistence matrix as table and heatplot \dQuote{graphs} = summary graphs as pdf files; \dQuote{maps} = summary maps as .pdf file; \dQuote{nexus} = write out the species to area classification as nexus file; \dQuote{statistics} = the summary tables as .txt files.
}
\value{
a set of files in the working directory
}

\seealso{
\code{\link{SpGeoCod}}
}
\examples{
\dontrun{
data(lemurs)
data(mdg_poly)

inp <- ReadPoints(lemurs, mdg_poly)
outp <- SpGeoCodH(inp)
WriteOut(outp)
WriteOut(outp, writetype = "graphs")
WriteOut(outp, writetype = "statistics")
WriteOut(outp, writetype = "nexus")
WriteOut(outp, writetype = "coexistence")
}
}

\keyword{ IO }