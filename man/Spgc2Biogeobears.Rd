\name{Spgc2BioGeoBEARS}
\alias{Spgc2BioGeoBEARS}

\title{
Geography Input for BioGeoBEARS
}
\description{
Convert an object of the class \code{spgeoOUT} in the format to be used as geography input for BioGeoBEARS.
}
\usage{
Spgc2BioGeoBEARS(x, phyl = NULL, file = NULL, true.areas = T, true.species = T)
}

\arguments{
  \item{x}{
an object of the clas \code{'spgeoOUT'}
}
  \item{phyl}{
an object of the class \code{phylo}. An optional phylogeny to be used with BioGeoBEARS, to align species. 
}
  \item{file}{
character.  The complete path and filename to the outputfile.
}
  \item{true.areas}{
logical.  If TRUE, areas with 0 species are removed from the matrix. Default = TRUE.
}
  \item{true.species}{
logical.  If TRUE, species not occurring in any area are removed from the matrix. Default = TRUE.
}
}
\details{
This will create a file in the working directory.
}
\value{
A file in the working directory and a list.
}

\note{
See \url{https://github.com/azizka/speciesgeocodeR/wiki} for more details and tutorials.
}

\author{
Alexander Zizka, Ruud Scharn
}


\examples{
\dontrun{
data(lemurs)
data(mdg_poly)

outp <- SpGeoCod(lemurs, mdg_poly)
conv <- Spgc2BioGeoBEARS(outp)
}

}

\keyword{ datgen }
