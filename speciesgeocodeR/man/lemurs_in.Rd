\name{lemurs_in}
\alias{lemurs_in}
\docType{data}
\title{
Example for an spgeoIN Object
}
\description{
This is an example dataset for an object of the class spgeoIN (created by \code{\link{ReadPoints}}).  It contains occurrence data for 39 species of Lemurs in Madagascar.  The data has 3 slots:
identifier = a \code{vector} of character strings containing the species name for each occurrence points
species_coordinates =  a \code{data.frame} with the longitude and latitude positions
polygons = a \code{SpatialPolygons} object with a set of 3 polygons.
}
\usage{
data("lemurs_in")
}
\seealso{
 \code{\link{ReadPoints}}
}
\examples{
data(lemurs_in)
summary(lemurs_in)
plot(lemurs_in)
}
\keyword{datasets}
