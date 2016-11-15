\name{CleanCoordinates}
\alias{CleanCoordinates}

\title{
Geographic Cleaning of Coordinates from Biologic Collections
}
\description{
Cleaning geographic coordinates by multiple empirical tests to flag potentially erroneous coordinates.
}
\usage{
CleanCoordinates(x, countries = NULL, species = NULL, dataset = NULL, 
                 validity = T, zeros = T, capitals = T, centroids = T,
                 seas = T, urban = F, countrycheck = T, outliers = T, GBIF = T, 
                 duplicates = F, verbose = T,
                 output = c("spatialvalid", "summary", "cleaned"),
                 zeros.rad = 0.5, capitals.rad = 0.05, outliers.mtp = 25, 
                 outliers.td = NULL, centroids.rad = 0.01, capitals.ref = NULL, 
                 centroids.detail = c("both", "country", "provinces"),
                 centroids.ref = NULL, seas.ref = NULL, urban.ref = NULL, 
                 country.ref = NULL, report = F)
}

\arguments{
  \item{x}{
a \code{matrix} or \code{data.frame} with two columns in the format: decimallongitude, decimallatitude.  
}
  \item{countries}{
character. A vector of the same length as x, with country information for each record in ISO3 format.  If set to NULL, the countries test is skipped.  Default = NULL 
}
  \item{species}{
character. A vector of the same length as x, with the species identity for each record.  If set to NULL, the outliers test is skipped.  Default = NULL 
}
  \item{dataset}{
character. A vector of the same length as x, with the dataset of origin for each record.  If set to NULL, the dataset test is skipped.  Default = NULL 
}
  \item{validity}{
logical. If TRUE, tests for non-numeric or missing coordinates and coordinates exceeding the global extent (lon/lat, WGS84). Default = TRUE
}
  \item{zeros}{
logical. If TRUE, tests for plain zeros, equal latitude and longitude and a rectangle around the point 0/0. The sidelength of the rectangle defined by \code{zeros.rad}.  Default = TRUE.
}
  \item{capitals}{
logical. If TRUE, tests a rectangle around adm-0 capitals. The side length of the rectangle is \code{capitals.rad} * 2. Default = TRUE.
}
  \item{centroids}{
logical. If TRUE, tests a rectangle around country centroids. The side length of the rectangle is \code{centroids.rad} * 2. Default = TRUE.
}
  \item{seas}{
logical. If TRUE, tests if coordinates fall into the ocean.  Default = TRUE.
}
  \item{urban}{
logical. If TRUE, tests if coordinates are from urban areas.  Default = FALSE.
}
  \item{countrycheck}{
logical.  If TRUE, tests if coordinates are from the country indicated in the country column.  Default = TRUE.
}
  \item{outliers}{
logical. If TRUE, tests each species for outlier records. Depending on the \code{outliers.mtp} and \code{outliers.td} arguments  either flags records that are a minimum distance away from all other records of this species (\code{outliers.td}) or records that are outside a multiple of the interquartile range of minimum distances to the next neighbour of this species (\code{outliers.mtp}).  Default = TRUE. 
}
  \item{GBIF}{
logical.  If TRUE, tests a one-degree rectangel around the GBIF headquarters in Copenhagen, Denmark.  Default = TRUE.
}
  \item{duplicates}{
logical.  If TRUE, tests for duplicate records. THis checks for identical coordinates or if a species vector is provided for identical coordinates within a species. All but the first records are flagged as duplicates.  Default = FALSE.
}
  \item{verbose}{
logical.  If TRUE the progress of testing is reported (to stdout).  Default = TRUE
}
  \item{output}{
character. Defining the output value. See the value section for details.  Default = \dQuote{\code{spatialvalid}}.
}
  \item{zeros.rad}{
numeric. The side length of the rectangle around 0/0 in degrees. Default = 0.5.
}
  \item{capitals.rad}{
numeric. The side length of the rectangle around capital coordinates in degrees. Default = 0.1
}
  \item{outliers.mtp}{
numeric. The multiplier for the interquartile range of the outlier test.  If NULL \code{outliers.td} is used.  Default = 25.
}
  \item{outliers.td}{
numeric.  The minimum distance of a record to all other records of a species to be indentified as outlier, in km. If NULL \code{outliers.mtp} is used. Default = NULL
}
  \item{centroids.rad}{
numeric. The side length of the rectangle around country centroids in degrees. Default = 0.01
}
  \item{capitals.ref}{
\code{data.frame} with alternative reference data for the country capitals test. If set to NULL, the \code{capitals} dataset is used.  Alternatives must be identical in structure.  Default = NULL.
}
  \item{centroids.detail}{
character.  Detail level of centroid test. If set to \dQuote{country} only country (adm-0) centroids are tested, if set to \dQuote{provinces} only province (adm-1) centroids are tested.  Default = \dQuote{both}.
}
  \item{centroids.ref}{
a \code{data.frame} with alternative reference data for the centroid test. If set to NULL, the \code{centroids} dataset is used.  Alternatives must be identical in structure.  Default = NULL.
}
  \item{seas.ref}{
a \code{SpatialPolygonsDataFrame} as alternative reference for the seas test. If set to NULL, the \code{\link{landmass}} dataset is used.  Default = NULL.
}
  \item{urban.ref}{
a \code{SpatialPolygonsDataFrame} as alternative reference for the urban test. If set to NULL, the \code{\link{urbanareas}} dataset is used.  Default = NULL.
}
  \item{country.ref}{
a \code{SpatialPolygonsDataFrame} as alternative reference for the countrycheck test. If set to NULL, the \code{\link{countryborders}} dataset is used.  Default = NULL.
}
  \item{report}{
logical or character.  If TRUE a report file is written to the working directory, summarizing the cleaning results. If a character, the path to which the file should be written.  Default = F.
}
}

\details{
The function needs all coordinates to be formally valid according to WGS84. If the data contains coordinates which are formally invalid, the test will stop after the validity test and returns a vector flagging the potentially problematic rows in the input data. TRUE = non-problematic coordinate, FALSE = potentially problematic coordinates.
}

\value{
Depending on the output argument:
\describe{
\item{\dQuote{spatialvalid}}{a \code{data.frame} with one column for each test. TRUE = clean coordinate, FALSE = potentially problematic coordinates.  The summary column is FALSE if any test flagged the respective coordinate.}
\item{\dQuote{summary}}{a logical vector with the same order as the input data summarizing the results of all test. TRUE = clean coordinate, FALSE = potentially problematic (= at least one test failed).}
\item{\dQuote{cleaned}}{a \code{data.frame} of cleaned coordinates if \code{species = NULL} or a \code{data.frame} with cleaned coordinates and species ID otherwise}
}
}
\note{
See package vignette for details and examples. 
}

\examples{

exmpl <- data.frame(species = sample(letters, size = 250, replace = TRUE),
                    decimallongitude = runif(250, min = 42, max = 51),
                    decimallatitude = runif(250, min = -26, max = -11))

test <- CleanCoordinates(exmpl[, 2:3],species = exmpl[, 1], verbose = FALSE)

plot(test)
}
\keyword{ datagen }

