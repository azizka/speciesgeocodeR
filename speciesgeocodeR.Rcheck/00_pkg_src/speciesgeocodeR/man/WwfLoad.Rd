\name{WwfLoad}
\alias{WwfLoad}

\title{
Load the WWF Terrestrial Ecoregions
}
\description{
Downloads a shape-file containing the terrestrial ecoregions as defined by Olson et al. 2001 from worldwildlife.org/publications/terrestrial-ecoregions-of-theworld, unpacks them and loads the into R.
}
\usage{
WwfLoad(x = "")
}
\arguments{
\item{x}{the path to the folder where the shapes should be saved. If set to "" the files are saved to the working directory.}
}

\value{
An object of the class \code{SpatialPolygonsDataframe}
}
\references{
Olson, D. M., Dinerstein, E. ,Wikramanayake,  E. D., Burgess,  N. D., Powel, G. V. N., Underwood,  E. C.,
Damico, J. A.,Itoua,  I., Strand, H. E., Morrison, J. C., Loucks, C. J., Ricketts,  T. H.,
Kura, Y., Lamoreux,  J. F., Wettengel,  W. W., Hedao, P., and Kassem, K.R. 2001 Terrestrial
ecoregions of the world: A new map of life on earth. \emph{BioScience}, \bold{51}(11):933--938.

\url{http://www.worldwildlife.org/publications/terrestrial-ecoregions-of-the-world}
}

\note{
See the speciesgeocodeR documentation for further information and examples.
}
\examples{
\dontrun{
wwf_eco <- WwfLoad()
plot(wwf_eco)
names(wwf_eco)}
}