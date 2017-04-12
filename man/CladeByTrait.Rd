\name{CladeByTrait}
\alias{CladeByTrait}

\title{
Extract clades based on trait values
}
\description{
Extract clades from a larger phylogeny based on trait values, with options to choose minimum and maximum clade size as well as the fraction of species sharing a certain trait value
}
\usage{
CladeByTrait(x, tree, prefix, min_clade_size, max_clade_size, 
             monophyly_threshold, summary = F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
a data.frame, with two columns: species, and the binary trait of interest (1 = present, 0 = absent, NA values permitted).
}
  \item{tree}{
the phylogenetic tree from which the clades shall be extracted in phylo format.  
}
  \item{prefix}{
a character string.  A prefix for the output file names.  
}
  \item{min_clade_size}{
numerical.  The minimum number of tips in the resulting clades.  
}
  \item{max_clade_size}{
numerical.  The maximum number of tips in the resulting clades. 
}
  \item{monophyly_threshold}{
numerical.  The number of species in a clade permitted not to exhibit the calde of interest.
}
  \item{summary}{
logical. If TRUE a summary file is written to disk.
}
}

\value{
A set of nexus files and plots in the working directory
}
\author{
Alexander Zizka, Ruud Scharn
}

\note{
See \url{https://github.com/azizka/speciesgeocodeR/wiki} for more details and tutorials.
}

\examples{
sp.nam <-  unique(paste(sample(letters, size = 250, replace = TRUE), 
                        sample(letters, size = 250, replace = TRUE),
			sep = ""))
tr.dat <- sample(c(0,1), size = length(sp.nam), replace = TRUE)
trait <- data.frame(species = sp.nam, trait = tr.dat)
}

\keyword{ datgen }% use one of  RShowDoc("KEYWORDS")
