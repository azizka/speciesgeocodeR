\name{CladeByTrait}
\alias{CladeByTrait}

\title{
Extract Clades Based on Trait Values
}
\description{
Extract clades from a larger phylogeny based on a binary trait, with options to choose minimum and maximum clade size as well as the fraction of species sharing the trait
}
\usage{
CladeByTrait(x, tree, prefix, min_clade_size, max_clade_size, 
             monophyly_threshold, summary = F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
a \code{data.frame}, with two columns: species, and the binary trait of interest (1 = present, 0 = absent, NA values permitted).
}
  \item{tree}{
a \code{phylo} object. The phylogenetic tree from which the clades shall be extracted.  
}
  \item{prefix}{
a character string.  A prefix for the output file names.  
}
  \item{min_clade_size}{
numeric.  The minimum number of tips in the resulting clades.  
}
  \item{max_clade_size}{
numeric.  The maximum number of tips in the resulting clades. 
}
  \item{monophyly_threshold}{
numeric.  The number of species in a clade permitted not to exhibit the trait of interest.
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

\keyword{ Area classification }% use one of  RShowDoc("KEYWORDS")
