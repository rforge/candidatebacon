\name{CandidateBacon-package}
\alias{CandidateBacon-package}
\alias{CandidateBacon}
\docType{package}
\title{
CandidateBacon
}
\description{
Description: Selection of candidate genes is an important step in the exploration of complex genetic architecture. However, it can cause misleading results if not validated properly. The CandidateBacon package is a simple tool that provides such validation and ensures that proposed interaction networks are not statistical artifacts.
}
\details{
\tabular{ll}{
Package: \tab CandidateBacon\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2013-10-29\cr
License: \tab GPL\cr
}
}
\author{
R.M. Nelson & M.E. Pettersson

Maintainer: Ronald Nelson <ronnie.nelson@slu.se>
}
\references{
Nelson R.M. & Pettersson M.E. Degrees of Separation as a statistical tool for finding candidate genes. Submitted
}
\keyword{ package }
\seealso{
}
\examples{
\dontrun{
#CandidateBacon example

network <- import_network("example_network_Cyt.txt")

path_k_to_e <- find_path(network, "k", "e")

draw_network(network, path_k_to_e)

candidate_gene_matrix <- as.data.frame(matrix(data = c("a", "c", "c", "d", "e", "h", "j", "f", "k", "i", "x", "i"), 
	ncol = 2, byrow = T), stringsAsFactors = F)

multi_gene_separation <- multi_gene_paths(network, candidate_gene_matrix)

DOS <- degree_of_separation(network, repeats = 100)
}
}
