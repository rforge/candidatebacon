\name{multi_gene_paths}
\alias{multi_gene_paths}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Find the average degree of separation for a set of nodes within a network.
}
\description{
Calculate the average degrees of separation for multiple node pairs. 
}
\usage{
multi_gene_paths(in_network, interaction_list)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{in_network}{
The network used for analysis
}
  \item{interaction_list}{
A two column matrix with all the pairwise interactions that need to be evaluated
}
}
\details{
This function allows the user to calculate the average DoS for multiple node pairs. The required input parameter is a list of interacting pairs and the search algorithm is applied to each pair. The average DoS for the set is returned.
}
\value{
A vector with the number of connections between each pair is returned. 
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
See Also \code{\link{import_network}}, \code{\link{find_path}}
}
\examples{
\dontrun{
network <- import_network("example_network_Cyt.txt")

candidate_gene_matrix <- as.data.frame(matrix(data = c("a","c","c","d","e","h","j","f","k","i","x","i"), ncol=2, byrow=T), stringsAsFactors = F)

multi_gene_separation <- multi_gene_paths(network,candidate_gene_matrix)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
