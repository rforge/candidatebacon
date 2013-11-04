\name{import_network}
\alias{import_network}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Import network data for CandidateBacon package
}
\description{
Function used it import network interaction data and extract relavent pairwise interactions. 
}
\usage{
import_network(infile, columns = c(1, 2))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{infile}{
file containing network data 
}
  \item{columns}{
columns in the file with pairwise interactions
}
}
\details{
The data is extracted from the indicated columns where each line indicates a pairwise interaction between two nodes. The data is formatted, and a matrix with the all interactions for each node is produced. Summary of the network features is provided. 
}
\value{
A matrix representing the network.
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
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
\dontrun{
network <- import_network("example_network_Cyt.txt")
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
