\name{degree_of_separation}
\alias{degree_of_separation}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate the average degree of separation for a network
}
\description{
The function calculates the average degree of separation by repeated sampling of two nodes finding the shortest path between them. 
}
\usage{
degree_of_separation(in_network, repeats = 100)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{in_network}{
The network used for analysis
}
  \item{repeats}{
Nummber of repeated sampling of two random node paths 
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A vector with the pathlength of each sampling event. 
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

DOS <- degree_of_separation(network, repeats=100)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
