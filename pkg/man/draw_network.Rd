\name{draw_network}
\alias{draw_network}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Generate a ".gv" figure of the shortest path in a network 
}
\description{
Generate a ".gv" figure of the shortest path in a network. This file is can be visualized in software able to read the DOT format.
}
\usage{
draw_network(in_network, pathway, outfile = "pathway.gv", level = "zero")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{in_network}{
The network used for analysis
}
  \item{pathway}{
The pathway between two genes, usually generated by the \code{\link{find_path}} function
}
  \item{outfile}{
A character string naming the output file
}
  \item{level}{
Level "zero" and level "one" currently supported. The figure with level zero include only the nodes in the specified path. The figure with level one include all the nodes in the specified path and all the nodes that they in turn interact with.  
}
}
\details{
Generates a file containing a text description of the sub-network returned by the \code{\link{find_path}} function. The output file is in the DOT format, which can be visualised by a number of Graphviz compatible software packages.
}
\value{
Generates a ".gv" file in the working directory.
}
\references{

}
\author{

}
\note{

}



\seealso{
See Also \code{\link{find_path}}, \code{\link{import_network}}
}
\examples{
\dontrun{
network <- import_network("example_network_Cyt.txt")

path_k_to_e <- find_path(network,"k","e")

draw_network(network,path_k_to_e)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
