\name{find_path}
\alias{find_path}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Find the shortest route between two nodes in a network
}
\description{
A search function that returns the shortest route between two nodes within a network.  
}
\usage{
find_path(in_network, node1, node2, quiet = F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{in_network}{
The network used for analysis
}
  \item{node1}{
Name of the first node of intrest (origin of search)
}
  \item{node2}{
Name of the second node of intrest (target of search)
}
  \item{quiet}{
Results retured to screen
}
}
\details{
One node is set as the origin-node and the other as the target-node. All the nodes interacting with the origin-node is placed in a vector, which is evaluated to determine if it contains the target-node. If the target-node is not in present in the evaluated vector, a new vector with all the nodes interacting with each of the nodes in the first vector is generated. This process is repeated. The number of times a new vector is created before the target-node is found equals the degree of separation between the two nodes of interest. The algorithm terminates once the target-node is found or when no new nodes are added in an iteration. The latter happens when the two nodes of interest are not connected, i.e. when the last vector contains all the nodes within the origin-node sub-network.
Once the shortest path is found, the subset of nodes included in the interaction search vector is traversed in the reverse direction, from target node to origin node, and pruned based on in which iteration the nodes were added and whether or not the interact with nodes in the previous tier in the reversed direction. The end result is a matrix containing only the nodes involved in the shortest path, or one such path if there are several with the same length.
}
\value{
A matrix with the shortest path between the two nodes of intrest is returned. 
}
\references{

}
\author{

}
\note{

}



\seealso{
See Also \code{\link{import_network}}
}
\examples{
\dontrun{
network <- import_network("example_network_Cyt.txt")

path_k_to_e <- find_path(network,"k","e")
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
