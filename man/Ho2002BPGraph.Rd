\name{Ho2002BPGraph}
\alias{Ho2002BPGraph}
\docType{data}
\title{A directed Graph for the Bait to Prey Interaction
	data detected by Ho et al. 2002.}

\usage{data(Ho2002BPGraph)}
\format{
  The format is:
 chr "Ho2002BPGraph"
}
\description{
	An instance of class graph, Ho2002BPGraph is a graphNEL
	object. The nodes are the union of viable baits (VB) and 
	viable preys (VP) of the experiment conducted by Ho et al. 
	2002. A viable bait is a node that has at least one directed 
	edge for which this node serves as the source. A viable prey 
	is a node that has at least one directed edge for which this 
	node serves as a sink.
	
	All nodes are indexed by the gene systematic names as given 
	by the primary source.

	Lastly any self-interactions between identical proteins  
	(homodimer relationships) have been removed from the graph. 
	Therefore, any isolated vertex in this graph (if it exists)
	were proteins only to have been found to interact
	in homodimer relationships.

	}
\source{
	The adjacency matrix for this graph can be found in the bioconductor
	R-package apComplex.
}
\references{
	Systematic identification of protein complexes in Saccharomyces 
	cerevisiae by mass spectrometry. Nature. 2002 Jan 10;415(6868):180-3.
}
\examples{
data(Ho2002BPGraph)
}

\keyword{datasets}