\name{Krogan2006BPGraph}
\alias{Krogan2006BPGraph}
\docType{data}
\title{A directed Graph for the Bait to Prey Interaction
	data detected by Krogan et al. 2006.}

\usage{data(Krogan2006BPGraph)}
\format{
  The format is:
 chr "Krogan2006BPGraph"
}
\description{
	An instance of class graph, Krogan2006BPGraph is a graphNEL
	object. The nodes are the union of viable baits (VB) and 
	viable preys (VP) of the experiment conducted by Krogan et al. 
	2006. A viable bait is a node that has at least one directed 
	edge for which this node serves as the source. A viable prey 
	is a node that has at least one directed edge for which this 
	node serves as a sink.
	
	All nodes are indexed by the gene systematic names as given by the 
	primary source.

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
	Global landscape of protein complexes in the yeast Saccharomyces 
	cerevisiae. Nature 440, 637-643 (30 March 2006).
}
\examples{
data(Krogan2006BPGraph)
}

\keyword{datasets}