\name{Krogan2004BPGraph}
\alias{Krogan2004BPGraph}
\docType{data}
\title{A directed Graph for the AP-MS Bait to Prey Interaction
	data detected by Krogan et al. 2004.}

\usage{data(Krogan2004BPGraph)}
\format{
  The format is:
 chr "Krogan2004BPGraph"
}
\description{
	An instance of class graph, Krogan2004BPGraph is a graphNEL
	object. The nodes are the union of viable baits (VB) and 
	viable prey (VP) of the experiment conducted by Krogan et al. 
	2004. A viable bait is a node that has at least one directed 
	edge for which this node serves as the source. A viable prey 
	is a node that has at least one directed edge for which this 
	node serves as a sink.
	
	All nodes are indexed by the gene systematic names as given
	in the primary source.

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
	 High-definition macromolecular composition of yeast RNA-processing 
	complexes.  Mol Cell. 2004 Jan 30;13(2):225-39. 
}
\examples{
data(Krogan2004BPGraph)
}

\keyword{datasets}