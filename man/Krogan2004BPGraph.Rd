\name{Krogan2004BPGraph}
\alias{Krogan2004BPGraph}
\docType{data}
\title{A directed Graph for the Bait to Prey Interaction
	data detected by Krogan et al. 2004.}

\usage{data(Krogan2004BPGraph)}
\format{
  The format is:
 chr "Krogan2004BPGraph"
}
\description{
	An instance of class graph, Krogan2004BPGraph is a graphNEL
	object. The nodes are the union of viable baits (VB) and 
	viable preys (VP) of the experiment conducted by Krogan et al. 
	2004. A viable bait is a node that has at least one directed 
	edge for which this node serves as the source. A viable prey 
	is a node that has at least one directed edge for which this 
	node serves as a sink.
	
	All nodes are indexed by the gene systematic names.

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