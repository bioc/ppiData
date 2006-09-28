\name{Krogan2006BPGraph}
\alias{Krogan2006BPGraph}
\docType{data}
\title{A directed Graph for the AP-MS Bait to Prey Interaction
	data detected by Krogan et al. 2006.}

\usage{data(Krogan2006BPGraph)}
\format{
  The format is:
  graphNEL "Krogan2006BPGraph"
}
\description{
	An instance of class graph, Krogan2006BPGraph is a graphNEL
	object. The nodes are the union of viable baits (VB) and 
	viable prey (VP) of the experiment conducted by Krogan et al. 
	2006. A viable bait is a node that has at least one directed 
	edge for which this node serves as the source. A viable prey 
	is a node that has at least one directed edge for which this 
	node serves as a sink.
	
	All nodes are indexed by the gene systematic names as given by the 
	primary source.

	These graphs are not simple. While we chose not to present data
        with multiple edges between nodes (i.e. if bait b found prey p
        with multiplicity k, we do not assign k directed edges from b to
        p, only a single edge). We do, however, allow self loops to
        detail homodimer relationships.

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