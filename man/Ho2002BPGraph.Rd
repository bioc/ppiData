\name{Ho2002BPGraph}
\alias{Ho2002BPGraph}
\docType{data}
\title{A directed Graph for the AP-MS Bait to Prey Interaction
	data detected by Ho et al. 2002.}

\usage{data(Ho2002BPGraph)}
\format{
  The format is:
  graphNEL "Ho2002BPGraph"
}
\description{
	An instance of class graph, Ho2002BPGraph is a graphNEL
	object. The nodes are the union of viable baits (VB) and 
	viable prey (VP) of the experiment conducted by Ho et al. 
	2002. A viable bait is a node that has at least one directed 
	edge for which this node serves as the source. A viable prey 
	is a node that has at least one directed edge for which this 
	node serves as a sink.
	
	All nodes are indexed by the gene systematic names as given 
	by the primary source.

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
	Systematic identification of protein complexes in Saccharomyces 
	cerevisiae by mass spectrometry. Nature. 2002 Jan 10;415(6868):180-3.
}
\examples{
data(Ho2002BPGraph)
}

\keyword{datasets}