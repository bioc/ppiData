\name{ItoCore2001BPGraph}
\alias{ItoCore2001BPGraph}
\docType{data}
\title{A directed Graph for the Y2H Bait to Prey Interaction
	data detected by Ito et al. 2001.}

\usage{data(ItoCore2001BPGraph)}
\format{
  The format is:
  graphNEL "ItoCore2001BPGraph"
}
\description{
	An instance of class graph, ItoCore2001BPGraph is a graphNEL
	object. The nodes are the union of viable baits (VB) and 
	viable prey (VP) of the experiment conducted by Ito et al. 
	2001 which they annotated as the core dataset by having verified 
	the bait-prey interaction at least 3 times. A viable bait is a node 
	that has at least one directed edge for which this node serves as 
	the source. A viable prey is a node that has at least one directed 
	edge for which this node serves as a sink.
	
	The data from Ito et al. 2001 were obtained via the Intact
	repository. The Intact repository assigns an Intact specific
	code for each VB and each VP. Each Intact ID is mapped to 
	the a SGD ID (if available) and then mapped to the gene 
	systematic name via the org.Sc.sgdCOMMON2ORF environment
	of the org.Sc.sgd R-data package. If the mapping is one to many 
	at any point, the first entry of the mapping is selected.
	While this selection is arbitrary, there is no definitive 
	way to select from the mappings without more information. If
	more information concerning the experiment is given, the 
	non-arbitrary choice can be made.

	For example: x --> (a,b); a --> (c,d); b --> (e,f), 
	in our algorithm, x -------> c	

	If no proper mapping could be achieved in this manner, 
	the Intact ID's were kept so that other methods can be employed 
	for translation in the future. In all 7 VBs and 12 VPs could
	not be mapped to the gene systematic names, and so retain the
	Intact ID's.

	These graphs are not simple. While we chose not to present data
        with multiple edges between nodes (i.e. if bait b found prey p
        with multiplicity k, we do not assign k directed edges from b to
        p, only a single edge). We do, however, allow self loops to
        detail homodimer relationships.

	}
\source{
	The sparse matrix adjacency matrix for this graph can be found in the bioconductor
	R-package ppiStats.
}
\references{
	A comprehensive two-hybrid analysis to explore the yeast protein interactome.
	Proc Natl Acad Sci U S A. 2001 Apr 10;98(8):4569-74. Epub 2001 Mar 13.
}
\examples{
data(ItoCore2001BPGraph)
}

\keyword{datasets}
