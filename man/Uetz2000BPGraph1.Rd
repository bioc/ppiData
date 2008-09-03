\name{Uetz2000BPGraph1}
\alias{Uetz2000BPGraph1}
\docType{data}
\title{A directed Graph for the Y2H Bait to Prey Interaction
	data detected by Uetz et al. 2000.}

\usage{data(Uetz2000BPGraph1)}
\format{
  The format is:
  graphNEL "Uetz2000BPGraph1"
}
\description{
	An instance of class graph, Uetz2000BPGraph is a graphNEL
	object. The nodes are the union of viable baits (VB) and 
	viable prey (VP) of the experiment conducted by Uetz et al. 
	2000. A viable bait is a node that has at least one directed 
	edge for which this node serves as the source. A viable prey 
	is a node that has at least one directed edge for which this 
	node serves as a sink.
	
	The data from Uetz et al. 2000 were obtained via the Intact
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
	for translation in the future. In all 1 VBs and 2 VPs could
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
	A comprehensive analysis of protein-protein interactions in Saccharomyces cerevisiae.
	Nature. 2000 Feb 10;403(6770):623-7.

}
\examples{
data(Uetz2000BPGraph1)
}

\keyword{datasets}