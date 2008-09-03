\name{Gavin2006BPGraph}
\alias{Gavin2006BPGraph}
\docType{data}
\title{A directed Graph for the AP-MS Bait to Prey Interaction
	data detected by Gavin et al. 2006.}

\usage{data(Gavin2006BPGraph)}
\format{
  The format is:
  graphNEL "Gavin2006BPGraph"
}
\description{
	An instance of class graph, Gavin2006BPGraph is a graphNEL
	object. The nodes are the union of viable baits (VB) and 
	viable prey (VP) of the experiment conducted by Gavin et al. 
	2006. A viable bait is a node that has at least one directed 
	edge for which this node serves as the source. A viable prey 
	is a node that has at least one directed edge for which this 
	node serves as a sink.

	The data from Gavin et al. 2006 were obtained via the Intact
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
	for translation in the future. In all 3 VBs  and 8 VPs could
	not be mapped to the gene systematic names.

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
	Proteome survey reveals modularity of the yeast cell machinery.
	Nature. 2006 Mar 30;440(7084):631-6. Epub 2006 Jan 22.
}
\examples{
data(Gavin2006BPGraph)
}

\keyword{datasets}