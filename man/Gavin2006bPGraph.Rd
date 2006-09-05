\name{Gavin2006BPGraph}
\alias{Gavin2006BPGraph}
\docType{data}
\title{A directed Graph for the AP-MS Bait to Prey Interaction
	data detected by Gavin et al. 2006.}

\usage{data(Gavin2006BPGraph)}
\format{
  The format is:
 chr "Gavin2006BPGraph"
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
	systematic name via the YEASTCOMMON2SYSTEMATIC environment
	of the YEAST R-data package. If the mapping is one to many 
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
	Proteome survey reveals modularity of the yeast cell machinery.
	Nature. 2006 Mar 30;440(7084):631-6. Epub 2006 Jan 22.
}
\examples{
data(Gavin2006BPGraph)
}

\keyword{datasets}