\name{Ito2001BPGraph}
\alias{Ito2001BPGraph}
\docType{data}
\title{A directed Graph for the Bait to Prey Interaction
	data detected by Ito et al. 2001.}

\usage{data(Ito2001BPGraph)}
\format{
  The format is:
 chr "Ito2001BPGraph"
}
\description{
	An instance of class graph, Ito2001BPGraph is a graphNEL
	object. The nodes are the union of viable baits (VB) and 
	viable preys (VP) of the experiment conducted by Ito et al. 
	2001. A viable bait is a node that has at least one directed 
	edge for which this node serves as the source. A viable prey 
	is a node that has at least one directed edge for which this 
	node serves as a sink.
	
	The data from Ito et al. 2001 were obtained via the Intact
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
	for translation in the future. In all 7 VBs and 27 VPs could
	not be mapped to the gene systematic names, and so retain the
	Intact ID's.

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
data(Ito2001BPGraph)
}

\keyword{datasets}