\name{Tong2002BPGraph}
\alias{Tong2002BPGraph}
\docType{data}
\title{A directed Graph for the Y2H Bait to Prey Interaction
	data detected by Tong et al. 2002.}

\usage{data(Tong2002BPGraph)}
\format{
  The format is:
 chr "Tong2002BPGraph"
}
\description{
	An instance of class graph, Tong2002BPGraph is a graphNEL
	object. The nodes are the union of viable baits (VB) and 
	viable prey (VP) of the experiment conducted by Tong et al. 
	2002. A viable bait is a node that has at least one directed 
	edge for which this node serves as the source. A viable prey 
	is a node that has at least one directed edge for which this 
	node serves as a sink.
	
	The data from Tong et al. 2002 were obtained via the Intact
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
	for translation in the future. In all 0 VBs and 1 VPs could
	not be mapped to the gene systematic names, and so retain the
	Intact ID's.

	Lastly any self-interactions between identical proteins  
	(homodimer relationships) have been removed from the graph. 
	Therefore, any isolated vertex in this graph (if it exists)
	were proteins only to have been found to interact
	in homodimer relationships.

	}
\source{
	The sparse matrix adjacency matrix for this graph can be found in the bioconductor
	R-package ppiStats.
}
\references{
	A combined experimental and computational strategy to define protein 
	interaction networks for peptide recognition modules. 
	Science. 2002 Jan 11;295(5553):321-4. Epub 2001 Dec 13.

}
\examples{
data(Tong2002BPGraph)
}

\keyword{datasets}