\name{Gavin2002BPGraph}
\alias{Gavin2002BPGraph}
\docType{data}
\title{A directed Graph for the AP-MS Bait to Prey Interaction
	data detected by Gavin et al. 2002.}

\usage{data(Gavin2002BPGraph)}
\format{
  The format is:
  graphNEL "Gavin2002BPGraph"
}
\description{
	An instance of class graph, Gavin2002BPGraph is a graphNEL
	object. The nodes are the union of viable baits (VB) and 
	viable prey (VP) of the experiment conducted by Gavin et al. 
	2002. A viable bait is a node that has at least one directed 
	edge for which this node serves as the source. A viable prey 
	is a node that has at least one directed edge for which this 
	node serves as a sink.

	One key fact is that Gavin et al. Used the protein common names
	when they recorded the data. As we have chosen to use the gene
	systematic names, we had to map from these common names to 
	systematic names. We used the  org.Sc.sgdCOMMON2ORF environment
	of the org.Sc.sgd R-data package to translate from common protein
	names to systematic gene names.

	We encountered two mappings which were not
	one to one. The VPs "Osh1" and "Swh1" both mapped to the 
	Open Reading Frame (ORF) "YAR042W". The VPs "Blm3" and the 
	alias "YFL006W" both mapped to the ORF "YFL007W". When mappings
	are not one to one, it is difficult to reproduce all the 
	information. 

	The VPs "Osh1" and "Swh1" were found by the same VB "Scs2" while 
	"Blm3" and the alias "YFL006W" were both detected by the VB "Scl1". 
	In creating this graphNEL, we deleted the VPs "Swh1" and "YFL006W".
	
	If, on the other hand, one protein mapped to several different ORFs
	we simple selected the first ORF in the list since there is no 
	definitive process to make the choice un-arbitrary. 
	
	For example: x --> (a,b), then x ---> a in our algorithm.

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
	Functional organization of the yeast proteome by systematic analysis 
	of protein complexes. Nature. 2002 Jan 10;415(6868):141-7
}
\examples{
data(Gavin2002BPGraph)
}

\keyword{datasets}