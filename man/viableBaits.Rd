\name{viableBaits}
\alias{viableBaits}
\docType{data}
\title{A List of the viable baits in each bait to prey experiment}
\description{
	This named list consists of the viable baits which correspond
	to each experimental bait to prey experiment. 

	The names should be listed by the systematic gene names. If 
	the systematic gene names could not be found, then a common 
	name is used. If the data was obtained from IntAct, and neither
	a common nor a systematic name could be found, the IntAct 
	Accension Identification Code ("EBI-*****") is retained. The 
	IntAct Codes will always be listed first if they exist.

	Because we have chosen to supress the homodimer relationships
	within the respective bait to prey directed graphs, there will
	be proteins listed as viable baits which index isolated 
	nodes in the graphs. These isolated nodes simply mean that 
	these proteins were only seen to interact within homodimer
	relationships.
}
\usage{data(viableBaits)}
\format{
  The format is:
List of 12
 $ Ito2001BPGraph   : chr [1:1522] "EBI-21769" "EBI-30053" "EBI-33234" "EBI-34037" ...
 $ Cagney2001BPGraph: chr [1:19] "YBL041W" "YDL147W" "YDR394W" "YDR427W" ...
 $ Tong2002BPGraph  : chr [1:20] "YAR014C" "YBL007C" "YBL085W" "YBR200W" ...
 $ Hazbun2003BPGraph: chr [1:66] "YBR034C" "YBR102C" "YBR160W" "YBR217W" ...
 $ Zhao2005BPGraph  : chr "YPL240C"
 $ Uetz2000BPGraph1 : chr [1:508] "EBI-30925" "YAL032C" "YAL034W-A" "YAL036C" ...
 $ Uetz2000BPGraph2 : chr [1:139] "YAL040C" "YAL041W" "YAR020C" "YBL007C" ...
 $ Gavin2002BPGraph : chr [1:455] "YBR236C" "YNR016C" "YLR359W" "YMR300C" ...
 $ Ho2002BPGraph    : chr [1:493] "YAL015C" "YAL017W" "YAL021C" "YAL036C" ...
 $ Krogan2004BPGraph: chr [1:153] "YJL109C" "YGL173C" "YDR324C" "YGR128C" ...
 $ Gavin2006BPGraph : chr [1:1752] "EBI-13145" "EBI-11337" "EBI-19558" "YBR238C" ...
 $ Krogan2006BPGraph: chr [1:2264] "YAL001C" "YAL002W" "YAL003W" "YAL005C" ...
}

\examples{
data(viableBaits)
## maybe str(viableBaits) ; plot(viableBaits) ...
}
\keyword{datasets}
