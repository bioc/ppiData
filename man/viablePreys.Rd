\name{viablePreys}
\alias{viablePreys}
\docType{data}
\title{A List of the viable preys in each bait to prey experiment}
\description{
	This named list consists of the viable preys which correspond
	to each experimental bait to prey experiment. 

	The names should be listed by the systematic gene names. If 
	the systematic gene names could not be found, then a common 
	name is used. If the data was obtained from IntAct, and neither
	a common nor a systematic name could be found, the IntAct 
	Accension Identification Code ("EBI-*****") is retained. The 
	IntAct Codes will always be listed first if they exist.

	Because we have chosen to supress the homodimer relationships
	within the respective bait to prey directed graphs, there will
	be proteins listed as viable preys which index isolated 
	nodes in the graphs. These isolated nodes simply mean that 
	these proteins were only seen to interact within homodimer
	relationships.
}
\usage{data(viablePreys)}
\format{
  The format is:
List of 12
 $ Ito2001   : chr [1:2493] "EBI-34960" "EBI-38535" "EBI-38876" "EBI-36299" ...
 $ Cagney2001: chr [1:40] "YER105C" "YDL020C" "YCR073C" "YIL007C" ...
 $ Tong2002  : chr [1:59] "EBI-33845" "YER054C" "YIL045W" "YDR195W" ...
 $ Hazbun2003: chr [1:1940] "EBI-5195" "EBI-31597" "EBI-31017" "EBI-852932" ...
 $ Zhao2005  : chr [1:90] "YAR015W" "YAR019C" "YBL103C" "YBR089W" ...
 $ Uetz2000-1: chr [1:630] "EBI-37713" "EBI-10698" "YBL043W" "YNL229C" ...
 $ Uetz2000-2: chr [1:400] "EBI-34060" "EBI-392763" "YBR160W" "YJR132W" ...
 $ Gavin2002 : chr [1:1362] "YBR236C" "YNR016C" "YLR359W" "YMR300C" ...
 $ Ho2002    : chr [1:1578] "YAL015C" "YAL017W" "YAL021C" "YAL036C" ...
 $ Krogan2004: chr [1:485] "YJL109C" "YGL173C" "YDR324C" "YGR128C" ...
 $ Gavin2006 : chr [1:2551] "EBI-13145" "EBI-11337" "EBI-19558" "EBI-21748" ...
 $ Krogan2006: chr [1:5361] "YAL001C" "YAL002W" "YAL003W" "YAL005C" ...
}

data(viablePreys)
}
\keyword{datasets}
