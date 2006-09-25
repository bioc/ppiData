\name{viablePrey}
\alias{viablePrey}
\alias{viableBaits}
\docType{data}
\title{Lists of viable prey or baits in each bait to prey dataset}
\description{
These are named lists consisting of either the viable prey or the viable
baits corresponding to each dataset. 

	The names should be listed by the systematic gene names. If 
	the systematic gene names could not be found, then a common 
	name is used. If the data was obtained from IntAct, and neither
	a common nor a systematic name could be found, the IntAct 
	Accension Identification Code ("EBI-*****") is retained. The 
	IntAct Codes will always be listed first if they exist.

	Because we have chosen to supress the homodimer relationships
	within the respective bait to bait or viable prey directed
        graphs, there will be proteins listed as viable prey which index
        isolated nodes in
        the graphs. These isolated nodes simply mean that 
	these proteins were only seen to interact within homodimer
	relationships.
}
\usage{
data(viableBaits)
data(viablePrey)
}
\format{A named list. Each element is a character vector of the viable
  prey (or baits) for the corresponding dataset.
}

\examples{
data(viablePrey)
data(viableBaits)
names(viablePrey)
names(viableBaits)
viableBaits[[1]][1:5]
}
\keyword{datasets}
