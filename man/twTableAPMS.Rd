\name{twTableAPMS}
\alias{twTableAPMS}
\docType{data}
\title{A table to look at the permutation statistics of pairs of Y2H 
	 experiment.}
\description{
For each pair of APMS experiment, let X be the restriction of common
viable proteins. Restricted to the set X, entry n11 of the table details 
how many times the protein is found in both experiments; n12 details
how many times the protein was found in experiment 1 but not 
experiment 2; entry n21 details how many times the protein was found
in experiment 2 and not experiment 1; and n22 details the number of 
times the proteins were found in neither experiments.
}
\usage{data(twTableAPMS)}
\format{
  A list of matrices. Each element of the list contains a table for a 
  particular pair of experimental datasets. The rows are indexed by
  (n11,n12,n21,n22), and the columns are indexed by the elements
  of X.
}
\examples{
data(twTableAPMS)
}
\keyword{datasets}
