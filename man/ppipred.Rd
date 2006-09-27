\name{ppipred}
\alias{ppipred}
\docType{data}
\title{ Protein-protein predictions}
\description{
The data in \code{ppipred} are those provided by Liu et al
for S. cervisiae protein protein interactions.
}
\usage{data(ppipred)}
\format{
  A data frame with 20088 observations on the following 3 variables.
  \describe{
    \item{\code{Gene1}}{A vector of the gene names for one interactor.}
    \item{\code{Gene2}}{A vector of the gene names for the other interactor.}
    \item{\code{Probability}}{A vectror containing the probability
      they give to the interaction.}
  }
}
\details{
 Liu et al discuss a method for estimating protein-protein interactions
 from existing data, across species. They have provided the data with the
following description:
\dQuote{We compute domain-domain interaction probabilities from Y2H 
protein-protein interactions, and then use these domain-domain interaction 
probabilities to compute the interaction probability between every pair 
of proteins. The prediction results with a false positive rate fp=3E-4 and 
a false negative rate fn=0.85 are listed blow.}

}
\source{
 \url{http://bioinformatics.med.yale.edu/interaction/}
}
\references{
\emph{Inferring protein-protein interactions through high-throughput 
interaction data from diverse organisms}, Y. Liu, N. Liu and H. Zhao,
Bioinformatics, 2005, 21, 3279-3285. 
}
\examples{
data(ppipred)
}
\keyword{datasets}
