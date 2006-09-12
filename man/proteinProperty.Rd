\name{proteinProperty}
\alias{proteinProperty}
\docType{data}
\title{A data frame which details 33 properties of the Yeast Genome}

\usage{data(proteinProperty)}
\format{
  The format is:
 data.frame "proteinProperty"
}
\description{
  This data frame is downloaded directly from SGD. It contains 33
  characteristics for 6714 open reading frames (ORFS). From the SGD
  README:

   Contains basic protein information about each ORF in SGD. This file
   does not include information on deleted or merged ORFs. Note, however,
   that it includes ORFs of all other classifications (Verified,
   Uncharacterized, and Dubious). This file is updated weekly (Saturday).

   The columns are below; only the first two columns are mandatory.  The
   column designated by an amino acid is the number of that particular
   residue in the protein sequence.  For example, if the ALA column is 2,
   then the protein contains 2 alanines.

  The columns of the dataframe are:

   FEATURE (ORF name)
   SGDID
   MOLECULAR WEIGHT (in Daltons)
   PI
   CAI (Codon Adaptation Index)
   PROTEIN LENGTH
   N TERM SEQ
   C TERM SEQ
   CODON BIAS
   ALA
   ARG
   ASN
   ASP
   CYS
   GLN
   GLU
   GLY
   HIS
   ILE
   LEU
   LYS
   MET
   PHE
   PRO
   SER
   THR
   TRP
   TYR
   VAL
   FOP SCORE (Frequency of Optimal Codons)
   GRAVY SCORE (Hydropathicity of Protein)
   AROMATICITY SCORE (Frequency of aromatic amino acids: Phe, Tyr, Trp)
   Feature type (ORF classification: Verified, Uncharacterized, Dubious)

   For more details, please visit the SGD website.
}
\source{
  ftp://genome-ftp.stanford.edu/pub/yeast/protein_info/protein_properties.tab
}
\references{

}
\examples{
data(proteinProperty)
}

\keyword{datasets}