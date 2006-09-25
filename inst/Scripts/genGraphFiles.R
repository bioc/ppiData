##Generating the graph objects

library(ppiData)
library(ppiStats)

bpY2HMats <- lapply(y2hSysGW, bpMatrix, homodimer=TRUE)
bpY2HGraphs <- lapply(bpY2HMats, genBPGraph)
names(bpY2HGraphs) <- bpExperimentNames[1:7]

apms <- list()
data(TAP)
data(HMSPCI)
data(Krogan)
data(gavinBP2006)
data(kroganBPMat2006)

apms$tap <- TAP
apms$hmspci <- HMSPCI
apms$krogan <- Krogan
apms$gavin06 <- gavinBP2006
apms$krogan06 <- kroganBPMat2006

makeSym <- function(bpMat){
  proteins <- union(rownames(bpMat),colnames(bpMat))
  newMat <- matrix(0, nrow = length(proteins), ncol=length(proteins))
  dimnames(newMat) <- list(proteins, proteins)
  newMat[rownames(bpMat),colnames(bpMat)] <- bpMat
  newMat
}

apms <- lapply(apms, makeSym)

bpAPMSGraphs <- lapply(apms, genBPGraph)

