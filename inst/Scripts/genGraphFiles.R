##Generating the graph objects

library(ppiData)
library(ppiStats)
library(YEAST)
yc2s <- as.list(YEASTCOMMON2SYSTEMATIC)
data(y2hSysGW)


bpY2HMats <- lapply(y2hSysGW, bpMatrix, homodimer=TRUE)
bpY2HGraphs <- lapply(bpY2HMats, genBPGraph)
names(bpY2HGraphs) <- bpExperimentNames[1:7]

apms <- list()
data(TAP)
data(HMSPCI)
data(Krogan)
data(gavinBP2006)
data(kroganBPMat2006)

rn <- toupper(rownames(TAP))
cn <- toupper(colnames(TAP))
rn1 <- vector(length=length(rn))
for(i in 1:nrow(TAP)){
    if(!is.null(yc2s[[rn[i]]])){
    rn1[i] <- yc2s[[rn[i]]][1]
    else rn1[i] <- rn[i]
}
cn1 <- vector(length = length(cn))
for(i in 1:ncol(TAP)){
    if(!is.null(yc2s[[cn[i]]])){
        cn1[i] <- yc2s[[cn[i]]][1]
    }
    else cn1[i] <- cn[i]
}

which(table(rn1) > 1)
which(table(cn1) > 1)
which(cn1 == "YAR042W")
TAP1 <- TAP
rownames(TAP1) <- rn1
colnames(TAP1) <- cn1
which(TAP1[,199] == 1)
which(TAP1[,1077] == 1)
TAP1 <- TAP1[,-1077]



apms$tap <- TAP1
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

