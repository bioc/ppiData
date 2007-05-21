##This file will take the parsed IntAct Data and create the 
##graph structure that we want:
parsingIntAct <- function(xmlFile){
###library(Rintact)

##you need to paste the path to the XML file in the following line
xml <- xmlFile

  ##"../../../../../../../../downloads/all/psi25/species/yeast_small-03.xml"

xmlObject <- psi25interaction(xml)

xmlObject <- xmlObject[[1]]

baits <- sapply(xmlObject@interactions, function(x) x@bait)

##the following list will store each pull down separately
xmlObjectL <- vector("list", length = length(baits))

for(i in 1:length(baits)){
  xmlObjectL[[i]] <- xmlObject@interactions[[i]]@prey
}

names(xmlObjectL) <- baits

fac <- vector(length=length(unlist(xmlObjectL)))

i <- 1
j <- 1

while(i < length(fac)){
  pullDownSize <- length(xmlObjectL[[j]])
  if(length(i:(i+pullDownSize-1)) == pullDownSize){
    fac[i:(i+pullDownSize-1)] <-
    rep(baits[j], pullDownSize)}
  else{stop(paste("bug",j))}
  i <- pullDownSize+i
  j <- j+1
}

##the following list will combine pull downs of the same
##bait but will still keep multiplicity of prey..i.e. if the
##prey was found in 2 pulldowns, it will be recored twice 
xmlObjectList <- split(unlist(xmlObjectL), fac)
return(xmlObjectList)
}
