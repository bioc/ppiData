##This file will take the parsed IntAct Data and create the 
##graph structure that we want:

library(Rintact)

##you need to paste the path to the XML file in the following line
xml <- "../../../../../../../../downloads/all/psi25/species/yeast_small-03.xml"

gavin06 <- psi25interaction(xml)

gavin06 <- gavin06[[1]]

baits <- sapply(gavin06@interactions, function(x) x@bait)

##the following list will store each pull down separately
gavin06L <- vector("list", length = length(baits))

for(i in 1:length(baits)){
  gavin06L[[i]] <- gavin06@interactions[[i]]@prey
}

names(gavin06L) <- baits

fac <- vector(length=length(unlist(gavin06L)))

i <- 1
j <- 1

while(i < length(fac)){
  pullDownSize <- length(gavin06L[[j]])
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
gavin06List <- split(unlist(gavin06L), fac)
