##This file will take the parsed IntAct Data and create the 
##graph structure that we want:
parsingIntAct <- function(xmlFiles, confidence=NULL){
###library(Rintact)
  if(class(xmlFiles)!="list"){
    stop("The input parameter must be a list.")}
  xmlList <- list()
  allBaits <- vector()
  
##you need to paste the path to the XML file in the following line
  for(i in 1:length(xmlFiles)){
    xml <- xmlFiles[[i]]
    
    ##"../../../../../../../../downloads/all/psi25/species/yeast_small-03.xml"
    
    xmlObject <- psi25interaction(xml)
    
    xmlObject <- xmlObject[[1]]

    if(!is.null(confidence)){
      ic <- sapply(xmlObject@interactions, function(x) x@confidenceValue) == confidence
      interaction <- xmlObject@interactions[ic]
    }

    ##the following list will store each pull down separately
    baits <- sapply(interaction, function(x) x@bait)
    xmlObjectL <- vector("list", length = length(baits))
    
    for(i in 1:length(baits)){
      xmlObjectL[[i]] <- interaction[[i]]@prey
    }

    names(xmlObjectL) <- baits
    
    xmlList <- c(xmlList, xmlObjectL)
    allBaits <- c(allBaits, baits)
    
  }
  print(length(xmlList))
  
  if(length(allBaits)!=length(unlist(xmlList))){
    fac <- vector(length=length(unlist(xmlList)))
    
    i <- 1
    j <- 1
    
    while(i <= length(fac)){
      pullDownSize <- length(xmlList[[j]])
      if(length(i:(i+pullDownSize-1)) == pullDownSize){
        fac[i:(i+pullDownSize-1)] <-
          rep(allBaits[j], pullDownSize)}
      else{stop(paste("bug",j))}
      i <- pullDownSize+i
      j <- j+1
    }
    
  }

  else{
    fac = allBaits
  }
  
  ##the following list will combine pull downs of the same
  ##bait but will still keep multiplicity of prey..i.e. if the
  ##prey was found in 2 pulldowns, it will be recored twice 
  xmlL <- split(unlist(xmlList), fac)
  return(xmlL)
}
