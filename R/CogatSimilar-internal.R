getConcepts = function(CAID1,CAID2,owlFile){

  Parents = list()
  
  # Climb tree and get all concepts
  Parents[[CAID1]] = getParents(CAID1,owlFile)
  Parents[[CAID2]] = getParents(CAID2,owlFile)
  return(Parents)
}


# Get concepts associated with a contrast
getAssociatedContrasts = function(CAID,cogat){
  
  query = paste('
  PREFIX dc: <http://purl.org/dc/terms/>
  PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
  PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
  PREFIX owl: <http://www.w3.org/2002/07/owl#>
  PREFIX cogat: <http://www.cognitiveatlas.org/id/>

  SELECT DISTINCT ?term_uri ?task ?relation
  WHERE {
    ?term_uri dc:identifier "',CAID,'" .
    ?term_uri rdfs:subClassOf ?subclass .
    ?subclass owl:someValuesFrom ?task .
    ?subclass owl:onProperty ?relation .
  }',sep="");
  
  result = sparql.rdf(cogat,query)
  concepts = result[grep("#measures",result[,3]),2]
  term_uri = result[1,1]
  
  cat("\nTERM URI:",term_uri,"CONCEPTS:",unlist(lapply(concepts,getBaseURI)),sep="\n")
  
  # The names label == parent
  names(concepts) = rep("base",length(concepts))
  return(concepts)
}

# Function to get concept parent tree
getParents = function(CAID,cogat){

  cat("\nLooking up initial concepts associated with contrast",CAID)
  
  # First get associated concept IDS
  concepts = getAssociatedContrasts(CAID,cogat)
  
  # Now walk up tree and retrieve "is_a" and "part_of" relations for each base
  TREE = list()
  for (base in concepts){
    TREE[[getBaseURI(base)]] = walkUpTree(base,cogat)
  }
  return(TREE)
}

# Get related "is_a" and "part_of" concepts
getRelatedConcepts = function(CONID,cogat) {
  hasParents = FALSE
  hasPartOf = FALSE
  
  # First get the parent
  # Here is the parent <rdfs:subClassOf rdf:resource="&cogat;CAO_00525"/> (is_a)
  query = paste('
  PREFIX dc: <http://purl.org/dc/terms/>
  PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
  PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
  PREFIX owl: <http://www.w3.org/2002/07/owl#>
  PREFIX cogat: <http://www.cognitiveatlas.org/id/>

  SELECT DISTINCT ?subclass
  WHERE {<',
     CONID,'> dc:identifier ?cnt_uri .
     <',CONID, '> rdfs:subClassOf ?subclass .
  }',sep="");
  
  result = sparql.rdf(cogat,query)
  if (length(result)>0){
    result = result[grep("#",result)]    
    # Remove the conid from the list
    parents = result[-grep(CONID,result)]
    parents = unlist(lapply(parents,getBaseURI))
    names(parents) = rep("is_a",length(parents))
    hasParents = TRUE
  } 
  
  # Next get associated "part of" concept IDS
  query = paste('
  PREFIX dc: <http://purl.org/dc/terms/>
  PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
  PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
  PREFIX owl: <http://www.w3.org/2002/07/owl#>
  PREFIX cogat: <http://www.cognitiveatlas.org/id/>

  SELECT DISTINCT ?task ?relation
  WHERE {<',
     CONID,'> dc:identifier ?cnt_uri .
     <',CONID, '> rdfs:subClassOf ?subclass .
     ?subclass owl:someValuesFrom ?task .
     ?subclass owl:onProperty ?relation .
  }',sep="");
  
  result = sparql.rdf(cogat,query)
  if (length(result)>0){
    partof = result[grep("#part_of",result[,2]),1]
    if (length(partof) > 0){
      partof = unlist(lapply(partof,getBaseURI))      
      names(partof) = rep("part_of",length(partof))
      hasPartOf = TRUE    
    }
  } 
  if (hasParents && hasPartOf){
    return(c(parents,partof))    
    } else if (hasParents) {
    return(parents)
    } else if (hasPartOf){
    return(partof)
    } else {
    return(NA)
    }
}

getBaseURI = function(uri){
  return(strsplit(uri,"#")[[1]][2])
}

# Starting at base concept, walk up tree to get related concepts
walkUpTree = function(base,cogat){
  queue = base
  concepts = c()
  while (length(queue) > 0){
    current = queue[1]
    queue = queue[-1]
    # This is the base of the ontology
    if (current!="http://www.cognitiveatlas.org/ontology/cogat.owl#CAO_00001"){
      tmp = getRelatedConcepts(current,cogat)
      if (!is.na(tmp[1])){
        cat(paste(names(tmp),tmp),sep="\n")
        concepts = c(concepts,tmp)
        queue = c(queue,tmp)
      }
    }
  }
  root = "CAO_00001"
  names(root) = "is_a"
  base = getBaseURI(base)
  names(base) = "is_a"
  concepts = c(base,concepts,root)
  return(concepts)
}

wangsim = function(CAID1, CAID2, owlFile) {
	weight.isa = 0.8
	weight.partof = 0.6

	if (CAID1 == CAID2){
		return (sim=1)		
  }
  
  # First retrieve a tree of concepts linked to the contrast  
	Concepts = getConcepts(CAID1,CAID2,owlFile)
	
	sv.a = 1
	sv.b = 1
	sw = 1
	names(sv.a) = CAID1
	names(sv.b) = CAID2 
	
	sv.a = uniqsv(SemVal(CAID1, Concepts, sv.a, sw, weight.isa, weight.partof))
	sv.b = uniqsv(SemVal(CAID2, Concepts, sv.b, sw, weight.isa, weight.partof))
	
	idx = intersect(names(sv.a), names(sv.b))
	inter.sva = unlist(sv.a[idx])
	inter.svb = unlist(sv.b[idx])
	sim = sum(inter.sva,inter.svb) / sum(sv.a, sv.b)
	return(sim)
}

# If a node is hit twice in the graph (from separate terms), we use the highest weight
uniqsv = function(sv) {
	sv = unlist(sv)
	una = unique(names(sv))
	sv = unlist(sapply(una, function(x) {max(sv[names(sv)==x])}))
	return (sv)
}

# Return list of weights assigned to each part_of, is_a relationshi
SemVal = function(CAID, Parents, startValue, startWeight, weight.isa, weight.partof) {
	p = unlist(Parents[[CAID]])
  
	# Ensure the root node is at the end
	p = sort(p,decreasing=TRUE)
  if (length(p) == 0) {
		cat("WARNING:",CAID, "does not have related concept parents defined in Cognitive Atlas\n")
		return(startValue)
	}
	relations = names(p)
	old.w = startWeight
	for (i in 1:length(p)) {
		if (grepl("is_a",relations[i])) {
			startWeight = old.w * weight.isa
		} else {
			startWeight = old.w * weight.partof
		}
		names(startWeight) = p[i]
		startValue = c(startValue,startWeight)
    old.w = startWeight
	}
	return (startValue)
}

InfoContentMethod = function(CAID1, CAID2, method) {
  
  # For now we are loading dummy data 
  load("/home/vanessa/Documents/Dropbox/Code/R/PACKAGES/CogatSimilar/data/Info_Contents_dummy.rda")
	Info.contents = IC
  
	rootCount = max(Info.contents[Info.contents != Inf])
	Info.contents["CAO_00001"] = 0
  
  # Get the concepts assigned to the contrast
  concepts.caid1 = getAssociatedContrasts(CAID1)
  concepts.caid2 = getAssociatedContrasts(CAID2)
  
  # Calculate an average probability
  p1 = Info.contents[which(names(Info.contents) %in% unlist(lapply(concepts.caid1,getBaseURI)))]
  p2 = Info.contents[which(names(Info.contents) %in% unlist(lapply(concepts.caid2,getBaseURI)))]
  
  p1 = mean(p1[!is.infinite(p1)])
  p2 = mean(p2[!is.infinite(p2)])
  
  p1 = p1/rootCount
	p2 = p2/rootCount    

	if (p1 == 0 || p2 == 0) return (NA)
	
  ancestor.caid1 = sort(unique(unlist(getParents(CAID1))),decreasing=TRUE)
  ancestor.caid2 = sort(unique(unlist(getParents(CAID2))),decreasing=TRUE)
	
  # If we are comparing a term to itself, the common ancestors are the shared concepts
  if (CAID1 == CAID2) {
		commonAncestor = ancestor.caid1
	} else { 
		commonAncestor = intersect(ancestor.caid1, ancestor.caid2)
	}

  commonAncestor = commonAncestor[!is.infinite(commonAncestor)]
  if (length(commonAncestor) == 0) return (NA)
  
  commonAncestor = Info.contents[commonAncestor]
  commonAncestor = commonAncestor[!is.infinite(commonAncestor)]
	pms = max(commonAncestor)/rootCount
	sim = switch(method,
   	    Resnik = pms,
   	    Lin = pms/(p1+p2),
   	    Jiang = 1 - min(1, -2*pms + p1 + p2), 
   	    Rel = 2*pms/(p1+p2)*(1-exp(-pms*rootCount))
	)   	
	return (sim)
}