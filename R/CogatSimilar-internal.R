getConcepts = function(CAID1,CAID2){

  Parents = list()
  
  # Climb tree and get all concepts
  Parents[[CAID1]] = getParents(CAID1)
  Parents[[CAID2]] = getParents(CAID2)
  return(Parents)
}

# Function to get concept parent tree
getParents = function(CAID){

  cat("\nLooking up initial concepts associated with contrast",CAID)
  
  # First get associated concept IDS
  concepts = getAssociatedConcepts(CAID)
  
  if (is.na(concepts)){
    return(NA)
  }
  
  # Now walk up tree and retrieve "is_a" and "part_of" relations for each base
  TREE = list()
  for (base in concepts$uid){
    TREE[[base]] = walkUpTree(base)
  }
  return(TREE)
}

# Get concepts associated with a contrast
getAssociatedConcepts = function(CAID){

  query = readLines(paste('http://cognitiveatlas.org/api/v-alpha/concept?contrast_id=',CAID,sep=""),warn=FALSE);
  concepts = fromJSON(query)  
  
  # Cognitive Atlas Bug
  if (concepts[1]==""){
    cat("\nProbem with Cognitive Atlas returning empty result for",CAID,"\n")
    return(NA)
  }
  
  result = c()
  if (length(concepts)!=0){  
    for (c in 1:length(concepts)){
      if (length(concepts[[c]]) > 1) {
        res = cbind(concepts[[c]]$name,concepts[[c]]$id)
        result = rbind(result,res)
      }
    }
    result = as.data.frame(result,stringsAsFactors=FALSE)
    colnames(result) = c("term","uid")
    cat("CONCEPTS:",as.character(result$term),sep="\n")
    
    # Define that we are at starting point
    result$relation = "base"
    return(result)
  } else {
    return(NA)
  }
}


# Get related "is_a" and "part_of" concepts
getRelatedConcepts = function(CONID) {
  hasParents = FALSE
  hasPartOf = FALSE
  
  # First get the parent
  query = readLines(paste("http://cognitiveatlas.org/api/v-alpha/concept?id=",CONID,sep=""),warn=FALSE);
  
  if (length(query)>0){
    relationships = fromJSON(query)[[1]]
    hasParents=TRUE
    
    if ("relationships" %in% names(relationships)) {
      # Find the parents
      parents = c()
      for (r in 1:length(relationships$relationships)){
        relation = relationships$relationships[[r]]
        if (relation$direction=="parent"){
          res = relation$id
          names(res) = relation$relationship
          parents = c(parents,res)
        }
      }
      return(parents)
    } else{
    return(NA)}
  }
  else {
    return(NA)
  }
}

# Starting at base concept, walk up tree to get related concepts
walkUpTree = function(base){
  queue = base
  seen = c()
  concepts = c()
  while (length(queue) > 0){
    current = queue[1]
    queue = queue[-1]
    # This is the base of the ontology
    if (!(current %in% seen)) {
      tmp = getRelatedConcepts(current)
        if (!is.null(tmp)){
          if (!is.na(tmp[1])){
            cat(paste(names(tmp),tmp),sep="\n")
            concepts = c(concepts,tmp)
            queue = c(queue,tmp)
            seen = c(seen,current)
          }
        }
     }
  }
  root = "CAO_00001"
    
  names(root) = "kind_of"
  names(base) = "kind_of"
  concepts = c(base,concepts,root)
  return(concepts)
}

wangsim = function(CAID1, CAID2) {
	weight.kindof = 0.8 # akin to "is_a"
	weight.partof = 0.6

	if (CAID1 == CAID2){
		return (sim=1)		
  }
  
  # First retrieve a tree of concepts linked to the contrast  
	Concepts = getConcepts(CAID1,CAID2)
	
	sv.a = 1
	sv.b = 1
	sw = 1
	names(sv.a) = CAID1
	names(sv.b) = CAID2 
	
  weightlists.a = getweightlists(CAID1, Concepts, sv.a, sw, weight.kindof, weight.partof)
	weightlists.b = getweightlists(CAID2, Concepts, sv.b, sw, weight.kindof, weight.partof)
  
  # If a node is hit twice in the graph (from separate parents) we use the highest weight
	sv.a = uniqsv(weightlists.a)
	sv.b = uniqsv(weightlists.b)
	
	idx = intersect(names(sv.a), names(sv.b))
	inter.sva = unlist(sv.a[idx])
	inter.svb = unlist(sv.b[idx])
	sim = sum(inter.sva,inter.svb) / sum(length(sv.a), length(sv.b))
	return(sim)
}

# If a node is hit twice in the graph (from separate terms), we use the highest weight
uniqsv = function(sv) {
	sv = unlist(sv)
	una = unique(names(sv))
	sv = unlist(sapply(una, function(x) {max(sv[names(sv)==x])}))
	return (sv)
}

# Return list of weights assigned to each part_of, is_a relationship
getweightlists = function(CAID, Parents, startValue, startWeight, weight.kindof, weight.partof) {
  # In the case of more than one parent, we will calculate similarity for both
  
  weightLists = c()
  noparents=TRUE
  startWeightHolder = startWeight
  
  for (pp in 1:length(Parents[[CAID]])) {
    p = unlist(Parents[[CAID]][[pp]])
    plabel = names(Parents[[CAID]][pp])
    
  	# Ensure the root node is at the end
  	p = sort(p,decreasing=TRUE)
    if (length(p) != 0) {
    	noparents = FALSE
      relations = names(p)
      startWeight = startWeightHolder
    	old.w = startWeight
    	for (i in 1:length(p)) {
    		if (grepl("kind of",relations[i])) {
    			startWeight = old.w * weight.kindof
    		} else {
    			startWeight = old.w * weight.partof
    		}
    		names(startWeight) = p[i]
    		startValue = c(startValue,startWeight)
        old.w = startWeight
    	}
    	weightLists = c(weightLists,startValue)
    }
  }
  # If no parents, then return root
  if (noparents==TRUE){
    weightLists = startValue
  }
return(weightLists)
}
