#' Cognitive Atlas Ontological Similarity Matrix
#'
#' Generate ontological similarity score for two lists of contrast IDs.
#' @param CALIST1: a list of unique IDs, should be contrasts, e.g., c("cnt_4e00ccaa100fd","cnt_4e00ccaa100fd")
#' @param CALIST2: a list of unique IDs, should be contrasts, e.g., c("cnt_4e00ccaa100fd","cnt_4e00ccaa100fd")
#' @param owl: The ontology owl file (RDF) object read in with load.rdf function of rrdf package (provided in package example folder)
#' @param method: "Resnik", "Jiang", "Lin", "Rel", "Wang" [default: Wang]
#' @keywords cognitive atlas, ontology, similarity
#' @return 
#' \item{score}{a similarity score for the two lists}
#' 

CogatSimilarMulti = function(CALIST1, CALIST2, owl, method="Wang"){

  library(rrdf)
  
  method = match.arg(method, c("Wang"))

	CA1 = unlist(CALIST1)
	CA2 = unlist(CALIST2)
	m = length(CA1)
	n = length(CA2)
	 
	scores = matrix(nrow=m, ncol=n)
	rownames(scores) = CA1
	colnames(scores) = CA2
	for(i in 1:m) {
		for (j in 1:n) {
			scores[i,j] = CogatSimilar(CA1[i], CA2[j], owl=owl, method=method)
		}
	}

	if (!sum(!is.na(scores))) return (NA)	
	if (n ==1 || m == 1) {
		return (max(scores))
	}
	
	sim = (sum(sapply(1:m, function(x) {max(scores[x,], na.rm=TRUE)})) + sum(sapply(1:n, function(x) {max(scores[,x], na.rm=TRUE)})))/(m+n)	
			
	return (round(sim,digits=3))

}
