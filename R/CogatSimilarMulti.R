#' Cognitive Atlas Ontological Similarity Matrix
#'
#' Generate ontological similarity score for two lists of contrast IDs.
#' @param CALIST1: a list of unique IDs, should be contrasts, e.g., c("cnt_4e00ccaa100fd","cnt_4e00ccaa100fd")
#' @param CALIST2: a list of unique IDs, should be contrasts, e.g., c("cnt_4e00ccaa100fd","cnt_4e00ccaa100fd")
#' @param method: "Resnik", "Jiang", "Lin", "Rel", "Wang" [default: Wang]
#' @keywords cognitive atlas, ontology, similarity
#' @return 
#' \item{score}{a similarity score for the two lists}
#' @export
#' 

CogatSimilarMulti = function(CALIST1, CALIST2, method="Wang"){
	method = match.arg(method, c("Resnik", "Jiang", "Lin", "Rel", "Wang"))

	CA1 = unlist(CALIST1)
	CA2 = unlist(CALIST2)
	m = length(CA1)
	n = length(CA2)
	 
	scores = matrix(nrow=m, ncol=n)
	rownames(scores) = CA1
	colnames(scores) = CA2
	for(i in 1:m) {
		for (j in 1:n) {
			scores[i,j] = CogatSimilar(CA1[i], CA2[j], method=method)
		}
	}

	if (!sum(!is.na(scores))) return (NA)	
	if (n ==1 || m == 1) {
		return (max(scores))
	}
	
	sim = (sum(sapply(1:m, function(x) {max(scores[x,], na.rm=TRUE)})) + sum(sapply(1:n, function(x) {max(scores[,x], na.rm=TRUE)})))/(m+n)	
			
	return (round(sim,digits=3))

}