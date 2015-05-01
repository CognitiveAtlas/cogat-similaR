#' Cognitive Atlas Ontological Similarity
#'
#' Five methods proposed by Resnik, Schlicker, Jiang, Lin and Wang, originally applied for Gene Ontology (GoSemSim package) modified for the Cognitive Atlas to calculate ontological similarity of terms.
#' @param CAID1: the unique ID for the first term 
#' @param CAID2: the unique ID for the second term
#' @param method: "Resnik", "Jiang", "Lin", "Rel", "Wang" [default: Wang]
#' @keywords cognitive atlas, ontology, similarity
#' @return 
#' \item{score}{a similarity score for the two terms}
#' @export
#' @examples
#' 

`CogatSimilar` =
function(CAID1, CAID2, method="Wang"){
	method = match.arg(method, c("Resnik", "Jiang", "Lin", "Rel", "Wang"))
	if (method == "Wang") {
		sim = wangsim(CAID1, CAID2)
	} else {
		sim = ygcInfoContentMethod(CAID1, CAID2, method=method)
	}
	sim <- unname(sim, force=TRUE)
	return(round(sim, digits=3))	
}
