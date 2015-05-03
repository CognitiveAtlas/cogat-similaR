#' Cognitive Atlas Ontological Similarity
#'
#' Five methods proposed by Resnik, Schlicker, Jiang, Lin and Wang, originally applied for Gene Ontology (GoSemSim package) modified for the Cognitive Atlas to calculate ontological similarity of contrasts.
#' @param CAID1: the unique ID for the first term, should be a contrast, e.g., "cnt_4e00ccaa100fd"
#' @param CAID2: the unique ID for the second term, should be a contrast, e.g., "cnt_4e02624559a17"
#' @param owl: The ontology owl file (RDF) object read in with load.rdf function of rrdf package (provided in package example folder)
#' @param method: "Resnik", "Jiang", "Lin", "Rel", "Wang" [default: Wang]
#' @keywords cognitive atlas, ontology, similarity
#' @return 
#' \item{score}{a similarity score for the two terms}
#' @export
#' @examples
#' 
#' options(java.parameters = "-Xmx4g") # This ensures we don't run out of memory
#' library(CogatSimilar)
#' library(rrdf)
#' owlFile = "example/cogat.v2.owl"
#' cogat = load.rdf(owlFile)
#' sim = CogatSimilar("cnt_4e00ccaa100fd","cnt_4e02624559a17",method="Wang",owl=cogat)

CogatSimilar = function(CAID1, CAID2, owl, method="Wang"){
  library(rrdf)
  
  method = match.arg(method, c("Resnik", "Jiang", "Lin", "Rel", "Wang"))
	if (method == "Wang") {
		sim = wangsim(CAID1, CAID2, owl)
	} else {
		sim = InfoContentMethod(CAID1, CAID2, method=method)
	}
	sim = unname(sim, force=TRUE)
	return(round(sim, digits=3))	
}