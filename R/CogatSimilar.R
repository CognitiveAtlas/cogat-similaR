#' Cognitive Atlas Ontological Similarity
#'
#' Five methods proposed by Resnik, Schlicker, Jiang, Lin and Wang, originally applied for Gene Ontology (GoSemSim package) modified for the Cognitive Atlas to calculate ontological similarity of contrasts.
#' @param CAID1: the unique ID for the first term, should be a contrast, e.g., "cnt_4e00ccaa100fd"
#' @param CAID2: the unique ID for the second term, should be a contrast, e.g., "cnt_4e02624559a17"
#' @param method: "Wang" [default: Wang]
#' @keywords cognitive atlas, ontology, similarity
#' @return 
#' \item{score}{a similarity score for the two terms}
#' @export
#' @examples
#' 
#' library(CogatSimilar)
#' sim = CogatSimilar("cnt_4e00ccaa100fd","cnt_4e02624559a17",method="Wang")

CogatSimilar = function(CAID1, CAID2, method="Wang"){
  library(rjson)
  
  method = match.arg(method, c("Wang"))
	if (method == "Wang") {
		sim = wangsim(CAID1, CAID2)
        	sim = unname(sim, force=TRUE)
	        return(round(sim, digits=3))	
	} else {
                cat("Unsupported method\n")
        }
}
