options(java.parameters = "-Xmx4g") # This ensures we don't run out of memory
library(CogatSimilar)
library(pheatmap)
library(rrdf)

# For now we give function the rdf object already loaded
owlFile = "example/cogat.v2.owl"
cogat = load.rdf(owlFile)

# This is a table of contrast IDS and names
contrasts = read.csv("example/contrast_ids.tsv",sep="\t",head=TRUE,stringsAsFactors=FALSE)

# This contrasts is not in ontology
contrasts = contrasts[-(which(contrasts$CONTRAST_ID=="cnt_4dffcba5c2efd")),]

simMatrix = CogatSimilarMatrix(contrasts$CONTRAST_ID, owl = cogat, method="Wang")
# Only do the below when you want to zoom in on a region and add task names
rownames(simMatrix) = paste(contrasts$TASK_ID,contrasts$CONTRAST_NAME,sep=" : ")
colnames(simMatrix) = paste(contrasts$TASK_ID,contrasts$CONTRAST_NAME,sep=" : ")

# Make a heatmap, first include all empty
hm = pheatmap(simMatrix)
simSorted = simMatrix[hm$tree_row$order,hm$tree_col$order]

# Get rid of empty columns and rows
simSorted = simSorted[,-which((colSums(simSorted)-1)==0)]
simSorted = simSorted[-which((rowSums(simSorted))==0),]
pheatmap(simSorted)

# Zoom in on a region (cols and rows are equal, so hide column names)
hm = pheatmap(simSorted[1:50,1:50],show_colnames=F)
