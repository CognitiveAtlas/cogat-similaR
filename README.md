### Cognitive Atlas Ontological Similarity

This package implements different ontological similarity metrics for the Cognitive Atlas to assess similarity of two contrast labels based on associated concepts.

UNDER DEVELOPMENT

### Installation

Make sure you have devtools installed

      library(devtools)
      devtools::install_github("CognitiveAtlas/cogat-similaR")

###[Wang](http://bioinformatics.oxfordjournals.org/content/23/10/1274.full)
This method aggregates the semantic contributions of ancestor terms (including this specific term).

#### Generating a list of concepts and weights for each contrast
 1. We start with concepts associated with the contrast
 2. We walk up the tree and append associated "is_a" and "part_of" concepts
 3. The weight for each concept is determined by multiplying the last (child node) weight by:
       0.8 for "is_a" relationships
       0.6 for "part_of" relationships
       This means that weights decrease as we move up the tree toward the root
 3. We stop at the root node

#### Calculating similarity between contrasts
 1. We take the weights at the intersection of each list from above
 2. The similarity score is sum(intersected weights) / sum(all weights)


#### References
This code base was adopted from the "GOSemSim" package, authored by Guangchuang Yu 

Yu et al. (2010) [GOSemSim: an R package for measuring semantic similarity among GO terms and gene products],(http://bioinformatics.oxfordjournals.org/cgi/content/abstract/26/7/976) Bioinformatics (Oxford, England), 26:7 976--978, April 2010. ISSN 1367-4803 PMID: 20179076
