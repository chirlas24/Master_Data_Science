install.packages(c("ComplexHeatmap",
                   "cluster",
                   "devtools",
                   "tissuesGeneExpression",
                   "MASS",
                   "rafalib",
                   "tidyverse",
                   "matlib",
                   "dslabs",
                   "RColorBrewer",
                   "dplyr","genefilter"))
source("http://bioconductor.org/biocLite.R")
biocLite("genefilter")
biocLite("ComplexHeatmap")

library(ComplexHeatmap)
library(cluster)
library(devtools)
install_github("genomicsclass/tissuesGeneExpression")
library(tissuesGeneExpression)


