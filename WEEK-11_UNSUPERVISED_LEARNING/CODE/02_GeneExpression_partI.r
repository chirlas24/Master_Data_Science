library(tissuesGeneExpression)
library(ComplexHeatmap)
data(tissuesGeneExpression)
dim(e) ##e contains the expression data
table(tissue) ##tissue[i] tells us what tissue is represented by e[,i]

#Calculemos la distancia entre las muestras 1 y 2 (riñones) de la matriz de expresión 
#y a la muestra 87 (colon).

x <- e[,1]
y <- e[,2]
z <- e[,87]
sqrt(sum((x-y)^2))
sqrt(sum((x-z)^2))

sqrt( crossprod(x-y) )
sqrt( crossprod(x-z) )

d <- dist(t(e))
class(d)

as.matrix(d)[1,2]
as.matrix(d)[1,87]


Heatmap(t(scale(t(e[1:1000,]))),show_row_names = F,show_column_names = F)
