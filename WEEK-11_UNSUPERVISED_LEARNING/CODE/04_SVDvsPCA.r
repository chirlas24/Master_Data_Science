install_github("dagdata","genomicsclass")
data(tissuesGeneExpression)
library(rafalib)
group <- as.fumeric(tab$Tissue)

#pca
x <- t(e)
pc <- prcomp(x)
# ?prcomp
names(pc)
plot(pc$x[,1], pc$x[,2], col=group, main="PCA", xlab="PC1", ylab="PC2")

#PCA es equivalente a calcular SVD en los datos centrados por columnas 
#(una vez traspuesta nuetra matriz x tiene los genes en las columnas) 
#Podemos usar la función _sweep_ para trabajar con filas y columnas eficientemente. 

cx <- sweep(x, 2, colMeans(x), "-")
sv <- svd(cx)
names(sv)
plot(sv$u[,1], sv$u[,2], col=group, main="SVD", xlab="U1", ylab="U2")

sv$v[1:5,1:5]
pc$rotation[1:5,1:5]

head(sv$d^2)
head(pc$sdev^2)
head(sv$d^2 / (ncol(e) - 1))

#Si dividimos la varianza por la suma obtenemos un plot del radio de 
#varianza explicada por cada componente principal:

plot(sv$d^2 / sum(sv$d^2), xlim=c(0,15), type="b", pch=16,
     xlab="principal components", 
     ylab="variance explained")
plot(sv$d^2 / sum(sv$d^2), type="b", pch=16,
     xlab="principal components", 
     ylab="variance explained")

#Si no hubiéramos centrado los datos antes de hacer `svd` el plot habría sido algo distinto:
  
svNoCenter <- svd(x)
plot(pc$x[,1], pc$x[,2], col=group, main="PCA", xlab="PC1", ylab="PC2")
points(0,0,pch=3,cex=4,lwd=4)
plot(svNoCenter$u[,1], svNoCenter$u[,2], col=group, main="SVD not centered", xlab="U1", ylab="U2")

#usado sobre las filas nos ayuda a detectar sesgos
sv2 <- svd(t(e))
plot(sv2$u[,1], sv2$u[,2], col=group, main="samples vs genes (typical PCA)", xlab="U1", ylab="U2")
sv1 <- svd(e)
plot(sv1$v[,1], sv1$v[,2], col=group, main="genes vs samples (SVA)", xlab="V1", ylab="V2")
