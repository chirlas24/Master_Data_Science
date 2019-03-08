library(rafalib)
library(MASS)
library(tidyverse)
library(matlib) #No se cargar, pero es para una cosa muy puntual. NO Problemo

#Generamos los datos de alturas de gemelos
set.seed(1)
n <- 100
y=t(mvrnorm(n,c(0,0), matrix(c(1,0.95,0.95,1),2,2)))
mypar()
plot(y[1,], y[2,], xlab="Twin 1 (standardized height)", 
     ylab="Twin 2 (standardized height)", xlim=c(-3,3), ylim=c(-3,3))
points(y[1,1:2], y[2,1:2], col=2, pch=16)

#calculamos sus distancias
d=dist(t(y))
as.matrix(d)[1,2]

#proponemos una transformacion plausible
z1 = (y[1,]+y[2,])/2 #the sum 
z2 = (y[1,]-y[2,])   #the difference
z = rbind( z1, z2) #matrix now same dimensions as y
thelim <- c(-3,3)
mypar(1,2)
plot(y[1,],y[2,],xlab="Twin 1 (standardized height)",
     ylab="Twin 2 (standardized height)",
     xlim=thelim,ylim=thelim)
points(y[1,1:2],y[2,1:2],col=2,pch=16)
plot(z[1,],z[2,],xlim=thelim,ylim=thelim,xlab="Average height",ylab="Difference in height")
points(z[1,1:2],z[2,1:2],col=2,pch=16)

#que resulta ser una rotacion
A <- 1/sqrt(2)*matrix(c(1,1,1,-1),2,2)
z <- A%*%y
d <- dist(t(y))
d2 <- dist(t(z))
mypar(1,1)
plot(as.numeric(d),as.numeric(d2)) #as.numeric turns distances into long vector
abline(0,1,col=2)

#comprobamos que es lo mismo
mypar(1,2)
thelim <- c(-3,3)
plot(y[1,],y[2,],xlab="Twin 1 (standardized height)",
     ylab="Twin 2 (standardized height)",
     xlim=thelim,ylim=thelim)
points(y[1,1:2],y[2,1:2],col=2,pch=16)
plot(z[1,],z[2,],xlim=thelim,ylim=thelim,xlab="Average height",ylab="Difference in height")
points(z[1,1:2],z[2,1:2],col=2,pch=16)

#revisamos como es ahora la distribucion de las distancias
d3 = dist(z[1,]) ##distance computed using just first dimension
mypar(1,1)
plot(as.numeric(d),as.numeric(d3)) 
abline(0,1)

#################################################################

#generamos un nuevo dataset de alturas de gemelos que sea bimodal:
#una distribución para adultos y una para niños

set.seed(1988)
n <- 100
x <- rbind(mvrnorm(n / 2, c(69, 69), matrix(c(9, 9 * 0.9, 9 * 0.92, 9 * 1), 2, 2)),
           mvrnorm(n / 2, c(55, 55), matrix(c(9, 9 * 0.9, 9 * 0.92, 9 * 1), 2, 2)))

#se ve que es bimodal en el plot
lim <- c(48, 78)
rafalib::mypar()
plot(x, xlim=lim, ylim=lim)

#Nuestras variables son otra vez vectores de dimension 2, con dos alturas. 
#Vamos a ver si con sólo una dimension somos capaces de identificar las dos 
#clases que tenemos: adultos y niños.

rafalib::mypar()


plot(x, xlim=lim, ylim=lim)
lines(x[c(1, 2),], col = "blue", lwd = 2)
lines(x[c(2, 51),], col = "red", lwd = 2)
points(x[c(1, 2, 51),], pch = 16)
d <- dist(x)
as.matrix(d)[1, 2]
as.matrix(d)[2, 51]

#La linea roja nos marca la distancia entre los dos puntos azules. 
#Usando la transformación que ya conocemos:

z  <- cbind((x[,2] + x[,1])/2,  x[,2] - x[,1])

#Vemos que la media es suficiente para explicar los datos 
#y que la distancia permanece igual al rotar:
rafalib::mypar()
plot(z, xlim=lim, ylim = lim - mean(lim))
lines(z[c(1,2),], col = "blue", lwd = 2)
lines(z[c(2,51),], col = "red", lwd = 2)
points(z[c(1,2,51),], pch = 16)

#explorando la primera dimension de z
qplot(z[,1], bins = 20, color = I("black"))

#tambien somos capaces de ver la bimodalidad, no en la segunda:
qplot(z[,2], bins = 20, color = I("black"))

#PCA ha funcionado bien porque las variables de x estaban correlacionadas
cor(x[,1], x[,2])

#las transformadas ya no lo están
cor(z[,1], z[,2])

#################################################################

#la suma de los cuadrados de cada columna es (si las variables estan centradas)
#la varianza de cada columna
colMeans(x^2) 
#la variable z recoge el 99% de la variabilidad de los datos
colMeans(z^2)
v <- colMeans(z^2)
v/sum(v)

#La _primera componente principal_ de una matriz $X$ es 
#la transformacion lineal ortogonal (su inversa es su traspuesta) 
#que maximiza la variabilidad. La función `prcomp` produce exactamente esto:
  
pca <- prcomp(x)
pca$rotation

#Es una matriz ortogonal (su traspuesta es igual a su inversa)
t(pca$rotation)
inv(pca$rotation)

c#comprobamos que svd en datos estandarizados produce el mismo resultado:
s <- svd( Y - rowMeans(Y) )
pc<-prcomp(t(Y))
mypar(1,2)
for(i in 1:nrow(Y) ){
  plot(pc$x[,i], s$d[i]*s$v[,i])
}

#los pesos de la rotacion son:
pc$rotation


