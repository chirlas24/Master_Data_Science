##############################################3
#        IRIS DATA SET
################################################
names(iris)
head(iris)
dim(iris)
library("dplyr")

#distancias
x <- iris[,1:4] %>% as.matrix()
d <- dist(x)
image(as.matrix(d), col = rev(RColorBrewer::brewer.pal(9, "RdBu")))

cor(x)

pca <- prcomp(x)
summary(pca)

rafalib::mypar()
illustrate_pca(x)

library("ggplot")

data.frame(pca$x[,1:2], Species=iris$Species) %>% 
  ggplot(aes(PC1,PC2, fill = Species))+
  geom_point(cex=3, pch=21) +
  coord_fixed(ratio = 1)

#Las dos primeras componentes preservan la distancia:
d_approx <- dist(pca$x[, 1:2])
qplot(d, d_approx) + geom_abline(color="red")



