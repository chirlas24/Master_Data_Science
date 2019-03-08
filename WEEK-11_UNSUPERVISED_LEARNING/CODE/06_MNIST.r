##################################################
#            MINST data set
##################################################

library(dslabs)
if(!exists("mnist")) mnist <- read_mnist()
str(mnist)

col_means <- colMeans(mnist$test$images)
pca <- prcomp(mnist$train$images)

pc <- 1:ncol(mnist$test$images)
qplot(pc, pca$sdev)

summary(pca)$importance[,1:5] 

#Tomando una muestra de 2000 dígitos:
data.frame(PC1 = pca$x[,1], PC2 = pca$x[,2],
           label=factor(mnist$train$label)) %>%
  sample_n(2000) %>% 
  ggplot(aes(PC1, PC2, fill=label))+
  geom_point(cex=3, pch=21)

Vamos a ver los pesos:
  

tmp <- lapply( c(1:4,781:784), function(i){
  expand.grid(Row=1:28, Column=1:28) %>%
    mutate(id=i, label=paste0("PC",i), 
           value = pca$rotation[,i])
})

tmp <- Reduce(rbind, tmp)
tmp %>% filter(id<5) %>%
  ggplot(aes(Row, Column, fill=value)) +
  geom_raster() +
  scale_y_reverse() +
  scale_fill_gradientn(colors = brewer.pal(9, "RdBu")) +
  facet_wrap(~label, nrow = 1)

#Las PCs con menor varianza aparecen en las esquinas:
  
tmp %>% filter(id>5) %>%
  ggplot(aes(Row, Column, fill=value)) +
  geom_raster() +
  scale_y_reverse() +
  scale_fill_gradientn(colors = brewer.pal(9, "RdBu")) +
  facet_wrap(~label, nrow = 1)

#36 dimensiones explican el 80% de los datos:
  
#First fit the model:
library(caret)
k <- 36
x_train <- pca$x[,1:k]
y <- factor(mnist$train$labels)
fit <- knn3(x_train, y)

#Transformamos el test:
x_test <- sweep(mnist$test$images, 2, col_means) %*% pca$rotation
x_test <- x_test[,1:k]

#Predecimos:
y_hat <- predict(fit, x_test, type = "class")
confusionMatrix(y_hat, factor(mnist$test$labels))$overall["Accuracy"]

