library(dplyr)
#install.packages("Rtsne")
library(Rtsne)
library(dslabs)
if(!exists("mnist")) mnist <- read_mnist()

train_full <- mnist$train$images
train <- sample_frac(as.data.frame(train_full),0.33)

## Curating the database for analysis with both t-SNE and PCA
label<-mnist$train$label
## for plotting
colors = rainbow(10)
names(colors) = unique(label)

## Executing the algorithm on curated data
tsne <- Rtsne(train[,-1], dims = 2, 
              perplexity=30, 
              verbose=TRUE, 
              max_iter = 500,
              col=label)
exeTimeTsne<- system.time(Rtsne(train[,-1], dims = 2, perplexity=30, verbose=TRUE, max_iter = 500))

plot(tsne$Y, t='n', main="tsne")
text(tsne$Y, labels=label, col=colors[label])

