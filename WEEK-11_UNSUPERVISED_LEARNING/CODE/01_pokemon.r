###################################################
#      pokemon
###################################################

#data generation
pokemons=c("blastoise","charizard","primarina","delphox","pikachu")
blastoise=c("azul","laguna","marisco","agua","NO","SI","NO")
charizard=c("naranja","pradera","llama","fuego","NO","SI","SI")
primarina=c("azul","laguna","solista","agua","NO","SI","NO")
delphox=c("naranja","pradera","zorro","fuego","NO","SI","NO")
pikachu=c("amarillo","bosque","ratón","eléctrico","NO","SI","NO")

all.pokemon=data.frame(blastoise,charizard,primarina,delphox,pikachu)
rownames(all.pokemon)=c("color", "hábitat", "categoría", "tipo", "aletas", "cola", "alas")

#heatmap
library(ComplexHeatmap) 
Heatmap(all.pokemon)

#distancia
sim<-matrix(NA,5,5)
for (i in 1:5){
  for (j in 1:5){sim[i,j]<-length(setdiff(all.pokemon[,i],all.pokemon[,j]))}
}
colnames(sim)=pokemons
rownames(sim)=pokemons

#clustering for categorical variables
library(cluster)
divisive.clust <- diana(sim, 
                        diss = TRUE, keep.diss = TRUE)
plot(divisive.clust, main = "Divisive")
