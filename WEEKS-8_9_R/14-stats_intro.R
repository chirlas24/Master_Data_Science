##########################################################################
# Jose Cajide - @jrcajide
# Master Data Science: Estadística y Modelado
##########################################################################



# Medidas de centralidad

cars$speed

x <- cars$speed
n <- length(x)

sum(x) / n
mean(x)

# La mediana es la observación numérica que divide los datos en dos partes iguales, de manera que una mitad queda bajo la mediana y la otra, por encima. Si tenemos un número impar de observaciones, la mediana es la observación central una vez ordenadas todas ellas. Si disponemos de un número par de observaciones, la mediana es el promedio de las dos observaciones centrales

(mediana <- median(x))

# Moda: La moda es el valor o categoría más frecuente

(velocidades <- unique(x)) 
velocidades[which.max(tabulate(match(x, velocidades)))]

# Medidas de dispersión

# La varianza es un estadístico que mide la dispersión de una distribución de frecuencias. 
# Específicamente, mide la dispersión de los datos respecto a su media.
sum((x-mean(x))^2)/((n-1))
var(x)

# La desviación estándar o típica es un estadístico que mide la dispersión de una distribuciónde frecuencias respecto a su media. 
# Es, concretamente, la raíz cuadrada de la varianza. 
# Supera la limitación de la varianza de venir expresada en las unidades de la variable al cuadrado. 
# Así, la desviación estándar viene medida en las unidades de la variable.

sqrt(sum((x - mean(x))^2) / (n - 1))
sqrt(var(x))
(desv_est <- sd(x))

# Rango o Recorrido: El recorrido o rango de una distribución de frecuencias es un estadístico que mide la dispersión de una distribución de frecuencias. 
# Concretamente, es la diferencia entre el valor máximo y el valor mínimo. 
# Cuanto mayor es el recorrido de una distribución de frecuencias, más dispersa es esta distribución.

(rango <- max(x) - min(x))
diff(range(x))

# El coeficiente de variación de Pearson es una medida de la dispersión relativa de una distribución de frecuencias. 
# Concretamente se define como el cociente entre la desviación estándar y la media de los datos. 
# Cuanto mayor es este coeficiente, menos representativa es la media (de la distribución) .

(pearson <- desv_est / mean(x))

## Modelos lineales
scatter.smooth(x=cars$speed, y=cars$dist, main="Dist ~ Speed") 

# Correlación
cor(cars$speed, cars$dist) 

modelo <- lm(dist ~ speed, data=cars) 

plot(x= cars$speed, y = cars$dist, main = "Cars", sub = "Gráfico de dispersión", bty="n", cex=0.5, cex.axis=0.6, pch=19, xlab="Velocidad", ylab="Distancia de frenado")
abline( modelo, col="red")

summary(modelo)

coefficients(modelo)

# Predicción
nuevos_datos <- data.frame(speed = 17)

predict(modelo, nuevos_datos)


# ¿Es válido el modelo para predecir?


set.seed(100) 

filas_aletorias <- sample(1:nrow(cars), 0.8*nrow(cars))  

datos_entrenamiento <- cars[filas_aletorias, ]  

datos_validacion  <- cars[-filas_aletorias, ]   

modelo <- lm(dist ~ speed, data=datos_entrenamiento) 

summary (modelo)

predicciones <- predict(modelo, datos_validacion) 

distancias <- data.frame(cbind(distancia_real=datos_validacion$dist, distancia_estimada=predicciones)) 

distancias


# Comparación de muestras para ver si tienen medias diferentes

# Pregunta: ¿Influye el tipo de transmisión en el consumo de un coche?

library(dplyr)
library(ggplot2)

mtcars %>% ggplot(aes(x=hp, y=mpg)) + geom_point() +geom_smooth(method = 'lm', se = F)

mtcars %>% ggplot(aes(x=hp, y=mpg, color=factor(am))) + geom_point() +geom_smooth(method = 'lm', se = F)

mtcars$mpg
mtcars$am

ggplot(mtcars, aes(x=factor(am), y=mpg)) + 
  geom_boxplot() + 
  labs(title="Comparación de medias", 
       subtitle= "Parece que los coches automáticos consumen más que los manuales. ¿Es una casualidad?", 
       caption = "Visualización: R + ggplot2")


# H0: Medias iguales
# H1: Medias no iguales

(resultados_test <- t.test(mpg ~ am, data=mtcars))
resultados_test$p.value

# p-value: probabilidad de que la diferencia aparente entre ambas medias se deba a la casualidad
# pValue < 0.05,  H0 No es cierto. Rechazamos H0


# Correlación

ggplot(mtcars, aes(x=wt, y=mpg)) + geom_point()

mtcars$mpg
mtcars$wt

cor.test(mtcars$mpg, mtcars$wt)

matriz_correlacion <- cor(mtcars)
matriz_correlacion

#install.packages("corrplot")
library(corrplot)
corrplot(matriz_correlacion, method="color")
corrplot(matriz_correlacion, method="number")

#----------------------------------------------------------------------------
# Modelado estadístico
#----------------------------------------------------------------------------

# https://docs.google.com/presentation/d/1sQIiz9klQLdrsI7vLWa17TFU441WSu8cHCtzhXA6sEU/pub?start=false&loop=false&delayms=60000

#----------------------------------------------------------------------------
# Modelos de regresión
#----------------------------------------------------------------------------

plot(mtcars)

modelo.1 <- lm(mpg ~ wt, mtcars)
summary(modelo.1)
predict(modelo.1)

coef(summary(modelo.1))

coche_nuevo = data.frame(wt=4.5)
predict(modelo.1, coche_nuevo)
# 37.2851 + (-5.3445) * 4.5


ggplot(mtcars, aes(wt, mpg)) + geom_point() + geom_smooth(method="lm")

# Modelo de regresión múltiple

ggplot(mtcars, aes(x=wt, y=mpg, col=cyl, size=disp)) + geom_point()

modelo.2 = lm(mpg ~ wt + disp + cyl, data=mtcars)
summary(modelo.2)

coef(summary(modelo.2))

predict(modelo.2)

coche_nuevo = data.frame(wt=4.5, disp=300, cyl=8)
coche_nuevo

predict(modelo.2, coche_nuevo)

# Comparamos ambos modelos
anova(modelo.1, modelo.2)


# Es válido este modelo para predecir?

set.seed(100) 

filas_aletorias <- sample(1:nrow(mtcars), 0.8*nrow(mtcars))  

datos_entrenamiento <- mtcars[filas_aletorias, ]  

datos_validacion  <- mtcars[-filas_aletorias, ]   

modelo.3 <- lm(mpg ~ wt + disp + cyl, data=datos_entrenamiento) 

summary (modelo.3)

predicciones <- predict(modelo.3, datos_validacion) 

consumos <- data.frame(cbind(consumo_real=datos_validacion$mpg, consumo_estimado=predicciones)) 

consumos

rmse <- function(valores_predecidos, valores_observados){
  sqrt(mean((valores_predecidos - valores_observados)^2))
}

rmse(consumos$consumo_estimado, consumos$consumo_real)


# Ejercicio 1: Repite el modelo incluyendo la variable "am": transmisión y comprueba si esta afecta al consumo

set.seed(100) 

filas_aletorias <- sample(1:nrow(mtcars), 0.8*nrow(mtcars))  

datos_entrenamiento <- mtcars[filas_aletorias, ]  

datos_validacion  <- mtcars[-filas_aletorias, ]   

modelo.4 <- lm(mpg ~ wt + cyl + am, data=datos_entrenamiento) 

summary (modelo.4)

predicciones <- predict(modelo.4, datos_validacion) 

consumos <- data.frame(cbind(consumo_real=datos_validacion$mpg, consumo_estimado=predicciones)) 

consumos

rmse <- function(valores_predecidos, valores_observados){
  sqrt(mean((valores_predecidos - valores_observados)^2))
}

rmse(consumos$consumo_estimado, consumos$consumo_real)


# Errores del modelo
fitted(modelo.3)
residuals(modelo.3)
plot(fitted(modelo.3), resid(modelo.3))
abline(h = 0)


# Ejercicio 2: Encuenta el modelo de regresión lineal que mejor se ajuste a los siguientes datos:

ventas <- c(10,12,11,13,12,14,16,12,14,11,10,19,8.5,8,9,13,16,18,20,22)
tv <- c(13,14,15,17,17.5,13,14.5,9,8,9,8,10,17,18,18.5,19,20,20,13,14)
radio <-c(56,55,60,65,69,67,68,67,97,66,65,60,70,110,75,80,85,90,56,55)
online <- c(40,40,42,50,40,44,40,44,46,46,45,110,30,50,45,40,80,90,90, 110)



set.seed(100)

df <- data.frame(ventas, tv, radio, online)

filas_aletorias <- sample(1:nrow(df), 0.8*nrow(df))  

datos_entrenamiento <- df[filas_aletorias, ]  

datos_validacion  <- df[-filas_aletorias, ]   

modelo.ex <- lm(ventas ~ online, data=datos_entrenamiento) 

summary (modelo.ex)

predicciones <- predict(modelo.ex, datos_validacion) 

ventas <- data.frame(cbind(ventas_real=datos_validacion$ventas, ventas_estimadas=predicciones)) 

rmse(ventas$ventas_estimadas, ventas$ventas_real)



#----------------------------------------------------------------------------
# Modelos de clasificación
#----------------------------------------------------------------------------

# https://docs.google.com/presentation/d/14ac22V-8Y-69JzBW8FGwxJGqpOhg00pLmdr86cAWFF8/edit?usp=sharing

# Regresión logística


modelo.4 = glm(formula = am ~ hp + wt,  data=datos_entrenamiento, family=binomial)
summary (modelo.4)

coche_nuevo = data.frame(hp=120, wt=2.8)
coche_nuevo

predict(modelo.4, coche_nuevo, type="response") 

datos_validacion$pred <- round(predict(modelo.4, datos_validacion, type = "response") ,2)

datos_validacion[, c('am', 'pred')]

install.packages("pROC")
library(pROC)
modelo.4.roc <- pROC::roc(datos_validacion$am, datos_validacion$pred)
plot(modelo.4.roc)
modelo.4.roc$auc


datos_validacion$pred
pred.logit <- rep(0,length(datos_validacion$pred))
pred.logit[datos_validacion$pred>=0.9] <- 1
pred.logit

modelo.4.roc.ajustado <- pROC::roc(datos_validacion$am, pred.logit)
plot(modelo.4.roc.ajustado)


#install.packages("randomForest")
library(randomForest)

table(mtcars$am)/nrow(mtcars)
table(mtcars$am)

table(datos_entrenamiento$am)/nrow(datos_entrenamiento)
table(datos_entrenamiento$am)

set.seed(123)

datos_entrenamiento$am <- as.factor(datos_entrenamiento$am)
levels(datos_entrenamiento$am)
datos_validacion$am <- as.factor(datos_validacion$am)

modelo.5 <- randomForest(am ~ .,data=datos_entrenamiento, ntree=20, importance=TRUE)
datos_validacion$pred_rf <- predict(modelo.5, datos_validacion)

datos_validacion[, c('am', 'pred', 'pred_rf')]

modelo.5.roc <- pROC::roc(as.numeric(datos_validacion$am), as.numeric(datos_validacion$pred_rf))
plot(modelo.5.roc)
modelo.5.roc$auc

#install.packages('caret')
library("caret")

caret::confusionMatrix(datos_validacion$pred_rf, datos_validacion$am)

caret::confusionMatrix(data=datos_validacion$pred_rf,
                       reference=datos_validacion$am,
                       positive='1')

varImpPlot(modelo.5,
           sort = T,
           main="Feature Importance",
           n.var=5)


# Árbol de decisión
#install.packages('party')
library (party)

modelo.6 <- ctree (am ~ ., data = datos_entrenamiento)  

modelo.6

plot (modelo.6, main="Árbol de decisión") 

# Ejercicio: Evaluar el resultado de este modelo en el conjunto de datos de validación
# ...


valores_estimados <- as.character (predict(modelo.6, datos_validacion)) 

valores_actuales <- as.character (datos_validacion$am) 

# % error de clasificación
mean (valores_estimados != valores_actuales) 



#----------------------------------------------------------------------------
# Ejercicio de clasificación y árboles
#----------------------------------------------------------------------------
#https://archive.ics.uci.edu/ml/datasets/Statlog+(German+Credit+Data)

table(good_bad)

# datos_entrenamiento<-subset(datos_entrenamiento, select=-default)
datos_entrenamiento <- datos_entrenamiento %>% dplyr::select(-(default))

modelo_scoring <- glm(good_bad~.,data=datos_entrenamiento,family=binomial())
summary(modelo_scoring)

datos_validacion$score <- predict(modelo_scoring,type='response',datos_validacion)

datos_validacion[, c('default', 'score')]

modelo_scoring.roc <- pROC::roc(datos_validacion$default, datos_validacion$score)
plot(modelo_scoring.roc)
modelo_scoring.roc$auc


# Árbol -------------------------------------------------------------------

library(rpart)
modelo_scoring.2 <- rpart(good_bad~.,data=datos_entrenamiento)

plot(modelo_scoring.2)
text(modelo_scoring.2)

datos_validacion$score_rpart <- predict(modelo_scoring.2,type='vector', newdata = datos_validacion)

datos_validacion[, c('default', 'score_rpart')]

modelo_scoring.2.roc <- pROC::roc(datos_validacion$default, datos_validacion$score_rpart)
plot(modelo_scoring.2.roc)
modelo_scoring.2.roc$auc


install.packages("FFTrees")
library("FFTrees")

datos_entrenamiento$default <- good_bad
datos_validacion <- credit[-filas_aletorias,]
modelo_scoring.3 <- FFTrees(default ~ .,data=datos_entrenamiento,data.test = datos_validacion)
modelo_scoring.3$auc

plot(modelo_scoring.3,
     main = "German Credit Data",decision.labels = c("Good", "Bad"))

plot(modelo_scoring.3,
     main = "German Credit Data",decision.labels = c("Good", "Bad"), tree = 7)


#----------------------------------------------------------------------------
# Agrupamiento
#----------------------------------------------------------------------------

# Agrupamiento jerárquico

# Preparación de los datos
# https://en.wikipedia.org/wiki/Standard_score

mpg <- mtcars$mpg
stripchart(mpg)

mpg_escalado <- (mtcars$mpg - mean(mtcars$mpg)) / sd(mtcars$mpg)
stripchart(mpg_escalado)

mtcars.escalado <- scale(mtcars)
head(mtcars)
head(mtcars.escalado)

distancias <- dist(mtcars.escalado)

cluster_jerarquico <- hclust(distancias)
plot(cluster_jerarquico)

rect.hclust(cluster_jerarquico, k=5, border="red")

grupos <- cutree(cluster_jerarquico, 5)

mtcars$grupo <- grupos
table(mtcars$grupo)

resumen <- mtcars %>% group_by(grupo) %>% summarise_all(mean) %>% mutate(num_coches = table(mtcars$grupo))
resumen


# K-means
# https://github.com/joseramoncajide/master_data_science_capstone
# https://es.wikipedia.org/wiki/K-means


# ¿Cuántos grupos?
wss <- (nrow(mtcars.escalado)-1)*sum(apply(mtcars.escalado,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mtcars.escalado, 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Número de cluster",
     ylab="Error")

modelo_kmeans <- kmeans(mtcars.escalado, 5) 
mtcars$grupo_kmeans <- modelo_kmeans$cluster

resumen <- mtcars %>% group_by(grupo_kmeans) %>% summarise_all(mean) %>% mutate(num_coches = table(mtcars$grupo_kmeans))
resumen

library(cluster) 
clusplot(mtcars, mtcars$grupo_kmeans, color=TRUE, shade=TRUE, labels=2, lines=0, main = "Agrupación")

# Reducción de la dimensionalidad

componentes_principales <- prcomp(mtcars.escalado, center=F, scale=F,retx=T)
summary(componentes_principales)

# Loadings o ejes principales
componentes_principales$rotation

plot(componentes_principales)
biplot(componentes_principales)


