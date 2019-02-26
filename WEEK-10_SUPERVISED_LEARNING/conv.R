#Convolutional neural network
#La de las casillitas


#install.packages("keras")
library("keras")

# Keras -------------------------------------------------------------------

# Tarda 20min en portatil

#install_keras()

mnist <- dataset_mnist()

image(as.matrix(mnist$train$x[2,,]))
mnist$train$y[2]

x_train <- mnist$train$x
y_train <- mnist$train$y

x_test <- mnist$test$x
y_test <- mnist$test$y

#redimiensionamiento sustituye al flatten

x_train <- x_train %>% array_reshape(c(60000, 28, 28, 1))
x_test <- x_test %>% array_reshape(c(10000, 28, 28, 1))

image(x_train[2,,,])

#Nomralizar el input (de 0 a 255) a (0 - 1)

x_train <- x_train/255
x_test <- x_test/255


#One-hot encoding a dummy variable

y_train <- to_categorical(y_train, 10)
y_test_real <- y_test
y_test <- to_categorical(y_test, 10)


#Modelo

modelo_cnn <- keras_model_sequential()

#Si tuviera color seria con 2 canales o mas en el shape
# Es 2d porque el arrastre se produce en 2 dimensiones

modelo_cnn %>%
  layer_conv_2d (kernel_size = c(3,3), activation = "relu", filters = 32 , input_shape = c(28, 28, 1)) %>%  #Ya no podemos usar el flatten, si es rgb o colores tenemos que meterle mas canales
  layer_conv_2d (kernel_size = c(3,3), activation = "relu", filters = 64) %>%
  layer_max_pooling_2d(pool_size = c(3,3)) %>% 
  layer_dropout(rate = 0.25) %>% 
  layer_flatten() %>% 
  layer_dense(unit=128, activation = "relu") %>% 
  layer_dropout(rate=.5) %>% 
  layer_dense(unit=10, activation = "softmax")

# Funcion de coste
modelo_cnn %>%
  compile(
    loss = "categorical_crossentropy",
    optimizer = optimizer_adadelta(),
    metric = c("accuracy")
  )


resultado <- fit(modelo_cnn, x_train, y_train, epochs = 3, batch_size = 64, validation_split = 0.2)
  