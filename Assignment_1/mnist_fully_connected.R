library(tidyverse)
library(devtools)
library(reticulate)



## Load assignment Specific packages

library(keras)
library(kerasR)
library(tensorflow)



## Install keras formally
## keras::install_keras()
## install_tensorflow()



## Specify Python Path
reticulate::use_python("C:\\Users\\samme\\Anaconda3\\python.exe")






## Set seed 

set.seed(37180)

mnist <- dataset_mnist()
x_train <- mnist$train$x
y_train <- mnist$train$y
x_test <- mnist$test$x
y_test <- mnist$test$y


x_train <- array_reshape(x_train, c(nrow(x_train), 784))
x_test <- array_reshape(x_test, c(nrow(x_test), 784))
# rescale
x_train <- x_train / 255
x_test <- x_test / 255

y_train <- keras::to_categorical(y_train, 10)
y_test <- keras::to_categorical(y_test, 10)

model <- keras_model_sequential() 
model %>%
  layer_dense(units = 256,activation = 'relu', input_shape = c(784)) %>%
  
 
  layer_dense(units = 10, activation = 'softmax')

summary(model)



model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_rmsprop(),
  metrics = c('accuracy')
)

history <- model %>% fit(
  x_train, y_train,
  batch_size = 128,
  epochs = 12,
  verbose = 1,
  validation_split = 0.2
)

score <- model %>% evaluate(
  x_test, y_test,
  verbose = 0
  
)


plot(history)

model %>% evaluate(x_test, y_test)
model %>% predict_classes(x_test)
print("done")
