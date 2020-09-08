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


x_train <- array_reshape(x_train, c(60000, 28, 28, 1))
x_test <- array_reshape(x_test, c(10000, 28, 28, 1))
# rescale
x_train <- x_train / 255
x_test <- x_test / 255

y_train <- keras::to_categorical(y_train, 10)
y_test <- keras::to_categorical(y_test, 10)


model <- keras_model_sequential() %>%
  layer_conv_2d(filters = 32, kernel_size = c(3,3),
                activation = 'relu', input_shape = c(28,28,1)) %>%
  layer_conv_2d(filters = 64, kernel_size = c(3,3),
                activation = 'relu') %>%
  layer_max_pooling_2d(pool_size = c(2,2)) %>%
  #layer_dropout(0.25) %>%
  layer_flatten() %>%
  layer_dense(units = 128, activation = 'relu') %>%
  #layer_dropout(0.5) %>%
  layer_dense(units = 10, activation = 'softmax')

summary(model)



model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_adadelta(),
  metrics = c('accuracy')
)

history <- model %>% fit(
  x_train, y_train,
  batch_size = 128,
  epochs = 6,
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
