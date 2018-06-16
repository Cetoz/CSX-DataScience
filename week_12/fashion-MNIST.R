library(keras)
fashiondata <- dataset_fashion_mnist()
x_train <- fashiondata$train$x
y_train <- fashiondata$train$y
x_test <- fashiondata$test$x
y_test <- fashiondata$test$y
rm(fashiondata)


x_train <- array_reshape(x_train,dim = c(nrow(x_train),784))
x_test <- array_reshape(x_test,dim = c(nrow(x_test),784))
#將grayscale value(0-255)轉換為floating point value(0-1)
x_train <- x_train/255
x_test <- x_test/255
#one-hot encode the vectors into binary class matrices 
y_train <- to_categorical(y_train,10)
y_test <- to_categorical(y_test,10)
#creating a sequential model and then adding layers using the pipe (%>%) operator
model <- keras_model_sequential()
model %>% 
  layer_dense(units = 256, activation = 'relu', input_shape=c(784)) %>% 
  layer_dropout(rate = 0.4) %>% 
  layer_dense(units = 128, activation = 'relu') %>% 
  layer_dropout(rate = 0.3) %>% 
  layer_dense(units = 10,activation = 'softmax')
summary(model)
#compile the model with appropriate loss function, optimizer, and metrics
model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_rmsprop(),
  metrics = c('accuracy')
)
#Use the fit() function to train the model for 30 epochs using batches of 128 images
history <- model %>% fit(
  x_train, y_train,
  epoch = 30, batch_size = 128,
  validation_split = 0.2
)
plot(history)

model %>% evaluate(x_test, y_test)








#======================================CNN==========================================
fashiondata <- dataset_fashion_mnist()
x_train <- fashiondata$train$x
y_train <- fashiondata$train$y
x_test <- fashiondata$test$x
y_test <- fashiondata$test$y
#畫第一筆資料
image(x_train[1,,], col = grey.colors(255))
#將grayscale value(0-255)轉換為floating point value(0-1)
x_train <- x_train/255
x_test <- x_test/255
#轉換維度
x_train <- array_reshape(x_train, dim=c(60000,28,28,1))
x_test <- array_reshape(x_test, dim=c(10000,28,28,1))
#one-hot encode
y_train <- to_categorical(y_train,10)
y_test <- to_categorical(y_test,10)
#Model
cnnmodel <- keras_model_sequential()
cnnmodel %>% 
  layer_conv_2d(filters = 28, kernel_size = 2, padding = 'same', activation = 'relu', input_shape = c(28,28,1)) %>% 
  layer_conv_2d(filters = 56, kernel_size = 2, padding = 'same', activation = 'relu') %>% 
  layer_max_pooling_2d(pool_size=c(2,2), strides=c(2,2), padding="same") %>% 
  layer_conv_2d(filters = 56, kernel_size = 2, padding = 'same', activation = 'relu') %>% 
  layer_conv_2d(filters = 56, kernel_size = 2, padding = 'same', activation = 'relu') %>% 
  layer_max_pooling_2d(pool_size=c(2,2), strides=c(2,2), padding="same") %>% 
  layer_dropout(rate=0.3) %>% 
  layer_flatten() %>% 
  layer_dense(units = 128, activation = 'relu', kernel_initializer = 'uniform') %>% 
  layer_dense(units = 64, activation = 'relu', kernel_initializer = 'uniform') %>% 
  layer_dense(units = 10,activation = 'softmax')
summary(cnnmodel)

cnnmodel %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = 'adam',
  metrics = c('accuracy')
)

history2 <- cnnmodel %>% fit(
  x_train,y_train,
  epoch = 20, batch_size=128,
  validation_split=0.2
)

plot(history2)
cnnmodel %>% evaluate(x_test, y_test)
