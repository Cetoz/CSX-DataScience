library(keras)
#匯入資料
data <- dataset_mnist()
x_train <- data$train$x
y_train <- data$train$y
x_test <- data$test$x
y_test <- data$test$y
rm(data)
#檢視資料
str(x_train)
x_train[1,,]#第一個數字

#x_train是3-d array，將其轉換成1-d matrix(reshaping width and height)
#使用array_reshape()而不用dim()，是因為前者會按row排，符合Keras 解讀array的方式
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
#Evaluate the model’s performance on the test data
model %>% evaluate(x_test, y_test)
#Generate predictions on new data
model %>% predict_classes(x_test)

callback_tensorboard(log_dir = 'C:\\Users\\FuHung\\Documents\\GitHub\\CSX-DataScience\\week_12')
sess = k_clear_session()

#https://cran.r-project.org/web/packages/keras/vignettes/getting_started.html