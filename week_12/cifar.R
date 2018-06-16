library(keras)
data <- dataset_cifar10()
str(data)
#[50000筆資料,32X32像素,3種顏色RGB]
data$train$x[1,,,1]
train_x <- data$train$x/255 # (0-255) -> floating point value(0-1)
train_y <- data$train$y
test_x <- data$test$x/255 # (0-255) -> floating point value(0-1)
test_y <- data$test$y
#one-hot encode
train_y <- to_categorical(train_y,num_classes = 10)
test_y <- to_categorical(test_y,num_classes = 10)
#model
cnnmodel <- keras_model_sequential()

cnnmodel %>% 
  layer_conv_2d(filter=32,kernel_size=3,padding='same',input_shape=c(32,32,3)) %>% 
  layer_activation("relu") %>%
  layer_max_pooling_2d(pool_size=c(2,2), strides=c(2,2), padding="same") %>%
  layer_conv_2d(filters = 64, kernel_size = 3, padding = 'same') %>% 
  layer_activation("relu") %>%
  layer_max_pooling_2d(pool_size=c(2,2), strides=c(2,2), padding="same") %>%
  layer_dropout(rate=0.2) %>% 
  layer_flatten() %>%
  layer_dense(units = 512, activation = 'relu', kernel_initializer = 'uniform',kernel_constraint = constraint_maxnorm(max_value=3)) %>% 
  layer_dropout(rate=0.2) %>% 
  layer_dense(units =256 , activation = 'relu', kernel_initializer = 'uniform',kernel_constraint = constraint_maxnorm(max_value=3)) %>% 
  layer_dropout(rate=0.2) %>% 
  layer_dense(units = 10,activation = 'softmax')
summary(cnnmodel)

cnnmodel %>% compile(loss = 'categorical_crossentropy',
                  optimizer = 'adam',
                  metrics = c('accuracy')
)


history <- cnnmodel %>% fit(
  train_x, train_y,
  batch_size = 128,
  epochs = 20,
  verbose = 1,
  callbacks = callback_tensorboard(),
  validation_split = 0.2
)

cnnmodel %>% evaluate(test_x,test_y)
