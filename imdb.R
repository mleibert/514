library(keras)

imdb <- dataset_imdb( num_words = 5000 )
 

# shrink data set

length(imdb[[1]]$x);length(imdb[[2]]$x)
length(imdb[[1]]$y);length(imdb[[2]]$y)

#	imdb[[1]]$x <- imdb[[1]]$x[1:5000]
#	imdb[[2]]$x <- imdb[[2]]$x[1:5000]

#	imdb[[1]]$y <- imdb[[1]]$y[1:5000]
#	imdb[[2]]$y <- imdb[[2]]$y[1:5000]



names(imdb)

mylist <- list()
for( i in 1:10){	mylist[[i]] <- rpois( rpois(1,20) , 100 ) }
sapply( mylist, length)

mylist[1:5]



x_train <- imdb$train$x %>% pad_sequences(maxlen = 400)
length( imdb$train$x ); dim( x_train  )
layer_embedding( 5000, 50 , 400 )
# 5000 is max_features, the 5000 most common words
# 50 is embedding dims ?
# 400 is input_length / maxlen, 
	#"length in input sequences when it is constant"





# Set parameters:
max_features <- 5000
maxlen <- 400
batch_size <- 32
embedding_dims <- 50
filters <- 250
kernel_size <- 3
hidden_dims <- 250
epochs <- 2

x_train <- imdb$train$x %>%
  pad_sequences(maxlen = maxlen)
x_test <- imdb$test$x %>%
  pad_sequences(maxlen = maxlen)


model <- keras_model_sequential()

model %>% 
  # Start off with an efficient embedding layer which maps
  # the vocab indices into embedding_dims dimensions
  layer_embedding(max_features, embedding_dims, input_length = maxlen) %>%
  layer_dropout(0.2) %>%

  # Add a Convolution1D, which will learn filters
    # Word group filters of size filter_length:
  layer_conv_1d(
    filters, kernel_size, 
    padding = "valid", activation = "relu", strides = 1
  ) %>%
  # Apply max pooling:
  layer_global_max_pooling_1d() %>%

  # Add a vanilla hidden layer:
  layer_dense(hidden_dims) %>%

  # Apply 20% layer dropout
  layer_dropout(0.2) %>%
  layer_activation("relu") %>%

  # Project onto a single unit output layer, and squash it with a sigmoid

  layer_dense(1) %>%
  layer_activation("sigmoid")

# Compile model
model %>% compile(
  loss = "binary_crossentropy",
  optimizer = "adam",
  metrics = "accuracy"
)

# Training ----------------------------------------------------------------

model %>%
  fit(
    x_train, imdb$train$y,
    batch_size = batch_size,
    epochs = epochs,
    validation_data = list(x_test, imdb$test$y)
  )
