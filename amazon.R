rm(list=ls())

library(text2vec)
setwd("G:\\math\\514")

require(keras) 
require(beepr)
amdir <- "C:\\Users\\Administrator\\Documents\\reviews.csv"
#	amazon <- read.csv(  amdir , stringsAsFactors= F )
#	amazon$Text <- as.character(amazon$Text )

library(tensorflow)
sess = tf$Session()
hello <- tf$constant('Hello, TensorFlow!') 
sess$run(hello)

load("amazon.RData")

it_train = itoken(amazon$Text, 
             preprocessor = tolower, 
             tokenizer = word_tokenizer, 
             ids = amazon$Id, 
             progressbar = T)
vocab = create_vocabulary(it_train)

#mylist <- list()
#mylist[[ nrow(amazon) + 1 ]] <- rep(NA, nrow(amazon))

# system.time(
#   for( i in 15136:(nrow(amazon)-100000) ){
#     a <- strsplit(gsub("[^[:alnum:] ]", "", amazon[i,10]), " +")[[1]] 
#     a <-  match(a , vocab[,1] )
#     mylist[[i]] <- a[!is.na( a )]
#     mylist[[nrow(amazon) + 1 ]][i]  <- amazon[i, ]$Score
#   }
# )

 

##########################
testlist <- list()
 
for( i in 1:22){	testlist[[i]] <- rpois( rpois(1,20) , 100 ) }
sapply( testlist, length)


tlt <- sample( 1:length(testlist), length(testlist)/ 2 )
tltr <-  setdiff(  1:length(testlist) , tlt )

# sort( c(tlt,setdiff(  1:length(testlist) , tlt )) ) - 1:length(testlist) 
testlist[[length(testlist)+1]] <- rnorm(22)

testlist
ourlist <- list()

ourlist[[1]]  <- list()
ourlist[[1]]$x <- testlist[tlt]
ourlist[[1]]$y <- testlist[[length(testlist)]][tlt]

ourlist[[2]]  <- list()
ourlist[[2]]$x <- testlist[tltr ]
ourlist[[2]]$y <- rnorm(11)

 

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 

library(text2vec)
require(keras) 
require(beepr)
#	amazon <- read.csv("reviews.csv")
#	amazon$Text <- as.character(amazon$Text )
setwd("G:\\math\\514")

library(tensorflow)
sess = tf$Session()
hello <- tf$constant('Hello, TensorFlow!') 
sess$run(hello)

load("amazon.RData")

mylist <- amzstuff[[1]]
x_train <- amzstuff[[2]]
x_test <- amzstuff[[3]]

amzlist <- list()
amzlist

testsamples <- sample( 1:(length(mylist)-1) , (length(mylist)-1)/ 2 )
trainsamples <-  setdiff(  1:(length(mylist)-1)  , testsamples)
length(testsamples ) == length(trainsamples )

amzlist[[1]]  <- list()
amzlist[[1]]$x <- mylist[testsamples]
amzlist[[1]]$y <- mylist[[( length(mylist)  )]][testsamples]
amzlist[[2]]  <- list()
amzlist[[2]]$x <- mylist[trainsamples ]
amzlist[[2]]$y <- mylist[[( length(mylist)  )]][trainsamples ]
names(amzlist) <- c("train","test")

head( amzlist$train$x )


max( unlist( mylist  ) )


# Set parameters:
max_features <- 400		#amount of total words
maxlen <- 200
batch_size <- 32
embedding_dims <- 50
filters <- 250
kernel_size <- 3
hidden_dims <- 250
epochs <- 2




x_train <- amzlist$train$x %>%
  pad_sequences(maxlen = maxlen)
#beep("coin")

x_test <- amzlist$test$x %>%
 pad_sequences(maxlen = maxlen)

x_train <- xtrain %>%
  pad_sequences(maxlen = maxlen)
#beep("coin")

x_test <- xtest %>%
 pad_sequences(maxlen = maxlen)
gc()


beep("coin")


model <- keras_model_sequential()

model %>% 
  # Start off with an efficient embedding layer which maps
  # the vocab indices into embedding_dims dimensions
  layer_embedding(max_features, embedding_dims, input_length = maxlen) %>%
  layer_dropout(0.2) %>%

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

  layer_dense(5) %>%
  layer_activation("softmax")%>%
  layer_dropout(0.2)  



model <- keras_model_sequential() %>% 
	layer_dense(400 , activation = "relu", input_shape = c(200) )  %>%
		layer_dropout(0.2) %>%
 	layer_dense(400 , activation = "relu" )  %>%
		layer_dropout(0.2) %>%
	layer_simple_rnn(units = 32 )  %>%
	layer_dense(5 , activation = "softmax" )  

  model <- keras_model_sequential() %>%
   layer_dense(400 , activation = "relu", input_shape = c(200) )  %>%
  layer_dropout(0.2) %>%
   layer_dense(400 , activation = "relu", input_shape = c(200) )  %>%
  layer_dropout(0.2) %>%
 
  layer_dense(5 , activation = "softmax" )  
  


# Compile model
model %>% compile(
  loss = "categorical_crossentropy",
  optimizer = "adam",
  metrics = "accuracy"
)

# Training ----------------------------------------------------------------

model %>%
  fit(
    x_train, t(ytrain),
    batch_size = batch_size,
    epochs = 10 ,
    validation_data = list(x_test, t(ytest ) )
  )






