library(text2vec)
require(keras) 
require(beepr)
#	amazon <- read.csv("reviews.csv")
#	amazon$Text <- as.character(amazon$Text )
head(amazon)

data("movie_review")
train_ids = sample(movie_review$id, 4000)
 train = movie_review[J(train_ids)]




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

 

### 


alist <- list()
alist

testsamples <- sample( 1:(length(mylist)-1) , (length(mylist)-1)/ 2 )
trainsamples <-  setdiff(  1:(length(mylist)-1)  , testsamples)
length(testsamples ) == length(trainsamples )

alist[[1]]  <- list()
alist[[1]]$x <- mylist[testsamples]
alist[[1]]$y <- mylist[[( length(mylist)  )]][testsamples]
alist[[2]]  <- list()
alist[[2]]$x <- mylist[trainsamples ]
alist[[2]]$y <- mylist[[( length(mylist)  )]][trainsamples ]
names(alist) <- c("train","test")

head( alist$train$x )


max( unlist( mylist  ) )


# Set parameters:
max_features <- 138531
maxlen <- 60
batch_size <- 32
embedding_dims <- 50
filters <- 250
kernel_size <- 3
hidden_dims <- 250
epochs <- 2




x_train <- alist$train$x %>%
  pad_sequences(maxlen = maxlen)
beep("coin")

x_test <- alist$test$x %>%
  pad_sequences(maxlen = maxlen)

beep("coin")


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
    x_train, alist$train$y,
    batch_size = batch_size,
    epochs = epochs,
    validation_data = list(x_test, alist$test$y)
  )






