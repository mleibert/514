library(keras)
library(tm)
library(tensorflow)
sess = tf$Session()
hello <- tf$constant('Hello, TensorFlow!')
sess$run(hello)
 
amazon <- read.csv("reviews.csv", stringsAsFactors= F)
one.hot <- function(Z){return(unname( as.matrix( 
  as.data.frame( t( model.matrix(~ as.factor(Z) + 0) ) ) ) )) }
 
testsamples <- sample( 1:nrow(amazon)   , ((nrow(amazon) )/ 2 ))
trainsamples <-  setdiff(  1:nrow(amazon)   , testsamples)
ytrain <- amazon[trainsamples,]$Score
ytest <- amazon[testsamples ,]$Score
 
length(testsamples ) == length(trainsamples )
samples <- amazon[,10]; rm(amazon)
samples <- removeWords(samples, stopwords("en"))
samples <- stripWhitespace(samples)

max_features <- 1000
gc()
 
tokenizer <- text_tokenizer( num_words = max_features   ) %>% 
	fit_text_tokenizer(samples)
 
oh_results <- texts_to_matrix(tokenizer,samples, mode = "tfidf")
dim(oh_results)
 
 
 
x_train <- oh_results[trainsamples,]
x_test <- oh_results[testsamples ,]
rm(oh_results)
 
ytrain <- t( one.hot(ytrain ) )
ytest <- t( one.hot(ytest) )
 
 
 
batch_size <- 32
embedding_dims <- 50
filters <- 250
kernel_size <- 3
hidden_dims <- 250
epochs <- 10

fitlist <- list()
 
 
 
maxlen <- dim(x_train)[2]
 

# 
# model <- keras_model_sequential() %>%
#   #layer_embedding(input_dim=max_features,
# 	#	 output_dim=embedding_dims, input_length = maxlen) %>%
#   #layer_dropout(rate=0.2) %>%
#   #layer_flatten(.) %>%
#   layer_dense(400 , activation = "relu", input_shape = maxlen )  %>%
#   layer_dropout(0.2) %>%
#   layer_dense(400 , activation = "relu"  )  %>%
#   layer_dropout(0.2) %>%
#   layer_dense(5 , activation = "softmax" )  
# 

L1 = 0.01 
L2 = 0.0 
DO = 0.2
OP = "adamax" 
Node = 400

model <- keras_model_sequential() %>% 
	layer_dense(Node , activation = "relu", input_shape = maxlen,
	            kernel_regularizer = regularizer_l1_l2(l1 = L1, l2 = L2) )  %>%
              layer_dropout(DO) %>% 
  layer_dense(Node , activation = "relu", input_shape = maxlen,
              kernel_regularizer = regularizer_l1_l2(l1 = L1, l2 = L2) )  %>%
              layer_dropout(DO) %>%  
  layer_dense(Node , activation = "relu", input_shape = maxlen ,
              kernel_regularizer = regularizer_l1_l2(l1 = L1, l2 = L2))  %>%
              layer_dropout(DO) %>% 
  layer_dense(Node , activation = "relu", input_shape = maxlen,
              kernel_regularizer = regularizer_l1_l2(l1 = L1, l2 = L2))  %>%
              layer_dropout(DO) %>%
  layer_dense(5 , activation = "softmax" )  
 
 
 
# Compile model
model %>% compile(
  loss = "categorical_crossentropy",
  optimizer = OP,
  metrics = "accuracy"
)
 
# Training ----------------------------------------------------------------
 
FIT <- model %>%
  fit(
    x_train, (ytrain),
    batch_size = batch_size,
    epochs = epochs ,
    validation_data = list(x_test, (ytest ) )
  )
 
save_model_hdf5( model, paste0(toupper(OP), "do", DO*10,   "L1", L1*100 , "L2", L2*100,".h5" ) ) 

 

#  

