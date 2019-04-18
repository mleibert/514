

library(keras)
library(tensorflow)
sess = tf$Session()
hello <- tf$constant('Hello, TensorFlow!')
sess$run(hello)

amazon <- read.csv("reviews.csv")

one.hot <- function(Z){return(unname( as.matrix( 
  as.data.frame( t( model.matrix(~ as.factor(Z) + 0) ) ) ) )) }

testsamples <- sample( 1:nrow(amazon)   , ((nrow(amazon) )/ 2 ))
trainsamples <-  setdiff(  1:nrow(amazon)   , testsamples)
ytrain <- amazon[trainsamples,]$Score
ytest <- amazon[testsamples ,]$Score

length(testsamples ) == length(trainsamples )
samples <- amazon[,10]; rm(amazon)

max_features <- 1000
gc()
 

tokenizer <- text_tokenizer( num_words = max_features ) %>% 
  fit_text_tokenizer(samples)

sequences <- texts_to_sequences(tokenizer, samples)
maxlen = 70 

x <- pad_sequences(sequences, maxlen = maxlen)


ytrain <- t( one.hot(ytrain ) )
ytest <- t( one.hot(ytest) )

x_train <- x[trainsamples,]
x_test <- x[testsamples ,]
rm(x);gc()



batch_size <- 32
embedding_dims <- 50
filters <- 250
kernel_size <- 3
hidden_dims <- 250
epochs <- 5



maxlen <- dim(x_train)[2]

model <- keras_model_sequential()
model %>%
  layer_embedding(input_dim = 1000, output_dim = 128) %>% 
  layer_lstm(units = 300, dropout = 0.2, recurrent_dropout = 0.2) %>% 
  layer_lstm(units = 300, dropout = 0.2, recurrent_dropout = 0.2) %>% 
  layer_lstm(units = 300, dropout = 0.2, recurrent_dropout = 0.2) %>% 
  layer_lstm(units = 300, dropout = 0.2, recurrent_dropout = 0.2) %>% 
  layer_dense(units = 5, activation = 'softmax')



# Compile model
model %>% compile(
  loss = "categorical_crossentropy",
  optimizer = "adagrad",
  metrics = "accuracy"
)

# Training ----------------------------------------------------------------

model %>%
  fit(
    x_train, (ytrain),
    batch_size = batch_size,
    epochs = 5 ,
    validation_data = list(x_test, (ytest ) )
  )


## RNN's and embedding? What is pad sequences doing


model %>% save_model("asdagrad.h5")


