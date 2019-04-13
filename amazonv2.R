max_features <- 10000
maxlen <- 70
 
samples <- amazon[,10]

tokenizer <- text_tokenizer( num_words = 400  ) %>% fit_text_tokenizer(samples)

#sequences <- texts_to_sequences(tokenizer, samples)

oh_results <- texts_to_matrix(tokenizer,samples, mode = "tfidf")
beep("coin")

dim(oh_results)
 
testsamples <- sample( 1:(dim(oh_results)[1]) , (dim(oh_results)[1])/ 2 )
trainsamples <-  setdiff(  1:(dim(oh_results)[1])  , testsamples)
length(testsamples ) == length(trainsamples )

xtrain <- oh_results[trainsamples,]
xtest <- oh_results[testsamples ,]
ytrain <- amazon[trainsamples,]$Score
ytest <- amazon[testsamples ,]$Score

ytrain <- t( one.hot(ytrain ) )

ytest <- t( one.hot(ytest) )
 

max_features <- 400		#amount of total words
maxlen <- 200
batch_size <- 32
embedding_dims <- 50
filters <- 250
kernel_size <- 3
hidden_dims <- 250
epochs <- 2



x_train <- xtrain %>%
  pad_sequences(maxlen = maxlen)
#beep("coin")

x_test <- xtest %>%
 pad_sequences(maxlen = maxlen)
beep("coin")

gc()



model <- keras_model_sequential() %>% 
	layer_dense(400 , activation = "relu", input_shape = c(200) )  %>%
		layer_dropout(0.2) %>%
 	layer_dense(400 , activation = "relu" )  %>%
		layer_dropout(0.2) %>%
	layer_dense(5 , activation = "softmax" )  


# Compile model
model %>% compile(
  loss = "categorical_crossentropy",
  optimizer = "adam",
  metrics = "accuracy"
)


model %>%
  fit(
    x_train, t(ytrain),
    batch_size = batch_size,
    epochs = 10 ,
    validation_data = list(x_test, t(ytest ) )
  )









