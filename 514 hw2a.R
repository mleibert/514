set.seed(123)
gamma <- .05
dat  <- perceptron.box.data( 100, gamma ) 

results <- train.perceptron( dat[,-3 ]  , dat[,3] ) 
vanilla( dat[,-3 ]  , dat[,3] ) 

newdat  <- perceptron.box.data(100,gamma)

predict.perceptron(results[[1]]  ,results[[2]], newdat[,-3]  ) 
 