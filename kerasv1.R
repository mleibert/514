library(keras)
use_condaenv('r-tensorflow',required=TRUE)

library(reticulate)
conda_list() 
reticulate::use_condaenv(conda_list()[1,1])

reticulate::use_condaenv(conda_list()[2,1])
mtrain <- read.csv("G:\\math\\mnist_train.csv" , header = F)

library(tensorflow)

 sess = tf$Session()





x <- unname( mtrain[,-1] )
x <- t(x)
x <- x / 255
y <-  as.matrix( unname(  mtrain[,1] ))

 Y=to_categorical(y,10)
testX=x/255.
testY=y

nh=30;lr=.15;
input_layer <- layer_input(shape = 784, name = 'input')
hidden_layer<- layer_dense(input_layer,units = nh,
activation = 'tanh',
bias_initializer=
initializer_random_uniform(
minval = -0.1, maxval = 0.1, seed = 104),
kernel_initializer=
initializer_random_normal(
mean=0,stddev=.1,
seed = 104))

output_layer <- layer_dense(hidden_layer ,units = 10,
activation='sigmoid',
bias_initializer=
initializer_random_uniform(
minval = -0.1, maxval = 0.1,
seed = 104),
kernel_initializer=
initializer_random_normal(
mean=0,stddev=.1,
seed = 104))
model=keras_model(
inputs = input_layer, outputs = output_layer )
opt <- optimizer_sgd(lr = .5,momentum=0)
compile(model_logit,
optimizer = opt,
loss = "categorical_crossentropy",
metrics = c("acc")
)



lr=lr0=.1
nh=30
input_layer <- layer_input(shape = 784, name = 'input')
hidden_layer<- layer_dense(input_layer,name='h1',
	units = nh, activation = 'relu' )
 kernel_initializer=initializer_random_normal(mean=0,stddev=.1, seed = 104)

hidden_layer2<- layer_dropout( layer_dense(
hidden_layer,name='h2',
units = 1, activation = 'softmax'
), rate=.4,name='h3')



output_layer <-layer_dense(hidden_layer ,
units = 10,
activation='softmax',
bias_initializer=
initializer_random_uniform(
minval = -0.1, maxval = 0.1,
seed = 104),
kernel_initializer=
initializer_random_normal(
mean=0,stddev=.1, seed = 104))
model_2h <- keras_model(inputs = input_layer,
outputs = output_layer )

opt <- optimizer_sgd(lr = lr0,momentum=0)
compile(model_2h,
optimizer = opt,
loss = "categorical_crossentropy",
metrics = c("acc")
) 

summary(model_2h)
 
results_2h=  system.time(
fit(model_2h, t(x),Y,epochs=20,verbose=0,batch_size = 128) )
beep("coin")
results_2h

