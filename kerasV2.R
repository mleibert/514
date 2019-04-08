library(keras)
require(beepr)
use_condaenv('r-tensorflow',required=TRUE)
#   install_keras( method = "conda", tensorflow = "gpu"); beep("coin")
library(reticulate)
conda_list()

reticulate::use_condaenv("r-tensorflow")

k = backend()

sess = k$get_session()
sess$list_devices() 
 
 

#https://www.youtube.com/watch?v=Rmjp1yFi9Ok
setwd("G:\\math\\514")
require(mlbench)
spirals <- spiralpred <- mlbench.spirals(75,1.5,.07)

y <- as.numeric(spirals$classes) - 1
x <- t(spirals$x )


#### C:\Program Files\NVIDIA Corporation\NVSMI
#https://stackoverflow.com/questions/52143756/how-to-force-keras-with-tensorflow-to-use-the-gpu-in-r



nh=30; lr0=1;
input_layer <- layer_input(shape = 2, name = 'input')
hidden_layer<- layer_dense(input_layer,units = nh,
activation = 'tanh',
bias_initializer=
initializer_random_uniform(
minval = -0.1, maxval = 0.1,
seed = 104),
kernel_initializer=
initializer_random_normal(
mean=0,stddev=.1, seed = 104))
output_layer <- layer_dense(hidden_layer ,units = 1,
activation='sigmoid',
bias_initializer=
initializer_random_uniform(
minval = -0.1, maxval = 0.1,
seed = 104),
kernel_initializer=
initializer_random_normal
(mean=0,stddev=.1, seed = 104))

model_logit <- keras_model(inputs = input_layer,
outputs = output_layer )

opt <- optimizer_sgd(lr = lr0,momentum=0)
compile(model_logit,
optimizer = opt,
loss = "binary_crossentropy",
metrics = c("acc")
)
 summary(model_logit)

results_binary=fit(model_logit,t(x),y,
	epochs=10000,verbose=0,batch_size = 75)
require(beepr)
beep("coin")
plot(results_binary)

