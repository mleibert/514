library(cloudml); setwd("G:\\math\\514\\cloud")
# gcloud_install(update = TRUE)
# gcloud_init()

cloudml_train("train.R", master_type = "standard_gpu", collect = T)

library(cloudml); setwd("G:\\math\\514")


load("fitlist.rdata")
require(ggplot2)
library("reshape2")

OPTdat <- data.frame(
  c(unname(unlist( OPT_List[[1]]$metrics )),unname(unlist( OPT_List[[2]]$metrics )),unname(unlist( OPT_List[[3]]$metrics ))),
  rep( c( rep("Acc", 15), rep("Loss",15), rep("Val_Acc", 15), rep("Val_loss", 15) ), 3 ),
  c( rep(OPT_List[[1]]$OPT,15), rep(OPT_List[[2]]$OPT,15) ,  rep(OPT_List[[3]]$OPT,15))
)

colnames(OPTdat) <- c("value","type", "opt")
OPTdat$epoch <- rep(1:15,3)
head(OPTdat)

optlist  <- list()
for( i in 1:3){
optdat <- do.call(rbind.data.frame, fitlist[[i]]$metrics)
optdat <- t(optdat); rownames(optdat) <- NULL
optdat <- as.data.frame(optdat); colnames(optdat) <- names(fitlist[[i]]$metrics)
optdat$epoch <- 1:10
optlist[[i]] <- optdat
}


optdat <- do.call(rbind,optlist)
optdat$opt <-  c( rep("adam",10), rep("rmsprop",10) ,  rep("adagrad",10))
# 
# test_data <-
#   data.frame(
#     var0 = 100 + c(0, cumsum(runif(49, -20, 20))),
#     var1 = 150 + c(0, cumsum(runif(49, -10, 10))),
#     date = seq(as.Date("2002-01-01"), by="1 month", length.out=100)
#   )





Met <- 1
ACC <- data.frame( rep(NA,nrow(optlist[[1]]) ),rep(NA,nrow(optlist[[1]]) ),rep(NA,nrow(optlist[[1]]) ) )

for(k in 1:3){ ACC[,k] <-  optlist[[k]][,Met] }
colnames(ACC) <- unique(c( rep("adam",10), rep("rmsprop",10) ,  rep("adagrad",10)))
ACC$epoch <- 1:nrow(ACC); 
ACC <- melt(ACC, id="epoch")  # convert to long format
colnames(ACC)[3] <- colnames(optlist[[1]])[Met]

ggplot(data=ACC, aes(x=epoch, y=ACC[,3], colour=variable)) +
  geom_line( lwd = 1) + ylab(colnames(ACC)[3]) +  geom_point()


