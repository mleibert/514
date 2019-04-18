library(cloudml); setwd("G:\\math\\514\\cloud")
# gcloud_install(update = TRUE)
# gcloud_init()

cloudml_train("train.R", master_type = "standard_gpu", collect = T)

library(cloudml); 
setwd("G:\\math\\514")


load("fitlists.rdata");fitlists <- fitlist
 load("fitlist.rdata")
 length(fitlist);length(fitlists)
for( i in 1:length(fitlists) ){ fitlist[[i+length(fitlists)]] <- fitlists[[i]] }

require(ggplot2)
library("reshape2")
library(gridExtra)
# 
# OPTdat <- data.frame(
#   c(unname(unlist( OPT_List[[1]]$metrics )),unname(unlist( OPT_List[[2]]$metrics )),unname(unlist( OPT_List[[3]]$metrics ))),
#   rep( c( rep("Acc", 15), rep("Loss",15), rep("Val_Acc", 15), rep("Val_loss", 15) ), 3 ),
#   c( rep(OPT_List[[1]]$OPT,15), rep(OPT_List[[2]]$OPT,15) ,  rep(OPT_List[[3]]$OPT,15))
# )
# 
# colnames(OPTdat) <- c("value","type", "opt")
# OPTdat$epoch <- rep(1:15,3)
# head(OPTdat)
# 
opts <- c( rep("adam",1), rep("rmsprop",1) ,  rep("adagrad",1))
for( i in 4:length(fitlist)){opts[i] <- fitlist[[i]]$OPT}
#   
optlist  <- list()
for( i in 1:length(fitlist)){
optdat <- do.call(rbind.data.frame, fitlist[[i]]$metrics)
optdat <- t(optdat); rownames(optdat) <- NULL
optdat <- as.data.frame(optdat); colnames(optdat) <- names(fitlist[[i]]$metrics)
optdat$epoch <- 1:10
optlist[[i]] <- optdat
}


optdat <- do.call(rbind,optlist)
 # 
# test_data <-
#   data.frame(
#     var0 = 100 + c(0, cumsum(runif(49, -20, 20))),
#     var1 = 150 + c(0, cumsum(runif(49, -10, 10))),
#     date = seq(as.Date("2002-01-01"), by="1 month", length.out=100)
#   )

source("multiplot.R")
piclist <- ACCs <- list()

titles <- c("Accuracy", "Loss", "Validation Accuracy", "Validation Loss")

for( j in 1:4){
Met <- j
ACC <- data.frame( rep(NA,nrow(optlist[[1]]) ) )

for(k in 1:length(fitlist)){ ACC[,k] <-  optlist[[k]][,Met] }
colnames(ACC) <- opts
ACC$epoch <- 1:nrow(ACC); 
ACC <- melt(ACC, id="epoch")  # convert to long format
colnames(ACC)[3] <- colnames(optlist[[1]])[Met]
ACC[,2] <- as.character( ACC[,2]); ACC[,2] <- paste0( ACC[,2], "  ")
ACCs[[j]] <- ACC
 


} 


j = 1
p1 <- ggplot(data=ACCs[[j]], aes(x=epoch, y=ACCs[[j]][,3], colour=variable)) +
  geom_line( lwd = 1) + ylab(colnames(ACC)[3]) +  geom_point() +
  ggtitle( titles[j] ) + guides(col = guide_legend(ncol = 6)) +  
  theme(plot.title = element_text(hjust = 0.5), legend.position="bottom", axis.title.x=element_blank(),
        legend.title = element_blank())
j = 2
p2 <- ggplot(data=ACCs[[j]], aes(x=epoch, y=ACCs[[j]][,3], colour=variable)) +
  geom_line( lwd = 1) + ylab(colnames(ACC)[3]) +  geom_point() +
  ggtitle( titles[j] ) + guides(col = guide_legend(ncol = 6)) +  
  theme(plot.title = element_text(hjust = 0.5), legend.position="bottom", axis.title.x=element_blank(),
        legend.title = element_blank())
j = 3
p3 <- ggplot(data=ACCs[[j]], aes(x=epoch, y=ACCs[[j]][,3], colour=variable)) +
  geom_line( lwd = 1) + ylab(colnames(ACC)[3]) +  geom_point() +
  ggtitle( titles[j] ) + guides(col = guide_legend(ncol = 6)) +  
  theme(plot.title = element_text(hjust = 0.5), legend.position="bottom", axis.title.x=element_blank(),
        legend.title = element_blank())
j = 4
p4 <- ggplot(data=ACCs[[j]], aes(x=epoch, y=ACCs[[j]][,3], colour=variable)) +
  geom_line( lwd = 1) + ylab(colnames(ACC)[3]) +  geom_point() +
  ggtitle( titles[j] ) + guides(col = guide_legend(ncol = 6)) +  
  theme(plot.title = element_text(hjust = 0.5), legend.position="bottom", axis.title.x=element_blank(),
        legend.title = element_blank())




#extract legend
#https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend<-g_legend(piclist[[1]])

grid.arrange(arrangeGrob(p1  + theme(legend.position="none"),
                         p2  + theme(legend.position="none"),
                         p3  + theme(legend.position="none"),
                         p4  + theme(legend.position="none"),
                               nrow=2),
                   mylegend, nrow=2,heights=c(10, 1))





