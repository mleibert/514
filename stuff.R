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



p1 <- ggplot(data=ACCs[[1]], aes(x=epoch, y=ACCs[[1]][,3], colour=variable)) +
  geom_line( lwd = 1) + ylab(colnames(ACC)[3]) +  geom_point() +
  ggtitle( titles[1] ) + guides(col = guide_legend(ncol = 6)) +  
  theme(plot.title = element_text(hjust = 0.5), legend.position="bottom", axis.title.x=element_blank(),
        legend.title = element_blank(), legend.text=element_text(size=14))

p2 <- ggplot(data=ACCs[[2]], aes(x=epoch, y=ACCs[[2]][,3], colour=variable)) +
  geom_line( lwd = 1) + ylab(colnames(ACC)[3]) +  geom_point() +
  ggtitle( titles[2] ) + guides(col = guide_legend(ncol = 6)) +  
  theme(plot.title = element_text(hjust = 0.5), legend.position="bottom", axis.title.x=element_blank(),
        legend.title = element_blank())

p3 <- ggplot(data=ACCs[[3]], aes(x=epoch, y=ACCs[[3]][,3], colour=variable)) +
  geom_line( lwd = 1) + ylab(colnames(ACC)[3]) +  geom_point() +
  ggtitle( titles[3] ) + guides(col = guide_legend(ncol = 6)) +  
  theme(plot.title = element_text(hjust = 0.5), legend.position="bottom", axis.title.x=element_blank(),
        legend.title = element_blank())

p4 <- ggplot(data=ACCs[[4]], aes(x=epoch, y=ACCs[[4]][,3], colour=variable)) +
  geom_line( lwd = 1) + ylab(colnames(ACC)[3]) +  geom_point() +
  ggtitle( titles[4] ) + guides(col = guide_legend(ncol = 6)) +  
  theme(plot.title = element_text(hjust = 0.5), legend.position="bottom", axis.title.x=element_blank(),
        legend.title = element_blank())




#extract legend
#https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend<-g_legend(p1)

grid.arrange(arrangeGrob(p1  + theme(legend.position="none"),
                         p2  + theme(legend.position="none"),
                         p3  + theme(legend.position="none"),
                         p4  + theme(legend.position="none"),
                               nrow=2),
                   mylegend, nrow=2,heights=c(10, 1))






p1 <- ggplot(data=ACCs[[1]], aes(x=epoch, y=ACCs[[1]][,3], colour=variable)) +
  geom_line( lwd = 1) + ylab(colnames(ACCs[[1]])[3]) +  geom_point() +
  ggtitle( titles[1] ) # + guides(col = guide_legend(ncol = 6)) +  
  # theme(plot.title = element_text(hjust = 0.5), legend.position="none", axis.title.x=element_blank(),
  #       legend.title = element_blank(), legend.text=element_text(size=14))  

p2 <- ggplot(data=ACCs[[2]], aes(x=epoch, y=ACCs[[2]][,3], colour=variable)) +
  geom_line( lwd = 1) + ylab(colnames(ACCs[[2]])[3]) +  geom_point() +
  ggtitle( titles[2] ) # + guides(col = guide_legend(ncol = 6)) +  
  # theme(plot.title = element_text(hjust = 0.5), legend.position="none", axis.title.x=element_blank(),
  #       legend.title = element_blank())


p3 <- ggplot(data=ACCs[[3]], aes(x=epoch, y=ACCs[[3]][,3], colour=variable)) +
  geom_line( lwd = 1) + ylab(colnames(ACCs[[3]])[3]) +  geom_point() +
  ggtitle( titles[3] )# + guides(col = guide_legend(ncol = 6)) +  
  # theme(plot.title = element_text(hjust = 0.5), legend.position="none", axis.title.x=element_blank(),
  #       legend.title = element_blank())  
 
p4 <- ggplot(data=ACCs[[4]], aes(x=epoch, y=ACCs[[4]][,3], colour=variable)) +
  geom_line( lwd = 1) + ylab(colnames(ACCs[[4]])[3]) +  geom_point() +
  ggtitle( titles[4] )  # +  
  # theme(plot.title = element_text(hjust = 0.5), legend.position="bottom", axis.title.x=element_blank(),
  #       legend.title = element_blank())+ guides(col = guide_legend(ncol = 6))


p1 <- ggplotly(p1, legendgroup = ~x, showlegend = F )
p2 <- ggplotly(p2, legendgroup = ~x, showlegend = F)
p3 <- ggplotly(p3, legendgroup = ~x, showlegend = F)
p4 <- ggplotly(p4, legendgroup = ~x, showlegend = F)  

subplot(p1, p2, p3, p4,  nrows = 2)

datz <- data.frame(x = c("a","b","c"), y = c(2,3,2), y2 = c(4,2,4))

p1 <- plot_ly(datz, type = 'bar', x = ~x, y = ~y, color = ~x, legendgroup = ~x)
p2 <- plot_ly(datz, type = 'bar', x = ~x, y = ~y2, color =  ~x, legendgroup = ~x, showlegend = F)
subplot(p1, p2, nrows = 2)

######################################################################################################
######################################################################################################


fit <- load_model_hdf5("GOODRESULT.h5")

names(fit)
fit$summary

##########################################################################################
################################################################################################
#############################################################################################


setwd("G:\\math\\514")
library(plotly)
library(ggplot2)
library(shiny)

preds <- read.csv("preds.csv" , header = T)
preds <- round(preds,5)
head( preds) 

predlist <- list()
for( i in 1:5){
  predlist[[i]] <- preds[ which(preds$y == i ),  ]
  predlist[[i]]$yhat <- apply( predlist[[i]][,-ncol(predlist[[i]])], 
                               1, which.max )
  predlist[[i]]$good <- predlist[[i]]$y == predlist[[i]]$yhat
}


head( predlist[[i]] )

sapply( predlist, function(X)  sum(X$good)  / nrow(X) )


mat <- matrix(NA , 5*5 , 3 ) 
mat[ , 1] <-  ( sort(   rep( 1:5 , 5) )  )
mat[ , 2] <-  (  (   rep( 1:5 , 5) )  )


for( i in 1:5){
  dat <- predlist[[i]]
  mat[ which(mat[,1] == i ) ,3  ] <- 
    table(  dat$yhat ) / nrow(dat)  }

mat <- as.data.frame(mat)
colnames(mat) <- c("y","yhat","pi")
mat[ which(mat[,1] == 3 ) ,3  ] 
mat[,3] <- round(mat[,ncol(mat)],3) 
mat <- as.data.frame(mat)


p<- ggplot(data = mat, aes(x = yhat, y = y, label =  pi )) +
  geom_tile(aes(fill = pi), color = "gray") + 
  scale_fill_gradient(low="yellow", high="red" ) +
  geom_text( size = 3 )




p <- ggplotly(p)
p

revs <- sapply(strsplit(amazon[,10], " "), length)
sumrys <- sapply(strsplit( amazon$Summary , " "), length)
RS <- data.frame(revs,sumrys)

ggplot(RS, aes(x=revs)) + geom_histogram( col="red"  , fill = NA , bins = 100) + xlim(0,200) +
  geom_histogram(  aes(x=sumrys), col="blue"  , fill = NA , bins = 100 ) + xlim(0,200) 
  
ggplot(RS, aes(x=sumrys)) + geom_histogram( col="red"  , fill = NA , bins = 100) +
  geom_histogram(  aes(x=revs), col="blue"  , fill = NA , bins = 100 ) + xlim(0,200) 


####


##########################################################################################
################################################################################################
#############################################################################################

library(stringr)
twitter <- read.csv("twitter.csv",header = F)
library(beepr); beep("coin")
head(twitter)
twitter <- twitter[,c(1,6)]
# write.csv(twitter, "twitter.csv" , row.names = F)
clean_tweet = gsub("&amp", "", twitter[,ncol(twitter)])
clean_tweet = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", clean_tweet)
clean_tweet = gsub("@\\w+", "", clean_tweet)
clean_tweet = gsub("[[:punct:]]", "", clean_tweet)
clean_tweet = gsub("[[:digit:]]", "", clean_tweet)
clean_tweet = gsub("http\\w+", "", clean_tweet)
clean_tweet = gsub("^\\s+|\\s+$", "", clean_tweet) 
clean_tweet = gsub( "  ", " ", clean_tweet  )
 
head(clean_tweet)

twitter[,ncol(twitter)] <- clean_tweet
head(twitters)

table(twitters[,1])
twitters <- twitter[ sample(1:nrow(twitter), 20000),]
twitters[which(twitters[,1] == 4),1] <- 1
head(twitters)




max_features <- 50
gc()

tokenizer <- text_tokenizer( num_words = max_features   ) %>%
  fit_text_tokenizer(twitters$V6)

tweets <- texts_to_matrix(tokenizer,twitters$V6, mode = "tfidf")

model <- load_model_hdf5("binary.h5")

model %>% predict(model )


 ##################

require(keras) 
library(tm) 
library(textclean)
library(SnowballC)
library(lattice)
max_features = 100
custom_stopwords <- tm::stopwords("english")[-which(tm::stopwords("english")=="not" | tm::stopwords("english")=="should" | tm::stopwords("english")=="against"|tm::stopwords("english")=="below"|tm::stopwords("english")=="above"|tm::stopwords("english")=="again"|tm::stopwords("english")=="few"|tm::stopwords("english")=="most")]

summary.clean <- amazon$Summary %>%
  tolower(.) %>%
  removePunctuation(.,preserve_intra_word_contractions=TRUE, preserve_intra_word_dashes=TRUE) %>%
  removeNumbers(.) %>%
  stripWhitespace(.) %>%
  replace_contraction(.) %>%
  removeWords(., custom_stopwords) %>%
  stemDocument(.)

head(summary.clean)
require(beepr);beep("coin")
tokenizer.summary <- text_tokenizer(num_words = max_features) %>% 
  fit_text_tokenizer(summary.clean)

vocab <- tokenizer.summary$word_counts
 beep("coin")

barchart(sort(unlist(vocab), decreasing=TRUE)[20:1], col='lightblue', xlab="Term Frequency", main="Most frequently appearing words")

