

#############################################
##Raw data EDA
#############################################
nrow(amazon)
length(unique(amazon$ProductId))
length(unique(amazon$UserId))
length(unique(amazon$ProfileName))

#Example of rating 1 review
amazon[which(amazon$Score==1)[1],c(7,9,10)]

#Example of rating 2 review
amazon[which(amazon$Score==2)[1],c(7,9,10)]

#Example of rating 3 review
amazon[which(amazon$Score==3)[1],c(7,9,10)]

#Example of rating 4 review
amazon[which(amazon$Score==4)[1],c(7,9,10)]

#Example of rating 5 review
amazon[which(amazon$Score==5)[1],c(7,9,10)]

#Distribution of ratings
library(ggplot2)
ggplot(amazon, aes(x=Score)) + geom_histogram(color="lightblue", fill="lightblue", binwidth = 0.5) + labs(title="Distribution of Ratings", x="Rating")

#Max. number of words in a single full text / summary
library(ngram)

#summary text
wc_distrib.summary <- lapply(as.character(amazon$Summary), function(x) wordcount(x)) 
summary(unlist(wc_distrib.summary))

#full text
wc_distrib.full <- lapply(as.character(amazon$Text), function(x) wordcount(x)) 
summary(unlist(wc_distrib.full))

wc_distrib.df <- data.frame(wordcount=c(unlist(wc_distrib.summary),unlist(wc_distrib.full)), 
                            text_type=c(rep('Summary',length(unlist(wc_distrib.summary))), rep('Full',length(unlist(wc_distrib.summary)))))

ggplot(wc_distrib.df,  aes(x=wordcount, col=text_type)) + geom_histogram(fill="white", position="dodge",binwidth=5) + xlim(0,200) + ylim(0,60000) + labs(title="Distribution of Word Count", x="Word Count/Review", y="Number of Reviews")

################################################
#Randomly drop observations with ratings 4 and 5 -- skipping this step for now
################################################
#set.seed(34567)

#Determine the number of reviews for each category
#length(which(amazon$Score==1))
#length(which(amazon$Score==2))
#length(which(amazon$Score==3))
#length(which(amazon$Score==4))
#length(which(amazon$Score==5))

#rating_5_newsize <- floor(runif(1,length(which(amazon$Score==3)), length(which(amazon$Score==1))))
#rating_4_newsize <- floor(runif(1,length(which(amazon$Score==3)), length(which(amazon$Score==1))))

#rating_5_sample_index <- sample(which(amazon$Score==5), rating_5_newsize, replace=FALSE)
#rating_5_sample <- amazon[rating_5_sample_index,]

#rating_4_sample_index <- sample(which(amazon$Score==4), rating_4_newsize, replace=FALSE)
#rating_4_sample <- amazon[rating_4_sample_index,]

#new_sample_index <- c(which(amazon$Score==1), which(amazon$Score==2),which(amazon$Score==3), rating_4_sample_index, rating_5_sample_index)
#amazon.new_sample_index <- sample(new_sample_index,length(new_sample_index), replace=FALSE)
#amazon.new_sample <- amazon[amazon.new_sample_index,]

#ggplot(amazon.new_sample, aes(x=Score)) + geom_histogram(color="lightblue", fill="lightblue", binwidth = 0.5) + labs(title="Distribution of Ratings (new sample)", x="Rating")

########################################
#Parametres
########################################
max_features <- 1000        #total number of words allowed per review
maxlen <- 200

############################################
##Data Cleaning
############################################
require(keras) 
library(tm) 
library(textclean)
library(SnowballC)
library(lattice)

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

tokenizer.summary <- text_tokenizer(num_words = max_features) %>% 
  fit_text_tokenizer(summary.clean)

vocab <- tokenizer.summary$word_counts
barchart(sort(unlist(vocab), decreasing=TRUE)[20:1], col='lightblue', xlab="Term Frequency", main="Most frequently appearing words")

#Full text data
fulltxt.clean <- amazon$Text %>%
  tolower(.) %>%
  removePunctuation(.,preserve_intra_word_contractions=TRUE, preserve_intra_word_dashes=TRUE) %>%
  removeNumbers(.) %>%
  stripWhitespace(.) %>%
  replace_contraction(.) %>%
  removeWords(., custom_stopwords) %>%
  stemDocument(.)

###############Do if have time, but deprioritize for now b/c have to implement lemmatization in place of stemming ########################################
#Count of positive < negative, positive = negative, positive > negative words in ratings 4+5
#Count of positive < negative, positive = negative, positive > negative words in rating = 3
#library(tidyverse)
#library(tidytext)
#library(glue)
#library(stringr)

#Count of positive < negative, positive = negative, positive > negative words in ratings 1+2

###############################################################################################

#################Save the dataset#########################
clean_dta <- data.frame(summary=summary.clean, fulltxt = fulltxt.clean, rating=amazon$Score)
head(clean_dta)
save(clean_dta, file = "amazon_clean_dta.RData")