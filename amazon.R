library(text2vec)

amazon <- read.csv("reviews.csv")
amazon$Text <- as.character(amazon$Text )
head(amazon)

data("movie_review")
train_ids = sample(movie_review$id, 4000)
 train = movie_review[J(train_ids)]




it_train = itoken(amazon$Text, 
             preprocessor = tolower, 
             tokenizer = word_tokenizer, 
             ids = amazon$Id, 
             progressbar = T)
vocab = create_vocabulary(it_train)

#mylist <- list()
#mylist[[ nrow(amazon) + 1 ]] <- rep(NA, nrow(amazon))

system.time(
for( i in 15136:(nrow(amazon)-100000) ){
	a <- strsplit(gsub("[^[:alnum:] ]", "", amazon[i,10]), " +")[[1]] 
	a <-  match(a , vocab[,1] )
	mylist[[i]] <- a[!is.na( a )]
	mylist[[nrow(amazon) + 1 ]][i]  <- amazon[i, ]$Score
}
)
 
 
  