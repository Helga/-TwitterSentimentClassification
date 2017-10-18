
# install.packages("tm")  # for text mining
# install.package("SnowballC") # for text stemming
# install.packages("RColorBrewer") # color palettes
# install.packages("DescTools")
# Load
setwd("C:/Users/hemaz/Google Drive/Fall_2016/PSYCH 625/Project2")

library("tm")
# library("wordcloud")
library("RColorBrewer")
library("DescTools")

train_fast <- function (Y, X) {
  print("Training the classifier")
  prob = array(NA, c(nlevels(Y),nlevels(as.factor(X)) , ncol(X)))
  eps = 1/ncol(X)
  for (i in 1:ncol(X)){
    t = table(Y, X[,i] )
    t = t+eps # add-one smoothing
    for (j in 1:nrow(t)) t[j,]= t[j,]/sum(t[j,])
    prob[,,i] =t
  }
  
  # calculate priors
  prior = rep(NA, nlevels(Y))
  for(i in 1:nlevels(Y)){
    prior[i] = sum(Y==levels(Y)[i])/length(Y)
  }
  classifier = list(prior=prior, posterior=prob)
  return(classifier)
} 

#####################
NB_fast <- function(classifer, Y, X, training_set, query){
  #classify
  prob = classifer$prior
  for (i in 1:length(query)){
    prob = prob* classifer$posterior[,query[i],i]
  }
  ind = which.max(prob)
  return (levels(training_set[,Y])[ind])
}
###########
cleanup <- function(text){
  docs <- Corpus(VectorSource(text))
  # Twitter tags
  tt<-function(x) gsub("RT |via", "", x)
  docs<- tm_map(docs, content_transformer(tt))
  
  # Twitter Usernames
  tun<-function(x) gsub("(^|[^@\\w])@(\\w{1,15})\\b", "", x)
  docs<- tm_map(docs, content_transformer(tun))
  
  
  # URLs 
  urlPat<-function(x) gsub("(ftp|http)(s?)://.*\\b", "", x)
  docs <- tm_map(docs, content_transformer(urlPat))
  
  # Convert the text to lower case
  docs <- tm_map(docs, content_transformer(tolower))
  
  # Remove numbers
  docs <- tm_map(docs, removeNumbers)
  # Remove english common stopwords
  docs <- tm_map(docs, removeWords, stopwords("english"))
  # Remove your own stop word
  # specify your stopwords as a character vector
  docs <- tm_map(docs, removeWords, c("blabla1", "blabla2")) 
  # Remove punctuations
  docs <- tm_map(docs, removePunctuation)
  # Eliminate extra white spaces
  docs <- tm_map(docs, stripWhitespace)
  
  return(docs)
}
LOOCV_fast <- function(classifier, Y, X, data, n){
  
  if (!is.data.frame(data)) stop("NB can only handle data frames")
  print("Running LOOCV")
  
  res = rep(NA, dim(data)[1])

  for (i in 1:dim(data)[1]){
    t0 = Sys.time()
    trainig_set = data[-i,]
    test = factor(as.character(data[i,X]))
    levels(test) = c(1,2)
    res[i] = NB_fast(classifier, Y, X, trainig_set, test);
    t1 = Sys.time() - t0;
    if (i%%1000==1){
      cat("trial#", i, "time remaining = ", t1*(dim(data)[1]-i)/60, "min")
      print(i)
    }
    
  }
  perf = mean(data[,Y] == res)
  cat('\n perf=', perf)

}
#################
sentiment <-function(data){
   
    text = data[,2]
    TYPE = data[,1]
    docs = cleanup(text)
    
    #Build a term-document matrix
    dtm = TermDocumentMatrix(docs)
    #remove sparse terms
    dtm =removeSparseTerms(dtm, sparse=0.99)
    
    dtm = weightBin(dtm)
    m <- as.matrix(dtm)
    data = data.frame(t(m), TYPE)
    #build the calssifier
    classifier = train_fast(as.factor(TYPE), t(m))
    X = colnames(data[,-ncol(data)])
    Y = "TYPE"
    a = LOOCV_fast(classifier, Y, X, data, n)
    
}

load("sentiment")
data <- subset(r,select=c("sentiment","text"))
sentiment(data)
sentiment(data)

DT <- as.data.frame(
  lapply(subset(r, candidate=="Donald Trump"),
         function(x) if(is.factor(x)) factor(x) else x
  )
)
data <- subset(DT,select=c("sentiment","text"))
sentiment(data)


JK <- as.data.frame(
  lapply(subset(r, candidate=="John Kasich"),
         function(x) if(is.factor(x)) factor(x) else x
  )
)
data <- subset(JK,select=c("sentiment","text"))
sentiment(data)
