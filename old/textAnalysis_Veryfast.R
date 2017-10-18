
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
  cat('number of cols=', ncol(X))
  p2 = array(NA, c(3,2 , ncol(X)))
  posterior = vector(mode = "list", length = ncol(X))
  
  for (i in 1:ncol(X)){
    t = table(Y, X[,i] )
    for (j in 1:nrow(t)) t[j,]= t[j,]/sum(t[j,])
  
    if (i%%500 == 1) cat('\n calulating the posterior for col', i)
    posterior[[i]] = t;
    p2[,,i] =t
  }
  
  
  # calculate priors
  prior = rep(NA, nlevels(Y))
  for(i in 1:nlevels(Y)){
    prior[i] = sum(Y==levels(Y)[i])/length(Y)
  }
  # classifier = list(prior=prior, posterior=posterior)
  classifier = list(prior=prior, posterior=p2)
  
  save(classifier, file="posterios.RData")


  
} 
##########classify
classify <- function (prior, posterior, quary) {
 
  b = prior
  for (i in 1:length(quary)){
    b = b* posterior[,quary[i],i]
  }
  
  return(b)
}

NB_fast <- function(prior, posterior, Y, X, training_set, query){

  
  result = classify(prior, posterior, query)
  ind = which.max(result)
  return (levels(training_set[,Y])[ind])
 
  
}
cleanup <- function(text){
  docs <- Corpus(VectorSource(text))
  #cleaning the test
  
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
LOOCV_fast <- function(Y, X, data, n){
  
  if (!is.data.frame(data)) stop("NB can only handle data frames")
  
  #   a = data[,X]
  #   #make sure query is in the correct format
  #   for (i in 1:length(X)){
  #     
  #     b = levels(a[,i]) #loop over X colomns
  #     if (!query[i] %in% b) stop("Query dosnot match the model")
  #   }
  print("Running LOOCV")
  
  res = rep(NA, dim(data)[1])
  load("posterios.RData")
  
  cnt = 0;
  for (i in 1:dim(data)[1]){
    t0 = Sys.time()
    trainig_set = data[-i,]
    test = factor(as.character(data[i,X]))
    levels(test) = c(1,2)
    # res[i] = NB(formula, trainig_set, test);
    
    res[i] = NB_fast(classifier$prior, classifier$posterior, Y, X, trainig_set, test);
    if ( data[i,Y] == res[i]) cnt = cnt+1
    t1 = Sys.time() - t0;
    if (i%%100==1){
    cat("trial#", i, "time remaining = ", t1*(dim(data)[1]-i)/60, "min")
    print(i)
    }
    
  }
  # perf = cnt/dim(data)[1]
  perf = mean(data[,Y] == res)
  # f = paste(n[1], "r.RData")
  # save(res,file=f )
  cat('\n perf=', perf)
  save(res, perf,file="result.RData" )
  
  
}

#function sentiment
sentiment <-function(n, TRAIN){

  if (TRAIN==1){
    load("sentiment")
    data <- subset(r,select=c("sentiment","text"))
    text = data[n,2]
    TYPE = data[n,1]
    # text = data[,2]
    # TYPE = data[,1]
    docs = cleanup(text)
    
    #Build a term-document matrix
    dtm = TermDocumentMatrix(docs)
    #remove sparse terms
    dtm =removeSparseTerms(dtm, sparse=0.99)
    
    dtm = weightBin(dtm)
    m <- as.matrix(dtm)
    data = data.frame(t(m), TYPE)
    save(data, m, TYPE,file="sentiment_clean.RData")
    X = t(m)
    Y = as.factor(TYPE)
    train_fast(Y, X)#build the calssifier
  }
  else{
    load ("sentiment_clean.RData")
    X = colnames(data[,-ncol(data)])
    Y = "TYPE"
    a = LOOCV_fast(Y, X, data, n)
    return(a)
  }
 

}




sentiment(1:13871,0)