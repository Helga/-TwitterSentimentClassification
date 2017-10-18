
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
  posterior = vector(mode = "list", length = ncol(X))
  for (i in 1:ncol(X)){
    t = table(Y, X[,i] )
    for (j in 1:nrow(t)) t[j,]= t[j,]/sum(t[j,])
    # t[1,]= t[1,]/sum(t[1,])
    # t[2,]= t[2,]/sum(t[2,])
    if (i%%500 == 1) cat('\n calulating the posterior for col', i)
    posterior[[i]] = t;
  }
  
  
  # calculate priors
  prior = rep(NA, nlevels(Y))
  for(i in 1:nlevels(Y)){
    prior[i] = sum(Y==levels(Y)[i])/length(Y)
  }
  classifier = list(prior=prior, posterior=posterior)
  save(classifier, file="posterios.RData")
  
#   return(list(prior=c(prior_1, prior_2), posterior=posterior))
  #return(list(prior=c(prior_1, prior_2)))
  
} 
##########classify
classify <- function (classifier, quary) {
  post = classifier$prior

  for (i in 1:length(classifier$posterior)){
    ###################SS#S##S#3
    t = classifier$posterior[[i]]
    for (j in 1:nrow(t)){
      post[j] =post[j]*t[j,quary[i]]
    }
    # post_2 = post_2 * t[2,quary[i]]
    # post_1 = post_1 * t[1,quary[i]]
    # 
    
  }
  return(post)
}

NB_fast <- function(classifier, Y, X, training_set, query){

  
#   classifier <- train(training_set[,Y], training_set[,X])
  
  result = classify(classifier, query)
  ind = which.max(result)
  return (levels(training_set[,Y])[ind])
  # if (result$post_2 < result$post_1){
  #   #  cat(levels(training_set[,Y])[1], result$post_1)
  #   return (levels(training_set[,Y])[1])
  # }  else  {
  #   #   cat(levels(training_set[,Y])[2], result$post_2)
  #   return (levels(training_set[,Y])[2])
  #   
  # }
  
}
cleanup <- function(text){
  docs <- Corpus(VectorSource(text))
  #cleaning the test
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
  # remove URLs
  # removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
  # docs <- tm_map(docs, removeURL)
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
    test = as.character(data[i,X])
    # res[i] = NB(formula, trainig_set, test);
    
    res[i] = NB_fast(classifier, Y, X, trainig_set, test);
    # res = NB_fast(classifier, Y, X, trainig_set, test);
    if ( data[i,Y] == res[i]) cnt = cnt+1
    t1 = Sys.time() - t0;
    
    cat("trial#", i, "time remaining = ", t1*(dim(data)[1]-i)/60, "min")
    print(i)
    
    
  }
  # perf = cnt/dim(data)[1]
  perf = mean(data[,Y] == res)
  # f = paste(n[1], "r.RData")
  # save(res,file=f )
  cat('\n perf=', perf)
  
}

#function sentiment
sentiment <-function(n, TRAIN){
  # load("sentiment")
  # data <- subset(r,select=c("sentiment","text"))
  # text = data[n,2]
  # TYPE = data[n,1]
  # # text = data[,2]
  # # TYPE = data[,1]
  # docs = cleanup(text)
  # 
  # #Build a term-document matrix
  # dtm <- TermDocumentMatrix(docs)
  # dtm = weightBin(dtm)
  # m <- as.matrix(dtm)
  # data = data.frame(t(m), TYPE)
  # save(data, m, TYPE,file="sentiment_clean.RData")
  # # for (i in 1:(dim(data)[2]-1)){
  # #   levels(data)<-c("0", "1")
  # # }
  # 
  
  load ("sentiment_clean.RData")
  if (TRAIN==1){
    X = t(m)
    Y = as.factor(TYPE)
    train_fast(Y, X)#build the calssifier
  }
  else{
    X = colnames(data[,-ncol(data)])
    Y = "TYPE"
    a = LOOCV_fast(Y, X, data, n)
    return(a)
  }
 # 
   
# #

}


######Build table 
# load("sentiment")
# data <- subset(r,select=c("sentiment","text"))
# data = data[1:10,]
# X = colnames(data[,-ncol(data)])
# Y = "TYPE"
# C = train(Y, X)
# sentiment(1:13871,1)

# sentiment(1:13871,0)