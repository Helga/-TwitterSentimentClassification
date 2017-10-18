# 
# formula <- party ~ .
# LOOCV(formula,mydata)
# install.packages("tm")  # for text mining
# install.package("SnowballC") # for text stemming
# install.packages("wordcloud") # word-cloud generator 
# install.packages("RColorBrewer") # color palettes
# Load
# setwd("C:/Users/hemaz/Google Drive/Fall_2016/PSYCH 625/Project2")

library("tm")
library("DescTools")

train <- function (Y, X) {
  posterior = vector(mode = "list", length = ncol(X))
  
  for (i in 1:ncol(X)){
    t = table(Y, X[,i] )
    for (j in 1:nrow(t)) t[j,]= t[j,]/sum(t[j,])
    
    posterior[[i]] = t;
  }
  
  # calculate priors
  prior = rep(NA, nlevels(Y))
  for(i in 1:nlevels(Y)){
    prior[i] = sum(Y==levels(Y)[i])/length(Y)
  }
  
  return(classifier = list(prior=prior, posterior=posterior))
} 
##########classify
classify <- function (classifier, query) {
  prob = classifier$prior
  for (i in 1:length(query)){
    t = classifier$posterior[[i]]
    prob = prob* t[,query[i]]
  }
  return(prob)
  
  
}
NB <- function(formula, training_set, query){
  if (!is.data.frame(training_set)) stop("NB can only handle data frames")
  if (!is.vector(query)) stop ("Query needs to be a vector")
  
  f1.parsed <- ParseFormula(formula,data=training_set[1,])
  Y <- f1.parsed$lhs$vars
  X <- f1.parsed$rhs$vars
  a = training_set[,X]
  #make sure query is in the correct format
  for (i in 1:length(X)){
    
    b = levels(a[,i]) #loop over X colomns
    if (!query[i] %in% b) stop("Query does not match the model")
  }
  classifier <- train(training_set[,Y], training_set[,X])
  
  prob = classify(classifier, query)
  ind = which.max(prob)
  cat(levels(training_set[,Y])[ind], prob[ind], '\n')

}
data <-read.csv('Tennis.csv')
data = data[,-1]
formula <- Play ~ Outlook + Temperature + Humidity + Wind
query <- c("Sunny", "Cool", "High", "Strong")
NB(formula, data, query)


query <- c("Sunny", "Cool", "Normal", "Weak")
NB(formula, data, query)
# 
# query <- c("Hot", "Cool", "Normal", "Weak")
# NB(formula, data, query)


# NB_fast <- function(Y, X, training_set, query){
#   if (!is.data.frame(training_set)) stop("NB can only handle data frames")
#   if (!is.vector(query)) stop ("Query needs to be a vector")
#   
#   a = training_set[,X]
#   #make sure query is in the correct format
#   for (i in 1:length(X)){
#     
#     b = levels(a[,i]) #loop over X colomns
#     if (!query[i] %in% b) stop("Query dosnot match the model")
#   }
#   classifier <- train(training_set[,Y], training_set[,X])
#   
#   result = classify(classifier, query)
#   
#   if (result$post_2 < result$post_1){
#     #  cat(levels(training_set[,Y])[1], result$post_1)
#     return (levels(training_set[,Y])[1])
#   }  else  {
#     #   cat(levels(training_set[,Y])[2], result$post_2)
#     return (levels(training_set[,Y])[2])
#     
#   }
#   
# }
LOOCV <- function(formula, data){
  f1.parsed <- ParseFormula(formula,data)
  Y <- f1.parsed$lhs$vars
  X <- f1.parsed$rhs$vars

  res = rep(NA, dim(data)[1])

  for (i in 1:dim(data)[1]){
    t0 = Sys.time()
    trainig_set = data[-i,]
    test = data[i,X]
    test = as.vector(t(test))
    # res[i] = NB(formula, trainig_set, test);

    res[i] = NB_fast(Y, X, trainig_set, test);
    t1 = Sys.time() - t0;

    cat("trial#", i, "time remaining = ", t1*(dim(data)[1]-i)/60, "min")
    print(i)


  }
  perf = mean(data[,Y] == res)
  cat('\n', perf)

}





# LOOCV_fast <- function(Y, X, data, n){
# 
#   
#   res = rep(NA, dim(data)[1])
#   
#   for (i in 1:dim(data)[1]){
#     t0 = Sys.time()
#     trainig_set = data[-i,]
#     test = data[i,X]
#     test = as.vector(t(test))
#     # res[i] = NB(formula, trainig_set, test);
#     
#     res[i] = NB_fast(Y, X, trainig_set, test);
#     t1 = Sys.time() - t0;
#     
#     cat("trial#", i, "time remaining = ", t1*(dim(data)[1]-i)/60, "min")
#     print(i)
#     
#     
#   }
#   perf = mean(data[,Y] == res)
#   f = paste(n[1], "r.RData")
#   save(res,file=f )
#   cat('\n', perf)
#   
# }



# 
# load("sentiment")
# data <- subset(r,select=c("sentiment","text"))
# 
# #function sentiment
# sentiment <-function(n){
# 
# text = data[n,2]
# TYPE = data[n,1]
# # text = data[,2]
# # TYPE = data[,1]
# docs <- Corpus(VectorSource(text))
# #cleaning the test
# # Convert the text to lower case
# docs <- tm_map(docs, content_transformer(tolower))
# # Remove numbers
# docs <- tm_map(docs, removeNumbers)
# # Remove english common stopwords
# docs <- tm_map(docs, removeWords, stopwords("english"))
# # Remove your own stop word
# # specify your stopwords as a character vector
# docs <- tm_map(docs, removeWords, c("blabla1", "blabla2")) 
# # Remove punctuations
# docs <- tm_map(docs, removePunctuation)
# # Eliminate extra white spaces
# docs <- tm_map(docs, stripWhitespace)
# # remove URLs
# # removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
# # docs <- tm_map(docs, removeURL)
# 
# #Build a term-document matrix
# dtm <- TermDocumentMatrix(docs)
# dtm = weightBin(dtm)
# m <- as.matrix(dtm)
# data = cbind(t(m), TYPE)
# # data.train = as.factor(data[-1,])
# # data.test=as.factor(data[1,-46])
# 
# # data.train = data.frame(data[-1,])
# # data.test=as.vector(data[1,-46])
# data = data.frame(data)
# 
# # for (i in 1:(dim(data)[2]-1)){
# #   levels(data)<-c("0", "1")
# # }
# 
# X = colnames(data[,-ncol(data)])
# Y = "TYPE"
# a = LOOCV_fast(Y, X, data, n)
# }
