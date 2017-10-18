
library("data.table")
library("DescTools")
library("plyr")
#setwd("C:/GoogleDrive/Fall_2016/PSYCH 625/Project2")
setwd("C:/Users/hemaz/Google Drive/Fall_2016/PSYCH 625/Project2")

############## training 
train <- function (Y, X) {
 
  
  posterior = vector(mode = "list", length = ncol(X))
  for (i in 1:ncol(X)){
    t = table(Y, X[,i] )
    t[1,]= t[1,]/sum(t[1,])
    t[2,]= t[2,]/sum(t[2,])
    
    posterior[[i]] = t;
  }
  
 
 # calculate priors
 prior_1 = sum(Y==levels(Y)[1])/length(Y)
 prior_2 = 1 - prior_1
 
  return(list(prior=c(prior_1, prior_2), posterior=posterior))
} 
##########classify
classify <- function (classifier, sample) {
  post_1 = classifier$prior[1]
  post_2 = classifier$prior[2]
  for (i in 1:length(classifier$posterior)){
   
    t = classifier$posterior[[i]]
    post_2 = post_2 * t[2,sample[i]]
    post_1 = post_1 * t[1,sample[i]]
    
    
  }
  return(list('post_2'=post_2, 'post_1'=post_1))
}
NB <- function(formula, training_set, query){
  if (!is.data.frame(training_set)) stop("NB can only handle data frames")
  if (!is.vector(query)) stop ("Query needs to be a vector")
  
  f1.parsed <- ParseFormula(formula,data=training_set)
  Y <- f1.parsed$lhs$vars
  X <- f1.parsed$rhs$vars
  a = training_set[,X]
  #make sure query is in the correct format
  for (i in 1:length(X)){
     
    b = levels(a[,i]) #loop over X colomns
    if (!query[i] %in% b) stop("Query dosnot match the model")
  }
  classifier <- train(training_set[,Y], training_set[,X])
  
  result = classify(classifier, query)
  
  if (result$post_2 < result$post_1){
  #  cat(levels(training_set[,Y])[1], result$post_1)
    return (levels(training_set[,Y])[1])
  }  else  {
 #   cat(levels(training_set[,Y])[2], result$post_2)
    return (levels(training_set[,Y])[2])
    
  }
  
}


# 
# 
data <-read.csv('Tennis.csv')
data = data[,-1]
# # #
# #
# formula <- Play ~ Outlook + Temperature + Humidity + Wind
# # #
# query <- c("Sunny", "Cool", "High", "Strong")
# NB(formula, data, query)

# query <- c("Sunny", "Cool", "Normal", "Weak")
# NB(formula, data, query)







# query <- c("Hot", "Cool", "Normal", "Weak")
# NB(formula, data, query)

#NB(formula, c(1,2), query)
# query <- data[data$Outlook == "Sunny", ]
# NB(formula, data, query)
# 
# 
# 
# ##########House data
houseData <- read.csv("house-votes-84.data",na.strings="?")
# # colnames(houseData) <- c("party", "handicapped-infants",
# #                          "water-project-cost-sharing",
# #                          "adoption-of-the-budget-resolution",
# #                          "physician-fee-freeze",
# #                          "el-salvador-aid",
# #                          "religious-groups-in-schools",
# #                          "anti-satellite-test-ban",
# #                          "aid-to-nicaraguan-contras",
# #                          "mx-missile",
# #                          "immigration",
# #                          "synfuels-corporation-cutback",
# #                          "education-spending",
# #                          "superfund-right-to-sue",
# #                          "crime", "duty-free-exports",
# #                          "export-administration-act-south-africa")
colnames(houseData) = c("party", LETTERS[seq( from = 1, to = 16 )])
mydata <- houseData[complete.cases(houseData),]
# formula <- party ~ .
# query <- c(as.vector(t(mydata[2,-1])))
# NB(formula,mydata[-2,],query)

##############leave one out cross validation 
LOOCV <- function(formula, data){
  f1.parsed <- ParseFormula(formula,data=data)
  Y <- f1.parsed$lhs$vars
  X <- f1.parsed$rhs$vars
  res = rep(NA, dim(data)[1])
  for (i in 1:dim(data)[1]){
    trainig_set = data[-i,]
    test = data[i,X]
    test = as.vector(t(test))
    res[i] = NB(formula, trainig_set, test);
    
  }
  perf = mean(data[,Y] == res)
  cat('\n', perf)

}
# query <- c("Sunny", "Cool", "High", "Strong")
# formula <- Play ~ Outlook + Temperature + Humidity + Wind
#  LOOCV(formula, data)
# 
#  formula <- Outlook ~ Temperature + Humidity + Wind + Play
#  LOOCV(formula,data)
# 
#  formula <- party ~.
#  LOOCV(formula,mydata)
 
 
# 
# formula <- party ~ .
# LOOCV(formula,mydata)
# install.packages("tm")  # for text mining
# install.package("SnowballC") # for text stemming
# install.packages("wordcloud") # word-cloud generator 
# install.packages("RColorBrewer") # color palettes
# Load
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

load("sentiment")
data <- subset(r,select=c("sentiment","text"))

#function sentiment
text = data[1:5,2]
class = data[1:5,1]
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

 #Build a term-document matrix
dtm <- TermDocumentMatrix(docs)
dtm = weightBin(dtm)
m <- as.matrix(dtm)
data = cbind(t(m), class)
data.train = as.factor(data[-1,])
data.test=as.factor(data[1,-46])

NB(class ~., data.frame(data.train), as.vector(data.test))

# v <- sort(rowSums(m),decreasing=TRUE)
# d <- data.frame(word = names(v),freq=v)
# wordcloud(words = d$word, freq = d$freq, min.freq = 1,
#           max.words=200, random.order=FALSE, rot.per=0.35, 
#           colors=brewer.pal(8, "Dark2"))