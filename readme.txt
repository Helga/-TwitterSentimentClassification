Oct 18, 2017
Note by HM
Modified July 21 2016


My implementation of a Naive Bayes classifier and leave-one-out cross validation in R. Used for predicting the sentiment category of the First GOP Debate Twitter Sentiment corpus.

######################
File: NB_LOOCV.R
My implementation of a Naive Bayes classifier, and a Leave-one-out-cross-validation function. 
main functions: 

1) My implementation of a Naive Bayes classifier. This function is called NB 
NB(formula, data, query)

Three inputs: 1. formula describing the model that learning should be performed on 2. data denoting the data-frame in which
the data reside on 3. query which is the instantiation of the predictor terms in the model. 

Outputs a list containing two elements: 1.prediction category of the dependent variable 2. probability of the prediction.

2) Leave-one-out-cross-validation using the NB function. This function is called LOOCV. 
inputs:  formula, data 
outputs: the classification accuracy of the model


3) Use the NB and LOOCV functions to perform sentiment analysis on labeled
text. Predicts the sentiment category of the First GOP Debate Twitter Sentiment corpus. 

-----------
File: NB_LOOCV_FAST.R is a faster implementstion of the above functions. Builds the classifer one time and uses it for classifying all the queries (instead of building it per query).
---------

File: report.Rnw is the markdown file. Can be used to build the classifier, run exmaples and create the pdf file. 