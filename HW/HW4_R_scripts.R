### This code is written for the purpose of cs4033/5033 class HW4 by Genwei Zhang, 11/01/2017.

# Re-direct the working directory.
setwd("~/Desktop/Data Science degree coursework/Fall 2017/CS4033:5033 ML/Homework/HW4/")
# Read in the data file (.csv format) using read.csv function, replace NaN with NA
originaldata<-read.csv("cs5033_fall2017_assign04_data.csv", header=TRUE, sep=",", na.strings = "NaN")
# check the dimension and column names of original file.
dim(originaldata)

##Question 1.a
# Data-preprocessing, replace "NA" with column mean of the non-missing values.
    for (i in 1:20) {
      originaldata[is.na(originaldata[,i]),i]<-mean(originaldata[,i],na.rm = TRUE)
    }
# Prepare training and validaton data (we call it tvdata), always the first 10000,... 
# the last 2000 are used for testing data.
tvdata<-originaldata[1:10000,]
testingdata<-originaldata[10001:12000,]

## Question 1.b
# First, convert the last column (m/s) in originaldata into kt, 1m/s=1.94384kt.
newwindspeed<-originaldata[,21]*3600/1852
#Create an empty output variable; also use 0 to represent non-severe wind, 1 as severe wind.
outputvariable<-vector(mode="integer",length=length(newwindspeed))
    for (i in 1:length(newwindspeed)) {
      if (newwindspeed[i]>=50) {
        outputvariable[i]=1
      }
      else {
        outputvariable[i]=0
      }
    }
# Bind it to original data, column 22.
originaldata<-cbind(originaldata,outputvariable)
colnames(originaldata)<-c('a','b','c','d','e','f','g','h','i',
                          'j','k','l','m','n','o','p','q','r','s','t','u','out')

##Question 2
install.packages('rpart',repos = "http://cran.us.r-project.org")        # for decision tree
install.packages('randomForest',repos = "http://cran.us.r-project.org")  # for random forest
install.packages('gbm',repos = "http://cran.us.r-project.org")          # for gradient boosting
library('rpart')
library('randomForest')
library('gbm')
# prepare the training, validation and testing data
tvdata<-originaldata[1:10000,c(1:20,22)]
testingdata<-originaldata[10001:12000,c(1:20,22)]
samp1<-sample(nrow(tvdata), 0.75*nrow(tvdata))
trainingdata<-tvdata[samp1,]
validationdata<-tvdata[-samp1,]

# 2.a 
# Build the decision tree and predict on both validation and testing data.
decisiontreefit<-rpart(out~a+b+c+d+e+f+g+h+i+j+k+l+m+n+o+p+q+r+s+t, 
                       data = trainingdata)
validationpred<-predict(decisiontreefit, newdata=validationdata)
testingpred<-predict(decisiontreefit,newdata=testingdata)
# Compute the BS for validation and testing data
validateBS<-sum((validationdata[,'out']-validationpred)^2)/length(validationpred)
testingBS<-sum((testingdata[,'out']-testingpred)^2)/length(testingpred)
# Chance of severe wind occur in validationdata
chance1<-sum(validationdata[,'out'])/nrow(validationdata)
boolean1<-validationpred>chance1
# Compute BSS for validation data
validateBSclimo<-sum((validationdata[,'out'][boolean1]-validationpred[boolean1])^2)/length(validationpred[boolean1])
validateBSS<-(validateBSclimo-validateBS)/validateBSclimo
# Chance of severe wind occur in testingdata
chance2<-sum(testingdata[,'out'])/nrow(testingdata)
boolean2<-testingpred>chance2
# Compute BSS for testing data
testingBSclimo<-sum((testingdata[,'out'][boolean2]-testingpred[boolean2])^2)/length(testingpred[boolean2])
testingBSS<-(testingBSclimo-testingBS)/testingBSclimo

# 2.b
# Build the random forest model and predict on both validation and testing data.
responsevariable<-as.factor(trainingdata$out)
randomforestfit<-randomForest(responsevariable~. -out, data=trainingdata, ntree=500,proximity =FALSE)
validatepred<-predict(randomforestfit, newdata = validationdata, type="prob")
testingpred<-predict(randomforestfit, newdata = testingdata, type="prob")

# Compute the BS for validation and testing data
validateBS<-sum((validationdata[,'out']-validatepred[,2])^2)/length(validatepred[,2])
testingBS<-sum((testingdata[,'out']-testingpred[,2])^2)/length(testingpred[,2])

# Compute BSS for validation data
validateBSclimo<-sum((validationdata[,'out'][boolean1]-validatepred[,2][boolean1])^2)/length(validatepred[,2][boolean1])
validateBSS<-(validateBSclimo-validateBS)/validateBSclimo

# Compute BSS for testing data
testingBSclimo<-sum((testingdata[,'out'][boolean2]-testingpred[,2][boolean2])^2)/length(testingpred[,2][boolean2])
testingBSS<-(testingBSclimo-testingBS)/testingBSclimo

# 2.c
# Build the gradient boosting model and predict on both validation and testing data.
gbm.fit<-gbm(out~. -out, data=trainingdata,n.trees = 500, distribution = "adaboost")
validatepred<-predict.gbm(gbm.fit, newdata = validationdata,n.trees =500, type="response")
testingpred<-predict.gbm(gbm.fit, newdata = testingdata, n.trees =500, type="response")

# Compute the BS for validation and testing data
validateBS<-sum((validationdata[,'out']-validatepred)^2)/length(validatepred)
testingBS<-sum((testingdata[,'out']-testingpred)^2)/length(testingpred)

# Compute BSS for validation data
validateBSclimo<-sum((validationdata[,'out'][boolean1]-validatepred[boolean1])^2)/length(validatepred[boolean1])
validateBSS<-(validateBSclimo-validateBS)/validateBSclimo

# Compute BSS for testing data
testingBSclimo<-sum((testingdata[,'out'][boolean2]-testingpred[boolean2])^2)/length(testingpred[boolean2])
testingBSS<-(testingBSclimo-testingBS)/testingBSclimo


## Question 3
# Re-prepare the training, validation and testing data
originaldata<-read.csv("cs5033_fall2017_assign04_data.csv", header=TRUE, sep=",", na.strings = "NaN")
    # Data-preprocessing, replace "NA" with column mean of the non-missing values.
    for (i in 1:20) {
      originaldata[is.na(originaldata[,i]),i]<-mean(originaldata[,i],na.rm = TRUE)
    }
colnames(originaldata)<-c('a','b','c','d','e','f','g','h','i',
                          'j','k','l','m','n','o','p','q','r','s','t','u')
    # Prepare training and validaton data (we call it tvdata), always the first 10000,... 
    # the last 2000 are used for testing data.
    tvdata<-originaldata[1:10000,]
    testingdata<-originaldata[10001:12000,]
    samp1<-sample(nrow(tvdata), 0.75*nrow(tvdata))
    trainingdata<-tvdata[samp1,]
    validationdata<-tvdata[-samp1,]
# 3.a
 # Build the decision tree and predict on both validation and testing data.
  decisiontreefit<-rpart(u~a+b+c+d+e+f+g+h+i+j+k+l+m+n+o+p+q+r+s+t, 
                           data = trainingdata)
  validatepred<-predict(decisiontreefit, newdata=validationdata)
  testingpred<-predict(decisiontreefit,newdata=testingdata)
  # Compute the MAE for validation and testing data
  validateMAE<-sum(abs(validationdata[,'u']-validatepred))/length(validatepred)
  testingMAE<-sum(abs(testingdata[,'u']-testingpred))/length(testingpred)
  # Compute the RMSE for valiation and testing data
  validateRMSE<-sqrt(sum((validationdata[,'u']-validatepred)^2)/length(validatepred))
  testingRMSE<-sqrt(sum((testingdata[,'u']-testingpred)^2)/length(testingpred))

# 3.b
  # Build the random forest model and predict on both validation and testing data.
  randomforestfit<-randomForest(u~. -u, data=trainingdata, ntree=500)
  validatepred<-predict(randomforestfit, newdata = validationdata)
  testingpred<-predict(randomforestfit, newdata = testingdata)  
  # Compute the MAE for validation and testing data
  validateMAE<-sum(abs(validationdata[,'u']-validatepred))/length(validatepred)
  testingMAE<-sum(abs(testingdata[,'u']-testingpred))/length(testingpred)
  # Compute the RMSE for valiation and testing data
  validateRMSE<-sqrt(sum((validationdata[,'u']-validatepred)^2)/length(validatepred))
  testingRMSE<-sqrt(sum((testingdata[,'u']-testingpred)^2)/length(testingpred))
  
# 3.c
  # Build the gradient boosting model and predict on both validation and testing data.
  gbm.fit<-gbm(u~. -u, data=trainingdata,n.trees = 500)
  validatepred<-predict.gbm(gbm.fit, newdata = validationdata,n.trees =500, type="response")
  testingpred<-predict.gbm(gbm.fit, newdata = testingdata, n.trees =500, type="response")
  # Compute the MAE for validation and testing data
  validateMAE<-sum(abs(validationdata[,'u']-validatepred))/length(validatepred)
  testingMAE<-sum(abs(testingdata[,'u']-testingpred))/length(testingpred)
  # Compute the RMSE for valiation and testing data
  validateRMSE<-sqrt(sum((validationdata[,'u']-validatepred)^2)/length(validatepred))
  testingRMSE<-sqrt(sum((testingdata[,'u']-testingpred)^2)/length(testingpred))
  
## Question 5
  
  # Choose two hyperparameters: the number of trees and 
  # the minimum number of predictors sampled for splitting at each branch node (mtry).
  
  numberoftrees<-c(100,200,300,400,500,600,700,800)
  numberofmtrys<-c(2,3,4,5,6,7)
  
  # Generate two empty matrix to store all the RMSE and MAE.
  RMSEtable<-data.frame(matrix(,ncol=8))
  MAEtable<-data.frame(matrix(,ncol=8))
  
  # Set a progress bar
  pb = txtProgressBar(min = 0, max = 8, initial = 0, style=3);
  # Two 'for' loops to try all 8X6 number of trees and mtrys combinations.
    for (i in 1:8){
      for(j in 1:6){
        totalMAE<-0;
        totalRMSE<-0;
        # Repeat 30 times
        for (k in 1:30){
          # From the tvdata data, select training (75%) and validation (25%) randomly.
          samp<-sample(nrow(tvdata),0.75*nrow(tvdata))
          trainingdata<-tvdata[samp,]
          validationdata<-tvdata[-samp,]
          # Build the random forest model and predict on both validation and testing data.
          randomforestfit<-randomForest(u~. -u, data=trainingdata, 
                                        ntree=numberoftrees[i],mtry=numberofmtrys[j])
          validatepred<-predict(randomforestfit, newdata = validationdata)
          # Compute the MAE and RMSE for the validation data
          validateMAE<-sum(abs(validationdata[,'u']-validatepred))/length(validatepred)
          validateRMSE<-sqrt(sum((validationdata[,'u']-validatepred)^2)/length(validatepred))
          # Add them up (30 times)
          totalMAE<-totalMAE+validateMAE
          totalRMSE<-totalRMSE+validateRMSE
        }
        # Put the averaged MAE/RMSE into built matrix for later plotting.
        MAEtable[j,i]=totalMAE/30
        RMSEtable[j,i]=totalRMSE/30
      }
      setTxtProgressBar(pb, i)
    }
  
  #install package 'plotly', which can generate color heatmap based on averaged MAE/RMSE values.
  install.packages("plotly")
  library(plotly)
  library(graphics)
  plot_ly( z=as.matrix(MAEtable),type="heatmap",
           colors=colorRamp(c("green2", "yellow", "blue1", "blue2", "skyblue","red", "pink", "orange")))
  plot_ly( z=as.matrix(RMSEtable),type="heatmap",
           colors=colorRamp(c("green2", "yellow", "blue1", "blue2", "skyblue","red", "pink", "orange")))
  
  # I found that the combination of n.trees=600 and mtry=2 gives the smallest MAE.
  # So, compute the testing data MAE and RMSE using the best model.
  randomforestfit<-randomForest(u~. -u, data=trainingdata, 
                                ntree=600 , mtry=2 )
  testingpred<-predict(randomforestfit, newdata = testingdata)
  # Compute the MAE and RMSE for the validation data
  testingMAE<-sum(abs(testingdata[,'u']-testingpred))/length(testingpred)
  testingRMSE<-sqrt(sum((testingdata[,'u']-testingpred)^2)/length(testingpred))

  
  