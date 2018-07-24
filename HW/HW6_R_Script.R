### This scriot is written by Genwei Zhang fot the purpose of CS 5033 
### homework 6 implementation. Date: 11/29/2017

##Question 1

install.packages("neuralnet")
library(neuralnet)
setwd("/Users/Genwei/Desktop/Data Science degree coursework/Fall 2017/CS4033:5033 ML/Homework/HW6/")
originaldata<-read.csv("Dataset.csv", header=FALSE,sep = ",")

# change the last column into 1 for g and 0 for h.
responsevector<-ifelse (originaldata[,11] %in% ("g"),1,0)
#Scale and re-organize the datatable.
scaleddata<-scale(originaldata[,1:10],scale=TRUE)
transformeddata<-cbind(scaleddata,responsevector)

# Randomly split the training, validation and testing data
# 50% training, 30% validation, 20% testing
samp<-sample(nrow(transformeddata),0.2*nrow(transformeddata))
testingdata<-transformeddata[samp,]
restdata<-transformeddata[-samp,]
samp1<-sample(nrow(restdata),(3/8)*nrow(restdata))
trainingdata<-restdata[-samp1,]
validationdata<-restdata[samp1,]

# Initialize Xselec and Xremain
# Create an empty chosen variable for each step; also initialize validation data.
Xremain<-trainingdata[,1:11]
Xselec<-trainingdata[,0]
tempvalidation<-validationdata[,0]
validationdataremain<-validationdata[,1:11]
# create an vector to store the mininum crossentropy every time.
mincrossentropy<-vector(mode="integer",)

## create an empty vector to store the order of selected columns.
# order<-vector(,)
# set up progress bar
prb<-txtProgressBar(min=0, max=10, initial = 0, style = 3)

for (i in 1:10) {
  # initialize a new crossentroy that store all cross-entropy values within each sub-for loop.
 crossentropy<-vector(mode="integer",)
 # update the training and validation data.
 tempdata1<-cbind(Xselec,Xremain)
 tempdata2<-cbind(tempvalidation,validationdataremain)
 innerprb<-txtProgressBar(min=0, max=(ncol(Xremain)-1), initial = 0, style = 3)
 for (j in 1:(ncol(Xremain)-1)) {
 # Train the neural network by calling the function: neuralnet()
   if (i==1){
   n<-colnames(tempdata1[,c(j,11)])
   f <- as.formula(paste("responsevector~", paste(n[!n %in% "responsevector"], collapse = " + ")))
   model.net<-neuralnet(f,tempdata1[,c(j,11)], 
                        hidden = c(5), threshold = 0.1, 
                        stepmax = 1e+05, rep = 1, linear.output = TRUE )
   
  validationdatapredict<-compute(model.net,tempdata2[,j])
  predictedprob<-validationdatapredict$net.result
   } else {
     n<-colnames(tempdata1[,c(1:(i-1),(i-1+j),11)])
     f <- as.formula(paste("responsevector~", paste(n[!n %in% "responsevector"], collapse = " + ")))
     model.net<-neuralnet(f,tempdata1[,c(1:(i-1),(i-1+j),11)], 
                          hidden = c(5), threshold = 0.1, 
                          stepmax = 1e+05, rep = 1, linear.output = TRUE,
                          startweights = model.net$weights)
     
     validationdatapredict<-compute(model.net,tempdata2[,c(1:(i-1),(i-1+j))])
     predictedprob<-validationdatapredict$net.result
}
a<-vector(mode="integer",length=length(predictedprob))
b<-vector(mode="integer",length=length(predictedprob))
for (k in 1: length(predictedprob)){
  a[k]=validationdata[k,11]*log2(predictedprob[k])
  b[k]=(1-validationdata[k,11])*log2(1-predictedprob[k])
}
a[is.na(a)]<-0;
b[is.na(b)]<-0;
# compute cross entropy
crossentropy[j]<-(1/-length(predictedprob))*sum(a+b)
setTxtProgressBar(innerprb,j)
}
# selected the lowest cross-entropy, return the index
selectedindex<-which(crossentropy[1:(ncol(Xremain)-1)]==min(crossentropy[1:(ncol(Xremain)-1)]))
mincrossentropy[i]<-min(crossentropy[1:(ncol(Xremain)-1)])
# updata Xselec, Xremain, tempvalidation and validationdataremain based on the selected index.
Xselec<-cbind(Xselec, Xremain[,selectedindex,drop=FALSE])
Xremain<-Xremain[,-selectedindex]
tempvalidation<-cbind(tempvalidation, validationdataremain[,selectedindex,drop=FALSE])
validationdataremain<-validationdataremain[,-selectedindex]

setTxtProgressBar(prb,i)
}
mincrossentropy
Xselec

# based the mincrossentropy decrease trend, it decreased 6.9% from step 4 to step 5, but from step 5 to step 6...
# it increased a bit, so we should select the first 5 variables, V9, V1, V3, V2, V4.


# Train the neural net again using only selected 5 variables
newtraindata<-trainingdata[,c(1:4,9,11)]
newvalidatedata<-validationdata[,c(1:4,9,11)]
newtestingdata<-testingdata[,c(1:4,9,11)]

n<-colnames(newtraindata)
f <- as.formula(paste("responsevector~", paste(n[!n %in% "responsevector"], collapse = " + ")))
model.net<-neuralnet(f,newtraindata, 
                     hidden = c(5), threshold = 0.1, 
                     stepmax = 1e+05, rep = 1, linear.output = TRUE,
                     startweights = model.net$weights)

# predict the new validationdata
newvalidationdatapredict<-compute(model.net,newvalidatedata[,1:5])
newpredictedprob<-newvalidationdatapredict$net.result
a<-vector(mode="integer",length=length(newpredictedprob))
b<-vector(mode="integer",length=length(newpredictedprob))
for (k in 1: length(newpredictedprob)){
  a[k]=validationdata[k,11]*log2(newpredictedprob[k])
  b[k]=(1-validationdata[k,11])*log2(1-newpredictedprob[k])
}
a[is.na(a)]<-0;
b[is.na(b)]<-0;
newcrossentropy_validate<-(1/-length(newpredictedprob))*sum(a+b)

# predict the new testingdata
testingpredict<-compute(model.net,newtestingdata[,1:5])
newtestingpredictedprob<-testingpredict$net.result
c<-vector(mode="integer",length=length(newtestingpredictedprob))
d<-vector(mode="integer",length=length(newtestingpredictedprob))
for (k in 1: length(newtestingpredictedprob)){
  c[k]=testingdata[k,11]*log2(newtestingpredictedprob[k])
  d[k]=(1-testingdata[k,11])*log2(1-newtestingpredictedprob[k])
}
c[is.na(c)]<-0;
d[is.na(d)]<-0;
newcrossentropy_testing<-(1/-length(newtestingpredictedprob))*sum(c+d)

# calculating AUC
install.packages('pROC')
library(pROC)
# AUC of validation data
roc_validation <- roc(validationdata[,11], newpredictedprob)
auc(roc_validation)

# AUC of testing data
roc_testing <- roc(testingdata[,11], newtestingpredictedprob)
auc(roc_testing)



