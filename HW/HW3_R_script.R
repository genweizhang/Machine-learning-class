### This code is written for the purpose of cs4033/5033 class HW3 by Genwei Zhang, 10/16/2017.

## Question 1:

#Install required package for performing the regularzied linear regression "glmnet".
install.packages("glmnet",repos = "http://cran.us.r-project.org" )
library('glmnet')
# Direct the working directory.
setwd("~/Desktop/Data Science degree coursework/Fall 2017/CS4033:5033 ML/Homework/HW3/")
# Read in the data file, saved in .csv format using read.csv fcuntion.
originaldata<-read.csv2("HW3_original_data.csv", header=TRUE, sep="," )
# check the dimension and column names of original file.
dim(originaldata)
colnames(originaldata)
# Remove 20% original data for testing, randomly.
samp1<-sample(nrow(originaldata), 0.2*nrow(originaldata))
testdata<-originaldata[samp1,]
remainingdata<-originaldata[-samp1,]

# List all possible alpha (10) and lambda (11) values
all_alpha<-c(0.001, 0.01, 0.05, 0.1, 0.5, 1.0, 5.0, 10.0, 50.0, 100.0)
all_lambda<-c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)

# Train regularized linear-regression models. Using a for loop to optimize the hyperparameters.
# Interestingly, in function glmnet(), the regularization weight is lambda ('all_alpha' in our case), 
# and the elastic-net parameter is called alpha ('all_lambda' in our case), which is opposite to 
# what is given in the homework. 

# Generate an empty matrix to store all the mean squared errors.
meansquarederr1<-data.frame(matrix(,ncol=102))
# start the 'for' loop
for (k in 1:100){
  # The rest 80% data for training (75%) and validation (25%), split randomly.
  samp2<-sample(nrow(remainingdata),0.75*nrow(remainingdata))
  trainingdata<-remainingdata[samp2,]
  validationdata<-remainingdata[-samp2,]
  # Prepare the input and output varialbes on the training data/validation data
  inputtrain<-data.matrix(trainingdata[,c(1:5,7:22)])
  outputtrain<-data.matrix(trainingdata[, 6])
  inputvalidate<-data.matrix(validationdata[,c(1:5,7:22)])
  outputvalidate<-data.matrix(validationdata[,6])
  # Another two 'for' loops to try all 110 alpha and lambda combinations
  for (i in 1:11){
    for(j in 1:10){
      RLR_model<-glmnet(x=inputtrain, y=outputtrain,lambda = all_alpha[j], alpha = all_lambda[i] )
      predictedvalidate<-predict(RLR_model,newx=inputvalidate, type="response",s=all_alpha[j])
      meansquarederr1[10*(i-1)+j,1:2]=c(all_lambda[i],all_alpha[j])
      meansquarederr1[10*(i-1)+j,k+2]<-sum((predictedvalidate-as.numeric(outputvalidate))^2)/length(predictedvalidate)
    }
  }
}
# take average of 100 time repeat
averagemean1<-rowMeans(meansquarederr1[,3:102])
# reformat the averagemean (a vector) into a matrix, named temptable.
temptable<-matrix(vector(),10,11)
i=1;
j=1;
for (i in 1:11){
  for (j in 1:10){
    temptable[j,i]=averagemean1[10*(i-1)+j]
  }
} 

#install package 'plotly', which can generate color heatmap based on averagemean values.
install.packages("plotly")
library(plotly)
library(graphics)
plot_ly( z=temptable,type="heatmap",
         colors=colorRamp(c("green2", "yellow", "blue1", "blue2", "skyblue","red", "pink", "orange")))

# I found that the combination of alpha=0.01 and lambda=0.0 has the smallest mean squared error.
# So, train a new model using 80% remainingdata and test it using original 20% testdata.
testdata<-originaldata[samp1,]
remainingdata<-originaldata[-samp1,]
newinputtrain<-data.matrix(remainingdata[,c(1:5,7:22)])
newoutputtrain<-data.matrix(remainingdata[, 6])
testdatainput<-data.matrix(testdata[,c(1:5,7:22)])
testdataoutput<-data.matrix(testdata[,6])
New_model<-glmnet(x=newinputtrain, y=newoutputtrain,lambda = 0.01, alpha = 0.0 )
predictedtest<-predict(New_model,newx=testdatainput, type="response",s=0.01)
meansquarederrtest<-sum((predictedtest-as.numeric(testdataoutput))^2)/length(predictedtest)
# report the final mean squared error
meansquarederrtest


## Question 2:

# install package 'caret', which has kNN-regression function.
install.packages('caret', repos="http://cran.us.r-project.org")
library(caret)

# Remove 20% original data for testing, randomly.
samp3<-sample(nrow(originaldata), 0.2*nrow(originaldata))
testdata<-originaldata[samp3,]
remainingdata<-originaldata[-samp3,]

# list all possible k values
k_values<-c(1:10,20,50)

# Generate an empty matrix to store all the mean squared errors.
meansquarederr2<-data.frame(matrix(,ncol=101))

# start the 100 times repeat.
for (i in 1:100) {
  
# The rest 80% data for training (75%) and validation (25%), split randomly.
samp4<-sample(nrow(remainingdata),0.75*nrow(remainingdata))
trainingdata<-remainingdata[samp4,]
validationdata<-remainingdata[-samp4,]
# Prepare the input and output varialbes on the training data/validation data,just 5 variables.
inputtrain<-data.matrix(trainingdata[,c(2,7,12,18,19)])
outputtrain<-data.matrix(trainingdata[, 6])
inputvalidate<-data.matrix(validationdata[,c(2,7,12,18,19)])
outputvalidate<-data.matrix(validationdata[,6])
for (j in 1:12) {
        knnmodel<-knnreg(x=inputtrain, y=as.numeric(outputtrain),k = k_values[j])
        predictedvalidate<-predict(knnmodel,newdata=inputvalidate)
        meansquarederr2[j,1]=k_values[j]
        meansquarederr2[j,i+1]<-sum((predictedvalidate-as.numeric(outputvalidate))^2)/length(predictedvalidate)
        
  }
}

# take average of 100 time repeat
averagemeanerr2<-rowMeans(meansquarederr2[,2:101])
# plot mean validation error as a function of k
plot (k_values,averagemeanerr2, xlab="K values", ylab="Mean validation error")

# the best K is 50, since it has the smallest mean squared error.
# So, train a new model using 80% remainingdata and test it using original 20% testdata.
testdata<-originaldata[samp3,]
remainingdata<-originaldata[-samp3,]
newinput<-data.matrix(remainingdata[,c(2,7,12,18,19)])
newoutput<-data.matrix(remainingdata[,6])
newtestinput<-data.matrix(testdata[,c(2,7,12,18,19)])
newtestoutput<-data.matrix(testdata[,6])
# train the model
newknnmodel<-knnreg(x=newinput, y=as.numeric(newoutput),k = k_values[12])
predictedtest<-predict(newknnmodel,newdata=newtestinput)
newmeansquarederr<-sum((predictedtest-as.numeric(newtestoutput))^2)/length(predictedtest)
# report the final mean squared error
newmeansquarederr


## Question 4

# install 'clue' package, which has the predict function for k-means clustering.
install.packages('clue', repos="http://cran.us.r-project.org")
library('clue')

# Remove 20% original data for testing, randomly.
samp5<-sample(nrow(originaldata), 0.2*nrow(originaldata))
testdata<-originaldata[samp5,]
remainingdata<-originaldata[-samp5,]

# list all possible k values
K_values<-c(2:15,20,25,50)

# Generate an empty matrix to store all the mean squared errors.
meansquarederr4<-data.frame(matrix(,ncol=101))

# start the 100 times repeat.
for (i in 1:100) {
  
  # The rest 80% data for training (75%) and validation (25%), split randomly.
  samp6<-sample(nrow(remainingdata),0.75*nrow(remainingdata))
  trainingdata<-remainingdata[samp6,]
  validationdata<-remainingdata[-samp6,]
  # Prepare the input and output varialbes on the training data/validation data, 5 variables same to Question 2.
  inputtrain<-data.matrix(trainingdata[,c(2,7,12,18,19)])
  outputtrain<-data.matrix(trainingdata[, 6])
  inputvalidate<-data.matrix(validationdata[,c(2,7,12,18,19)])
  outputvalidate<-data.matrix(validationdata[,6])
  for (j in 1:17) {
    kmeansmodel<-kmeans(inputtrain,K_values[j])
    predictedvalidate<-cl_predict(kmeansmodel,newdata=inputvalidate)
    meansquarederr4[j,1]=K_values[j]
    meansquarederr4[j,i+1]<-sum((predictedvalidate-as.numeric(outputvalidate))^2)/length(predictedvalidate)
  }
}

# take average of the 100 time repeats
averagemeanerr4<-rowMeans(meansquarederr4[,2:101])
# plot mean validation error as a function of k
plot (K_values, averagemeanerr4, xlab="K values", ylab="Mean validation error")

# the best K is 50, since it has the smallest mean squared error.
# So, train a new model using 80% remainingdata and test it using original 20% testdata.
testdata<-originaldata[samp5,]
remainingdata<-originaldata[-samp5,]
newinput<-data.matrix(remainingdata[,c(2,7,12,18,19)])
newoutput<-data.matrix(remainingdata[,6])
newtestinput<-data.matrix(testdata[,c(2,7,12,18,19)])
newtestoutput<-data.matrix(testdata[,6])
# train the model
newkmeansmodel<-kmeans(newinput, 50)
predictedtest<-cl_predict(newkmeansmodel,newdata=newtestinput)
newmeansquarederr4<-sum((predictedtest-as.numeric(newtestoutput))^2)/length(predictedtest)
# report the final mean squared error
newmeansquarederr4



