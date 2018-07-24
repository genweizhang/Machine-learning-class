## This code is for custom neural network implementation, took some inspiration from the 
## following web link: https://www.r-bloggers.com/r-for-deep-learning-i-build-fully-connected-neural-network-from-scratch/

# This neural network R-code is for regression analysis only

# Sigmoid function:
sigmoid<-function(x){
  g<-1/(1+exp(-1*x))
  g
}
# Piece-wise linear
pwl<-function(x){
  if (x>=1/2){
    1;
  }
  if (x>(-1/2)&&x<1/2){
    x+1/2;
  }
  if(x<=(-1/2)){
    0;
  }
}
# Prediction function
NNpred<-function(model, data=testdata){
  new.data <- data.matrix(data)
  hidden.layer <- sweep(new.data %*% model$W1 ,2, model$b1, '+')
  # activation func : tanh/sigmoid/pwl
  hidden.layer <- sigmoid(hidden.layer)
  value <- sweep(hidden.layer %*% model$W2, 2, model$b2, '+')
  return(value)
}

# Training function
NNtrain<-function(x, y, traindata=data, testdata=data, 
                  hidden=c(6), maxit=2000, 
                  #delta loss:
                  abstol=1e-8, 
                  #learning rate:
                  lr=1e-2,
                  # regularization rate:
                  reg=1e-4,
                  # show result every "display" steps:
                  display=100,
                  random.seed=1) {
  set.seed(random.seed)
  N<-nrow(traindata)
  X<-unname(data.matrix(traindata[,x]))
  Y<-traindata[,y]
  
  #if (is.factor(Y)) {Y<-as.integer(Y)}
  # Y.length<-length(unique(Y)) # unique() remove duplicates, return a vector!
  # Y.set<-sort(unique(Y))
  # Y.index<-cbind(1:N, match(Y, Y.set)) # match() return a vector which has Y's ranking numbers
  # after matcing onto Y.set.
  
  D<-ncol(X) # number of input features, which will equal to the number of input neurons.
  # number of categories for classification
  
  K<-1
  H<-hidden
  
  # create initial weights and bias:
  
  W1<-matrix(rnorm(D*H), nrow=D, ncol=H)/sqrt(D*H)
  b1<-matrix(0, nrow=1, ncol=H)
  
  W2<-0.01*matrix(rnorm(H*K), nrow=H, ncol=K)/sqrt(D*H)
  b2<-matrix(0, nrow=1, ncol=K)
  
  batchsize<-N; # if dataset is too big, can chop into smaller datasets by sample().
  # set initial loss to a big number:
  loss<-100000
  # set iteration i=0.
  i<-0;
  
  # start training
  while (i<maxit&& loss>abstol) {
    i<-i+1;
    ## forward propagation:
    hidden.layer<-sweep(X%*%W1, 2, b1, '+');
    # apply activation function: tanh/sigmoid/pwl
    hidden.layer<-sigmoid(hidden.layer);
    score<-sweep(hidden.layer%*%W2,2, b2, '+');
    
    # compute the loss, (mean squrared error)
    diff<-score-Y;
    data.loss<-(t(diff)%*%diff)/(2*batchsize)
    # L2 regularization: add 1/2λW^2 for each W (where λ is the strength).
    reg.loss<-0.5*reg*(sum(W1*W1)+sum(W2*W2)) 
    loss<-data.loss+reg.loss
    
    
    # display results and update model
    if( i %% display == 0) {
      if(!is.null(testdata)) {
        model <- list( D = D,
                       H = H,
                       K = K,
                       # weights and bias
                       W1 = W1, 
                       b1 = b1, 
                       W2 = W2, 
                       b2 = b2)
        value <- NNpred(model, testdata[,-y])      
        # calculate mean squared error
        diff<-value-testdata[,y];
        d<-nrow(testdata)
        mse <- sqrt(sum(diff^2))/d*627.51
        cat(i, loss, mse, "\n")
      } else {
        cat(i, loss, "\n")
      }
    }
    
    # backward propagation:
    
    dscores<-2*(score-Y);
    dscores<-dscores/batchsize;
    
    dW2<-t(hidden.layer)%*%dscores;
    db2<-colSums(dscores);
    
    dhidden<-dscores%*%t(W2);
    
    dW1 <- dhidden * (hidden.layer * (1-hidden.layer))
    dW1 <- t(X) %*% dW1
    
    db1 <- dhidden * (hidden.layer * (1-hidden.layer))
    db1 <- colSums(db1)
    
    
    ### update weigths and bias:
    dW2<-dW2+reg*W2;
    dW1<-dW1+reg*W1;
    
    W1<-W1-lr*dW1;
    b1<-b1-lr*db1;
    
    W2<-W2-lr*dW2;
    b2<-b2-lr*db2;
    
  } 
  
  
  # Final result output:
  model <- list( D = D,
                 H = H,
                 K = K,
                 # weights and bias
                 W1= W1, 
                 b1= b1, 
                 W2= W2, 
                 b2= b2);
  
  return(model);
}


set.seed(1)

# water-dimer fixed_monomer test!

setwd("C:/Users/Yang_PLS/Desktop/Genwei/NN")
rawdistance<-read.csv("distances_fixed_monomer.csv", header=FALSE, sep=",")
dim(rawdistance)
energyinput<-read.csv("energy_fixed_monomer.csv", header=TRUE,sep=",")
rawdata<-cbind(rawdistance, energyinput)
dim(rawdata)
head(rawdata, 50)
# scale method one:[(x)-mean(x)]/sd(s)
scaleddata<-scale(rawdata,scale=TRUE)
samp1<-sample(nrow(scaleddata), 0.9*nrow(scaleddata))
# samp2<-sample(nrow(scaleddata[-samp,]), 0.2*nrow(scaleddata[-samp,]))

traindata<-scaleddata[samp1,]
dim(traindata)

testdata<-scaleddata[-samp1,]
dim(testdata)

#traindata<-df[samp,]

#testdata<-df[samp2,]
# training scaled distances
watermodel <- NNtrain(x=1:9, y=10, traindata, testdata, 
                       hidden=10, maxit=1000000, display=200)
watermodel2 <- NNtrain(x=1:9, y=10, traindata, testdata, 
                      hidden=10, maxit=3000000, display=3000)

# 5000 training, 5000 prediction
watermodel3 <- NNtrain(x=1:9, y=10, traindata, testdata, 
                      hidden=10, maxit=1000000, display=1000)

watermodel4 <- NNtrain(x=1:9, y=10, traindata, testdata, 
                       hidden=50, maxit=1000000, display=1000)
watermodel5 <- NNtrain(x=1:9, y=10, traindata, testdata, 
                       hidden=50, maxit=5000000, display=1000)

watermodel6 <- NNtrain(x=1:9, y=10, traindata, testdata, 
                       hidden=50, maxit=10000000, display=1000)
# 9000_1000, 10000000 iterations, 50 hidden layers.
watermodel7<- NNtrain(x=1:9, y=10, traindata, testdata, 
                      hidden=50, maxit=10000000, display=1000)

# 9000_1000, 10000000 iterations, 10 hidden layers.
watermodel8<- NNtrain(x=1:9, y=10, traindata, testdata, 
                      hidden=10, maxit=10000000, display=1000)


predictdata<-testdata
pred<-NNpred(watermodel7, predictdata[,-10])


# if used scale method, do following: 

pred_<-(pred*sd(rawdata$energy)+mean(rawdata$energy))
predictdata_<-(predictdata*sd(rawdata$energy)+mean(rawdata$energy))

library(utils)
library(ROCR)
library(gplots)

min(predictdata_[,10])
max(predictdata_[,10])
plot(pred_,predictdata_[,10], xlim=c(-152.855, -152.840), ylim=c(-152.855, -152.840),
     xlab="x/ predicted energy (Hartree)", 
     main=" Results from a self-built NN_fixed monomer", ylab="y/ Given energy (Hartree)")
abline(lm(predictdata_[,10]~pred_), col="red")

# Compute RMSD values
d<-pred_-predictdata_[,10]
rmsd=sqrt(sum(d^2)/length(d))
rmsd
rmsd*627.509
text(-152.850, -152.843,"RMSD= 0.1946573 kcal/mol")

# Compute MAE
absd<-abs(d)
MAE<-sum(absd)/length(d)
MAE

