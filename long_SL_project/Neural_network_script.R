install.packages("neuralnet")
library(neuralnet)
# library(nnet)
install.packages('utils')
library(utils)
install.packages('ROCR')
library(ROCR)
install.packages('plyr')
library(plyr)
install.packages('foreach')
library(foreach)
install.packages("NeuralNetTools")
library('NeuralNetTools')
install.packages('gplots')
library(gplots)

cl<-makeCluster(detectCores()-1)
registerDoParallel(cl) 

setwd("/Users/Genwei/Desktop/Dr_Shao/Water_cluster/")
distancedata<-read.csv("aimd_water_dimer.distances.csv", header=TRUE, sep = ",")
energydata<-read.csv("aimd_water_dimer.energy.csv", header=TRUE, sep = ",")
md<-cbind(distancedata,energydata)
samp<-sample(nrow(md), 0.9*nrow(md))

## This is not a very suitable scaling method for this case!
maxs<-apply(md, 2, max)
mins<-apply(md, 2, min)
scaledmd<-as.data.frame(scale(md, center = mins, scale = maxs-mins))

## This scaling method is better for this case, which is: [(x)-mean(x)]/sd(s)
scaledmd2<-scale(md)
mdtrain<-scaledmd2[samp,]
mdtest<-scaledmd2[-samp,]

dim(mdtrain)
dim(mdtest)

set.seed(123)
md.net<-neuralnet(energy~r12+r13+r14+r15+r16+r23+r24+r25+r26+r34+r35+r36+r45+r46+r56, 
                  data=mdtrain, hidden = c(5), threshold = 0.05, stepmax = 1e+08, 
                  rep = 1,act.fct = 'tanh', linear.output = TRUE )

plot(md.net,rep="best")


# mlp() to evaluate the relative importance of input variables.
install.packages('RSNNS')
install.packages('Rcpp')
library('RSNNS')

x<-mdtrain[,c('r12','r13','r14','r15','r16','r23',
                'r24','r25','r26','r34','r35','r36','r45','r46','r56')]
y<-mdtrain[,'energy']
rel_imp<-mlp(x,y, size=5, lin.Out =TRUE)
# use garson algorithm to tell input variable importance
garson(md.net, bar_plot=TRUE, xlab='input variable', ylab='relative importance')

pred<-compute(md.net, mdtest[,-16])
# ls(pred)
# pred$neurons
p<-pred$net.result
#p_<-p*(max(md2$energy)-min(md2$energy))+min(md2$energy)
#mdtest_<-mdtest$energy*(max(md2$energy)-min(md2$energy))+min(md2$energy)
# head(p,10)
# head(mdtest[,16],10)
p_<-p*sd(mdd2$energy)+mean(mdd2$energy)

#length(p_)
mdtest_<-mdtest[,16]*sd(mdd2$energy)+mean(mdd2$energy)
#length(mdtest_)
s.pred<-summary(lm(mdtest_~p_))
plot(p_,mdtest_, xlab="x/ predicted energy (Hartree)", main=" Results from a trained neural network", ylab="y/ Given energy (Hartree)")
abline(lm(mdtest_~p_), col="blue")
s.pred$r.squared

#    text(-152.847, -152.836,"R^2=0.9811")
#    text(-152.847, -152.834,"y=1.01x+1.82")
#    text(-152.847, -152.838,"RMSD=0.3638 kcal/mol")

d<-p_-mdtest_
rmsd=sqrt(sum(d^2)/length(d))
RMSD=rmsd*627.509
RMSD

stopCluster(cl)


