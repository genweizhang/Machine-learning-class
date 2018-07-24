library(randomForest)
install.packages('gplots')
install.packages('ROCR')
library(gplots)
library(ROCR)
getwd()
setwd("/Users/Genwei/Desktop/Dr_Shao/Water_cluster")
md<-read.csv("aimd_water_dimer.distances.csv", header = TRUE, na.strings = "na")
# charge<-read.csv("aimd_water_dimer.charges.csv", header = TRUE, na.strings = "na")
# head(charge, 100)
# charge.md<-charge/md
head(md,10)
energy<-read.csv("aimd_water_dimer.energy.csv", header = TRUE, na.strings = "na")

head(energy, 10)
md2<-cbind(md, energy)
head(md2,10)
samp<-sample(nrow(md2), 0.9*nrow(md2))
mdtrain<-md2[samp,]
mdtest<-md2[-samp,]
head(mdtrain, 20)
set.seed(123)
md2.rf<-randomForest(energy~. -energy, data=mdtrain, mtry=5, ntree=500)
print(md2.rf)
pred=predict(md2.rf, newdata = mdtest)
head(pred, 10)
# head(mdtest$energy, 10)
d<-pred-mdtest$energy
# write(d, "", sep="\t")
rmsd=sqrt(sum(d^2)/length(d))
rmsd*627.509

plot(pred, mdtest$energy,xlab="x/ predicted energy (Hartree)", 
     ylab="y/ given energy (Hartree)", 
     main="Results from a trained randomForest model")
abline(lm(mdtest$energy~pred), col="blue")
s<-summary(lm(mdtest$energy~pred))
s
fit<-lm(mdtest$energy~pred)
cf<-round(coef(fit),2)
cf
r2=round(s$r.squared,4)
r2
text(-152.847, -152.834,"R^2=0.8936")
text(-152.847, -152.832,"y=1.06x+9.21")
text(-152.847, -152.836,"RMSD=0.883kcal/mol")


mtry<-tuneRF(mdtrain[-16], mdtrain$energy, ntreeTry = 500, 
             stepFactor = 1.5, improve = 0.01, trace = TRUE, plot=TRUE)

print(mtry)



