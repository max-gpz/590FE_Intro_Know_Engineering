#Support Vector Classifier

set.seed(1)
x=matrix(rnorm(20*2),ncol=2)
y=c(rep(-1,10),rep(1,10))
x[y==1,]=x[y==1,]+1
plot(x,col=(3-y))

dat=data.frame(x=x,y=as.factor(y))
library(e1071)
svmfit=svm(y~.,data=dat,kernel="linear",cost=10,scale=FALSE)
plot(svmfit,dat)

svmfit$index
summary(svmfit)

svmfit=svm(y~.,data=dat,kernel="linear",cost=.1,scale=FALSE)
plot(svmfit,dat)
svmfit$index

set.seed(1)
tune.out=tune(svm,y~.,data=dat,kernel="linear",ranges=list(cost=c(0.001,.01,.1,1,5,10,100)))
summary(tune.out)

bestmod=tune.out$best.model
summary(bestmod)

xtest=matrix(rnorm(20*2),ncol=2)
ytest=sample(c(-1,1),20,rep=TRUE)
xtest[ytest==1,]=xtest[ytest==1,]+1
testdat=data.frame(x=xtest,y=as.factor(ytest))

ypred=predict(bestmod,testdat)
table(predict=ypred,truth=testdat$y)

svmfit=svm(y~.,data=dat,kernel="linear",cost=.01,scale=FALSE)
ypred=predict(svmfit,testdat)
table(predict=ypred,truth=testdat$y)

x[y==1,]=x[y==1,]+.5
plot(x,col=(y+5)/2,pch=19)

dat=data.frame(x=x,y=as.factor(y))
svmfit=svm(y~.,data=dat,kernel="linear",cost=1e+05)
summary(svmfit)
plot(svmfit,dat)

svmfit=svm(y~.,data=dat,kernel="linear",cost=1)
summary(svmfit)
plot(svmfit,dat)

#Support Vector Machine

set.seed(1)
x=matrix(rnorm(200*2),ncol=2)
x[1:100,]=x[1:100,]+2
x[101:150,]=x[101:150,]-2
y=c(rep(1,150),rep(2,50))
dat=data.frame(x=x,y=as.factor(y))

plot(x,col=y)

train=sample(200,100)
svmfit=svm(y~.,data=dat[train,],kernel="radial",gamma=1,cost=1)
plot(svmfit,dat[train,])
summary(svmfit)
svmfit=svm(y~.,data=dat[train,],kernel="radial",gamma=1,cost=1e5)
plot(svmfit,dat[train,])

set.seed(1)
tune.out=tune(svm,y~.,data=dat[train,],kernel="radial",ranges=list(cost=c(.1,1,10,100,1000),gamma=c(.5,1,2,3,4)))
summary(tune.out)

table(true=dat[-train,"y"],pred=predict(tune.out$best.model,newdata=dat[-train,]))



#SVM with Multiple Classes

set.seed(1)
x=rbind(x,matrix(rnorm(50*2),ncol=2))
y=c(y,rep(0,50))
x[y==0,2]=x[y==0,2]+2
dat=data.frame(x=x,y=as.factor(y))
par(mfrow=c(1,1))
plot(x,col=(y+1))

svmfit=svm(y~.,data=dat,kernel="radial",cost=10,gamma=1)
plot(svmfit,dat)






#FDA etc. 
library(mda)



set.seed(1)
x=matrix(rnorm(200*2),ncol=2)
x[1:100,]=x[1:100,]+2
x[101:150,]=x[101:150,]-2
y=c(rep(1,150),rep(2,50))
dat=data.frame(x=x,y=as.factor(y))

plot(x,col=y)

train=sample(200,100)
fda.fit=fda(y~.,data=dat[train,])
fda.pred=predict(fda.fit,newdata=dat[-train,])
table(dat[-train,"y"],fda.pred)

fda.fit2=fda(y~.,data=dat[train,],method=mars)
fda.pred2=predict(fda.fit2,newdata=dat[-train,])
table(dat[-train,"y"],fda.pred2)

mda.fit=mda(y~.,data=dat)
mda.pred=predict(mda.fit,newdata=dat[-train,])
table(dat[-train,"y"],mda.pred)

mda.fit2=mda(y~.,data=dat,method=mars)
mda.pred2=predict(mda.fit2,newdata=dat[-train,])
table(dat[-train,"y"],mda.pred2)

library(penalizedLDA)
pda.fit=PenalizedLDA(x,y,K=1,lambda=.14)

xtest=matrix(rnorm(200*2),ncol=2)
xtest[1:100,]=xtest[1:100,]+2
xtest[101:150,]=xtest[101:150,]-2
ytest=c(rep(1,150),rep(2,50))
pda.pred=predict(pda.fit,xte=xtest)
mean((pda.pred$ypred-ytest)^2)
table(pda.pred$ypred,ytest)

pda.fit.cv=PenalizedLDA.cv(x,y)
pda.fit2=PenalizedLDA(x,y,lambda=pda.fit.cv$bestlambda,K=1)
pda.pred2=predict(pda.fit2,xte=xtest)
mean((pda.pred2$ypred-ytest)^2)
table(pda.pred2$ypred,ytest)




set.seed(1)
n <- 20
p <- 100
x <- matrix(rnorm(n*p), ncol=p)
y <- c(rep(1,5),rep(2,5),rep(3,10))
x[y==1,1:10] <- x[y==1,1:10] + 2
x[y==2,11:20] <- x[y==2,11:20] - 2
out <- PenalizedLDA(x,y,lambda=.14,K=2)
print(out)
plot(out)

