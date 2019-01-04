
#ppr(formula, data, weights, subset, na.action,
#    contrasts = NULL, ..., model = FALSE)

#ppr(x, y, weights = rep(1,n),
#    ww = rep(1,q), nterms, max.terms = nterms, optlevel = 2,
#    sm.method = c("supsmu", "spline", "gcvspline"),
#    bass = 0, span = 0, df = 5, gcvpen = 1, ...)






library(MASS)

set.seed(10)
train=sample(1:nrow(Boston),nrow(Boston)/2)


bost.ppr=ppr(medv~.,data=Boston[train,],nterms=5,max.terms=50)
bost.ppr
summary(bost.ppr)



bost.pred=predict(bost.ppr,newdata=Boston[-train,])
boston.test=Boston[-train,"medv"]

mean((bost.pred-boston.test)^2)

bost2.ppr=ppr(medv~lstat+rm+tax+ptratio,data=Boston[train,],nterms=4,max.terms=8)


bost2.ppr
summary(bost2.ppr)


bost2.pred=predict(bost2.ppr,newdata=Boston[-train,])


mean((bost2.pred-boston.test)^2)


bost3.ppr=ppr(medv~.,data=Boston[train,],nterms=2,max.terms=10)


bost3.ppr
summary(bost3.ppr)


bost3.pred=predict(bost3.ppr,newdata=Boston[-train,])


mean((bost3.pred-boston.test)^2)


#Neural Networks


#Example taken from http://gekkoquant.com/2012/05/26/neural-networks-with-r-simple-example/
library("neuralnet")

#Going to create a neural network to perform sqare rooting
#Type ?neuralnet for more information on the neuralnet library

#Generate 50 random numbers uniformly distributed between 0 and 100
#And store them as a dataframe
traininginput <-  as.data.frame(runif(50, min=0, max=100))
trainingoutput <- sqrt(traininginput)

#Column bind the data into one variable
trainingdata <- cbind(traininginput,trainingoutput)
colnames(trainingdata) <- c("Input","Output")

#Train the neural network
#Going to have 10 hidden layers
#Threshold is a numeric value specifying the threshold for the partial
#derivatives of the error function as stopping criteria.
net.sqrt <- neuralnet(Output~Input,trainingdata, hidden=10, threshold=0.01)
print(net.sqrt)

#Plot the neural network
plot(net.sqrt)

#Test the neural network on some training data
testdata <- as.data.frame((1:10)^2) #Generate some squared numbers
net.results <- compute(net.sqrt, testdata) #Run them through the neural network

#Lets see what properties net.sqrt has
ls(net.results)

#Lets see the results
print(net.results$net.result)
print(net.results$neurons)

#Lets display a better version of the results
cleanoutput <- cbind(testdata,sqrt(testdata),
                     as.data.frame(net.results$net.result))
colnames(cleanoutput) <- c("Input","Expected Output","Neural Net Output")
print(cleanoutput)





library(neuralnet)

?neuralnet

#bost3.nn.car=train(medv~.,data=Boston[train,],method="neuralnet")
#bost3.nn.car.pred=predict(bost3.nn.car,newdata=Boston[-train,])
#mean((bost3.nn.car.pred-boston.test)^2)

Scaled=Boston[,c(14,13,6,10,11)]
Scaled$lstat=(Scaled$lstat-mean(Scaled$lstat))/sd(Scaled$lstat)
Scaled$rm=(Scaled$rm-mean(Scaled$rm))/sd(Scaled$rm)
Scaled$tax=(Scaled$tax-mean(Scaled$tax))/sd(Scaled$tax)
Scaled$ptratio=(Scaled$ptratio-mean(Scaled$ptratio))/sd(Scaled$ptratio)

bost4.nn=neuralnet(medv~lstat+rm+tax+ptratio,data=Scaled[train,],hidden=5,threshold=.1)
#print(bost4.nn)
plot(bost4.nn)

bost4.nn.pred=compute(bost4.nn,Scaled[-train,c(2,3,4,5)])
summary(bost4.nn.pred)
names(bost4.nn.pred)
print(bost4.nn.pred$neurons)
print(bost4.nn.pred$net.result)

mean((bost4.nn.pred$net.result-boston.test)^2)

library(nnet)

#nnet(formula, data, weights, ...,
#     subset, na.action, contrasts = NULL)

#nnet(x, y, weights, size, Wts, mask,
#     linout = FALSE, entropy = FALSE, softmax = FALSE,
#     censored = FALSE, skip = FALSE, rang = 0.7, decay = 0,
#     maxit = 100, Hess = FALSE, trace = TRUE, MaxNWts = 1000,
#     abstol = 1.0e-4, reltol = 1.0e-8, ...)


bost.nn=nnet(medv~.,data=Scaled[train,],size=5)
summary(bost.nn)
#plot(bost.nn)
bost.nn.pred=predict(bost.nn,newdata=Scaled[-train,])

mean((bost.nn.pred-Scaled[-train,1])^2)


bost2.nn=nnet(medv~.,data=Boston[train,],size=10)
bost2.nn.pred=predict(bost2.nn,newdata=Boston[-train,])
mean((bost2.nn.pred-boston.test)^2)

n=length(train)
bost3.nn=nnet(medv~.,data=Boston[train,],size=13,weights=rep(.01,n),decay=5e-4)
bost3.nn.pred=predict(bost3.nn,newdata=Boston[-train,])
mean((bost3.nn.pred-boston.test)^2)

bost3.nn


#investigate caret
library(robustbase)
library(caret)

bost.avn=avNNet(medv~lstat+rm+tax+ptratio,size=5,data=Boston[train,],repeats=5)
bost.avn.pred=predict(bost.avn,Boston[-train,c(13,6,10,11)])
mean((bost.avn.pred-boston.test)^2)
