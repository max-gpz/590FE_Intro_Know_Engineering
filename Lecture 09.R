library(randomForest)
library(MASS)
library(tree)


set.seed(1)
train=sample(1:nrow(Boston),nrow(Boston)/2)
medv.tree=tree(medv~.,Boston,subset=train)
plot(medv.tree)

cv.medv=cv.tree(medv.tree)
cv.medv
names(cv.medv)
plot(cv.medv$size,cv.medv$dev,type="b")
plot(cv.medv$k,cv.medv$dev,type="b")

set.seed(1)
bag.boston=randomForest(medv~.,data=Boston,subset=train,mtry=13,importance=TRUE)
bag.boston
par(mfrow=c(1,2))
yhat.bag=predict(bag.boston,newdata=Boston[-train,])
boston.test=Boston[-train,"medv"]
plot(yhat.bag,boston.test)
abline(0,1)
mean((yhat.bag-boston.test)^2)

yhat2=predict(medv.tree,newdata=Boston[-train,])
plot(yhat2,boston.test)
abline(0,1)
mean((yhat2-boston.test)^2)

par(mfrow=c(1,1))
library(gbm)
set.seed(1)
boost.boston=gbm(medv~.,data=Boston[train,],distribution="gaussian",n.trees=5000,interaction.depth=4)
#if we instead used distribution "bernoulli" we would be dealing with classification data
summary(boost.boston)

par(mfrow=c(1,2))
plot(boost.boston,i="rm")
plot(boost.boston,i="lstat")

yhat.boost=predict(boost.boston,newdata=Boston[-train,],n.trees=5000)
mean((yhat.boost-boston.test)^2)
#running this again with a different lambda
boost.boston=gbm(medv~.,data=Boston[train,],distribution="gaussian",n.trees=5000,interaction.depth=4,shrinkage=.2,verbose=F)
yhat.boost=predict(boost.boston,newdata=Boston[-train,],n.trees=5000)
mean((yhat.boost-boston.test)^2)

