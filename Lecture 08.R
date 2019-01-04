#GAMS Lab
library(ISLR)
library(splines)
attach(Wage)
gam1=lm(wage~ns(year,4)+ns(age,5)+education,data=Wage)

library(gam)
gam.m3=gam(wage~s(year,4)+s(age,5)+education,data=Wage)
par(mfrow=c(1,3))
plot(gam.m3,se=TRUE,col="blue")
plot.gam(gam1,se=TRUE,col="red")

gam.m1=gam(wage~s(age,5)+education,data=Wage)
gam.m2=gam(wage~year+s(age,5)+education,data=Wage)
anova(gam.m1,gam.m2,gam.m3,test="F")

summary(gam.m3)

preds=predict(gam.m2,newdata=Wage)
num=length(preds)

# Mean sqaured error for M2
sum((Wage$wage-preds)^2)/num

# Mean squared error for M1
preds=predict(gam.m1,newdata=Wage)
num=length(preds)

sum((Wage$wage-preds)^2)/num

# Mean squared error for M3
preds=predict(gam.m3,newdata=Wage)
num=length(preds)

sum((Wage$wage-preds)^2)/num

# Determine if our training data represents a different perspective on the model
train <- sample(3000, 1500, replace = FALSE)

gam.m3=gam(wage~s(year,4)+s(age,5)+education,data=Wage[train,])
gam.m1=gam(wage~s(age,5)+education,data=Wage[train,])
gam.m2=gam(wage~year+s(age,5)+education,data=Wage[train,])
anova(gam.m1,gam.m2,gam.m3,test="F")

preds=predict(gam.m2,newdata=Wage[-train,])
num=length(preds)
sum((Wage[-train,]$wage-preds)^2)/num

preds=predict(gam.m3,newdata=Wage[-train,])
num=length(preds)
sum((Wage[-train,]$wage-preds)^2)/num

preds=predict(gam.m1,newdata=Wage[-train,])
num=length(preds)
sum((Wage[-train,]$wage-preds)^2)/num

#Tree Lab

library(tree)
attach(Carseats)
High=ifelse(Sales<=8,"No","Yes")

Carseats=data.frame(Carseats,High)
tree.carseats=tree(High~.-Sales,Carseats)
summary(tree.carseats)

par(mfrow=c(1,1))

plot(tree.carseats)
text(tree.carseats,pretty=0)

tree.carseats

set.seed(2)
train=sample(1:nrow(Carseats),200)
Carseats.test=Carseats[-train,]
High.test=High[-train]
tree.carseats=tree(High~.-Sales,Carseats,subset=train)
tree.pred=predict(tree.carseats,Carseats.test,type="class")
table(tree.pred,High.test)

set.seed(3)
cv.carseats=cv.tree(tree.carseats,FUN=prune.misclass)
names(cv.carseats)
cv.carseats

par(mfrow=c(1,2))
plot(cv.carseats$size,cv.carseats$dev,type="b")
plot(cv.carseats$k,cv.carseats$dev,type="b")

prune.carseats=prune.misclass(tree.carseats,best=9)
plot(prune.carseats)
text(prune.carseats,pretty=0)
tree.pred=predict(prune.carseats,Carseats.test,type="class")
table(tree.pred,High.test)

prune.carseats=prune.misclass(tree.carseats,best=15)
plot(prune.carseats)
text(prune.carseats,pretty=0)
tree.pred=predict(prune.carseats,Carseats.test,type="class")
table(tree.pred,High.test)


library(ISLR)
par(mfrow=c(1,1))
salary.available <- !is.na(Hitters$Salary)
Hitters <- Hitters[salary.available,]
Hitters$Salary <- log(Hitters$Salary)
plot(Hitters$Years, Hitters$Hits, pch=19, xlab="Years", ylab="Hits",col=rainbow(7)[cut(Hitters$Salary, 7)],main="Baseball Player Salary")
sal.tree <- tree(Salary ~ Years + Hits, Hitters)
pruned.tree <- prune.tree(sal.tree, best=3)
plot(pruned.tree)
text(pruned.tree)

plot(Hitters$Years, Hitters$Hits,
      pch=19, xlab="Years", ylab="Hits",
      col=rainbow(7)[cut(Hitters$Salary, 7)],
      main="Baseball Player Salary")
partition.tree(sal.tree, ordvars=c("Years", "Hits"), add=TRUE)

cv.base=cv.tree(sal.tree)
cv.base

best.prune=prune.tree(sal.tree,best=4)
plot(best.prune)
text(best.prune)
plot(Hitters$Years, Hitters$Hits,
     pch=19, xlab="Years", ylab="Hits",
     col=rainbow(7)[cut(Hitters$Salary, 7)],
     main="Baseball Player Salary")
partition.tree(best.prune, ordvars=c("Years", "Hits"), add=TRUE)
#regression trees

library(MASS)
set.seed(16)
train=sample(1:nrow(Boston),nrow(Boston)/2)
tree.boston=tree(medv~.,Boston,subset=train)
summary(tree.boston)

plot(tree.boston)
text(tree.boston,pretty=0)

cv.boston=cv.tree(tree.boston)
cv.boston
plot(cv.boston$size,cv.boston$dev,type='b')

prune.boston=prune.tree(tree.boston,best=8)
plot(prune.boston)
text(prune.boston,pretty=0)

yhat=predict(prune.boston,newdata=Boston[-train,])
boston.test=Boston[-train,"medv"]
mean((yhat-boston.test)^2)

yhat=predict(tree.boston,newdata=Boston[-train,])
boston.test=Boston[-train,"medv"]
plot(yhat,boston.test)
abline(0,1)
mean((yhat-boston.test)^2)

means=seq(1,10,1)
for (i in 10:2)
{
prune.boston=prune.tree(tree.boston,best=i)
#plot(prune.boston)
#text(prune.boston,pretty=0)

yhat=predict(prune.boston,newdata=Boston[-train,])
boston.test=Boston[-train,"medv"]
means[i]=mean((yhat-boston.test)^2)
}
