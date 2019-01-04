library(boot)
library(leaps)
library(glmnet)
library(caTools)
library(pls)
library(tree)
library(randomForest)
library(gbm)
library(gam)
library(splines)
library(MASS)

#Federal funds, often referred to as fed funds, are excess reserves that commercial banks and other financial institutions deposit at regional Federal Reserve banks; these funds can be lent, then, to other market participants with insufficient cash on hand to meet their lending and reserve needs.

#The discount rate is the interest rate charged to commercial banks and other depository institutions for loans received from the Federal Reserve's discount window. The discount rate also refers to the interest rate used in discounted cash flow analysis to determine the present value of future cash flows.

#The prime rate is the interest rate that commercial banks charge their most credit-worthy customers. Generally, a bank's best customers consist of large corporations. The prime interest rate, or prime lending rate, is largely determined by the federal funds rate, which is the overnight rate that banks use to lend to one another; the prime rate is also important for individual borrowers, as the prime rate directly affects the lending rates available for a mortgage, small business loan or personal loan.

getwd()
setwd("C:/Users/gang.ping.m.zhu/OneDrive - Accenture/Stevens/FE 590/A4")
int.rate.df <- read.csv("data.csv", header=TRUE, na.strings=-9999)
names(int.rate.df)
int.rate.clean.df <- int.rate.df[, c("month_1_nonfinancial_commercial_paper",
                                     "month_2_nonfinancial_commercial_paper", 
                                     "month_3_nonfinancial_commercial_paper",
                                     "month_1_financial_commercial_paper",                  
                                     "month_2_financial_commercial_paper",
                                     "month_3_financial_commercial_paper", 
                                     "prime_rate",
                                     "week_4_treasury_bill",
                                     "month_3_treasury_bill",
                                     "month_6_treasury_bill",
                                     "year_1_treasury_bill",
                                     "month_1_treasury_constant_maturity" ,
                                     "month_3_treasury_constant_maturity",
                                     "month_6_treasury_constant_maturity" ,
                                     "year_1_treasury_constant_maturity",
                                     "year_2_treasury_constant_maturity",
                                     "year_3_treasury_constant_maturity",
                                     "year_5_treasury_constant_maturity",
                                     "year_7_treasury_constant_maturity",
                                     "year_10_treasury_constant_maturity",
                                     "year_20_treasury_constant_maturity",
                                     "year_30_treasury_constant_maturity",
                                     "year_5_inflation_indexed_treasury_constant_maturity", 
                                     "year_7_inflation_indexed_treasury_constant_maturity",
                                     "year_10_inflation_indexed_treasury_constant_maturity",
                                     "year_20_inflation_indexed_treasury_constant_maturity", 
                                     "year_30_inflation_indexed_treasury_constant_maturity",
                                     "inflation_indexed_long_term_average")]
int.rate.clean.df <- na.omit(int.rate.clean.df)

regfit.full=regsubsets(prime_rate~.,data=int.rate.clean.df, nvmax = 15, really.big = T)
t(summary(regfit.full)$which)
reg.summary <- summary(regfit.full)
reg.summary
reg.summary$rsq

set.seed(33)
glm.fit1 <- glm(prime_rate~
                  month_2_nonfinancial_commercial_paper+
                  month_1_financial_commercial_paper+
                  month_6_treasury_bill+
                  year_3_treasury_constant_maturity+
                  year_5_treasury_constant_maturity+
                  year_7_treasury_constant_maturity+
                  year_10_treasury_constant_maturity+
                  year_20_treasury_constant_maturity+
                  year_30_treasury_constant_maturity+
                  year_5_inflation_indexed_treasury_constant_maturity+
                  year_7_inflation_indexed_treasury_constant_maturity+
                  year_10_inflation_indexed_treasury_constant_maturity+
                  year_20_inflation_indexed_treasury_constant_maturity+
                  inflation_indexed_long_term_average, data=int.rate.clean.df)
cv.err1 <- cv.glm(int.rate.clean.df, glm.fit1)
cv.err1$delta
# 0.001241602 0.001241588

glm.fit2 <- glm(prime_rate~
                  month_1_nonfinancial_commercial_paper+
                  year_5_treasury_constant_maturity+
                  year_7_treasury_constant_maturity+
                  year_10_treasury_constant_maturity+
                  year_20_treasury_constant_maturity+
                  year_30_treasury_constant_maturity+
                  year_5_inflation_indexed_treasury_constant_maturity+
                  year_7_inflation_indexed_treasury_constant_maturity+
                  year_10_inflation_indexed_treasury_constant_maturity+
                  year_20_inflation_indexed_treasury_constant_maturity+
                  inflation_indexed_long_term_average, data=int.rate.clean.df)
cv.err2 <- cv.glm(int.rate.clean.df, glm.fit2)
cv.err2$delta
# 0.001690060 0.001690047

glm.fit3 <- glm(prime_rate~
                  month_2_nonfinancial_commercial_paper+
                  month_1_financial_commercial_paper+
                  month_6_treasury_bill+
                  year_3_treasury_constant_maturity+
                  year_5_treasury_constant_maturity+
                  year_7_treasury_constant_maturity+
                  year_10_treasury_constant_maturity+
                  year_20_treasury_constant_maturity+
                  year_30_treasury_constant_maturity+
                  year_5_inflation_indexed_treasury_constant_maturity+
                  year_7_inflation_indexed_treasury_constant_maturity+
                  year_10_inflation_indexed_treasury_constant_maturity+
                  year_20_inflation_indexed_treasury_constant_maturity+
                  inflation_indexed_long_term_average+
                  poly(inflation_indexed_long_term_average, 2),data=int.rate.clean.df)
cv.err3 <- cv.glm(int.rate.clean.df, glm.fit3)
cv.err3$delta
#0.001186794 0.001186780

glm.fit4<- glm(prime_rate~
                  month_2_nonfinancial_commercial_paper+
                  month_1_financial_commercial_paper+
                  month_6_treasury_bill+
                  year_3_treasury_constant_maturity+
                  year_5_treasury_constant_maturity+
                  year_7_treasury_constant_maturity+
                  year_10_treasury_constant_maturity+
                  year_20_treasury_constant_maturity+
                  year_30_treasury_constant_maturity+
                  year_5_inflation_indexed_treasury_constant_maturity+
                  year_7_inflation_indexed_treasury_constant_maturity+
                  year_10_inflation_indexed_treasury_constant_maturity+
                  year_20_inflation_indexed_treasury_constant_maturity+
                  inflation_indexed_long_term_average+
                  poly(inflation_indexed_long_term_average, 2)+
                  poly(year_20_inflation_indexed_treasury_constant_maturity, 2),data=int.rate.clean.df)
cv.err4 <- cv.glm(int.rate.clean.df, glm.fit4)
cv.err4$delta
#0.001153627 0.001153613

anova(glm.fit1,glm.fit2,glm.fit3,glm.fit4)

# Transforming the information into a quanitive variable
new.col.transform <- data.frame(int.rate.clean.df$inflation_indexed_long_term_average)
new.col.transform <- data.frame(new.col.transform[-1,])
new.col.transform <- rbind(new.col.transform, c(0))
int.rate.clean.df$inflation_indexed_long_term_average_previous <- new.col.transform
int.rate.clean.df$direction <- ifelse(int.rate.clean.df$inflation_indexed_long_term_average>=int.rate.clean.df$inflation_indexed_long_term_average_previous, "Up", "Down")
# int.rate.clean.df$inflation_indexed_long_term_average_previous <- NULL

# Dividing into a training set and validation set
n = 1:floor(nrow(int.rate.clean.df)/2)
TrainingSet = int.rate.clean.df[n, ]
ValidationSet = int.rate.clean.df[ - n, ]
dim(TrainingSet)
names(TrainingSet)
dim(ValidationSet)
names(ValidationSet)


# Model 1 for Infation Indexed Long Term Average Direction (Quantative Variable)
# LDA for Inflaction Indexed Long Term Average Direction
inf.d.Val = ValidationSet$direction

inf.d.lda.fit1 <- lda(direction~
                  month_2_nonfinancial_commercial_paper+
                  month_1_financial_commercial_paper+
                  month_6_treasury_bill+
                  prime_rate+
                  year_3_treasury_constant_maturity+
                  year_5_treasury_constant_maturity+
                  year_7_treasury_constant_maturity+
                  year_10_treasury_constant_maturity+
                  year_20_treasury_constant_maturity+
                  year_30_treasury_constant_maturity+
                  year_5_inflation_indexed_treasury_constant_maturity+
                  year_7_inflation_indexed_treasury_constant_maturity+
                  year_10_inflation_indexed_treasury_constant_maturity+
                  year_20_inflation_indexed_treasury_constant_maturity+
                  inflation_indexed_long_term_average, data=TrainingSet)

inf.d.lda.pred=predict(inf.d.lda.fit1,ValidationSet)
inf.d.lda.class=inf.d.lda.pred$class
table(inf.d.lda.class,inf.d.Val)
inf.d.LDA = round(mean(inf.d.lda.class==inf.d.Val)*100, 4)
inf.d.LDA
#Success rate of 49.6088%

#QDA for Inflaction Indexed Long Term Average Direction
inf.d.qda.fit1 <- qda(direction~
                        month_2_nonfinancial_commercial_paper+
                        month_1_financial_commercial_paper+
                        month_6_treasury_bill+
                        prime_rate+
                        year_3_treasury_constant_maturity+
                        year_5_treasury_constant_maturity+
                        year_7_treasury_constant_maturity+
                        year_10_treasury_constant_maturity+
                        year_20_treasury_constant_maturity+
                        year_30_treasury_constant_maturity+
                        year_5_inflation_indexed_treasury_constant_maturity+
                        year_7_inflation_indexed_treasury_constant_maturity+
                        year_10_inflation_indexed_treasury_constant_maturity+
                        year_20_inflation_indexed_treasury_constant_maturity+
                        inflation_indexed_long_term_average, data=TrainingSet)

inf.d.qda.pred=predict(inf.d.qda.fit1,ValidationSet)
inf.d.qda.class=inf.d.qda.pred$class
table(inf.d.qda.class,inf.d.Val)
inf.d.QDA = round(mean(inf.d.qda.class==inf.d.Val)*100, 4)
inf.d.QDA
#Success rate of 52.7387%

#KNN for Inflaction Indexed Long Term Average Direction

var1 <- c("month_2_nonfinancial_commercial_paper",
         "month_1_financial_commercial_paper",
         "month_6_treasury_bill",
         "prime_rate",
         "year_3_treasury_constant_maturity",
         "year_5_treasury_constant_maturity",
         "year_7_treasury_constant_maturity",
         "year_10_treasury_constant_maturity",
         "year_20_treasury_constant_maturity",
         "year_30_treasury_constant_maturity",
         "year_5_inflation_indexed_treasury_constant_maturity",
         "year_7_inflation_indexed_treasury_constant_maturity",
         "year_10_inflation_indexed_treasury_constant_maturity",
         "year_20_inflation_indexed_treasury_constant_maturity",
         "inflation_indexed_long_term_average")
var2 <- c("direction")
train.var1 <- TrainingSet[var1]
test.var1 <- ValidationSet[var1]
test.var1 <- test.var1[var1]
train.dep1 <- TrainingSet[var2]
test.dep1 <- ValidationSet[var2]

KNN.Multi <- rep(NA,50)
for (i in 1:50){
  set.seed(1)
  inf.knn.pred <- knn(train.var1,test.var1,train.dep1$direction,k = i)
  KNN.Multi[i] <- mean(inf.knn.pred==test.dep1$direction)
}
KN <- c(1:50)
KNN.Multi.KN <- cbind(KNN.Multi,KN)
inf.knn <- KNN.Multi.KN[which.max(KNN.Multi), ]
inf.knn
inf.knn.NoLag <- round(mean(inf.knn[1])*100, 4)
inf.knn.NoLag

#Success rate of 51.1737% when K = 6

# Model 2 for Infation Indexed Long Term Average Direction
# LDA for Inflaction Indexed Long Term Average Direction

inf.d.lda.fit2 <- lda(direction~
                        month_1_nonfinancial_commercial_paper+
                        month_2_nonfinancial_commercial_paper+
                        month_2_financial_commercial_paper+
                        month_3_financial_commercial_paper+
                        prime_rate+
                        week_4_treasury_bill+
                        month_3_treasury_bill+
                        month_1_treasury_constant_maturity+
                        month_3_treasury_constant_maturity+
                        month_6_treasury_constant_maturity+
                        year_30_inflation_indexed_treasury_constant_maturity+
                        inflation_indexed_long_term_average, data=TrainingSet)

inf.d.lda.pred2 <- predict(inf.d.lda.fit2,ValidationSet)
inf.d.lda.class2 <- inf.d.lda.pred2$class
table(inf.d.lda.class2,inf.d.Val)
inf.d.LDA2 = round(mean(inf.d.lda.class2==inf.d.Val)*100, 4)
inf.d.LDA2
#Success rate of 48.5133%

#QDA for Inflaction Indexed Long Term Average Direction
inf.d.qda.fit2 <- qda(direction~
                        month_1_nonfinancial_commercial_paper+
                        month_2_nonfinancial_commercial_paper+
                        month_2_financial_commercial_paper+
                        month_3_financial_commercial_paper+
                        prime_rate+
                        week_4_treasury_bill+
                        month_3_treasury_bill+
                        month_1_treasury_constant_maturity+
                        month_3_treasury_constant_maturity+
                        month_6_treasury_constant_maturity+
                        year_30_inflation_indexed_treasury_constant_maturity+
                        inflation_indexed_long_term_average, data=TrainingSet)

inf.d.qda.pred2 <- predict(inf.d.qda.fit2,ValidationSet)
inf.d.qda.class2 <- inf.d.qda.pred2$class
table(inf.d.qda.class2,inf.d.Val)
inf.d.QDA2 = round(mean(inf.d.qda.class2==inf.d.Val)*100, 4)
inf.d.QDA2
#Success rate of 51.9562%

#KNN for Inflaction Indexed Long Term Average Direction

var1.2 <- c("month_1_nonfinancial_commercial_paper",
         "month_2_nonfinancial_commercial_paper",
         "month_2_financial_commercial_paper",
         "month_3_financial_commercial_paper",
         "prime_rate",
         "week_4_treasury_bill",
         "month_3_treasury_bill",
         "month_1_treasury_constant_maturity",
         "month_3_treasury_constant_maturity",
         "month_6_treasury_constant_maturity",
         "year_30_inflation_indexed_treasury_constant_maturity",
         "inflation_indexed_long_term_average")

var2.2 <- c("direction")

train.var1.2 <- TrainingSet[var1.2]
test.var1.2 <- ValidationSet[var1.2]
test.var1.2 <- test.var1.2[var1.2]
train.dep1.2 <- TrainingSet[var2.2]
test.dep1.2 <- ValidationSet[var2.2]

KNN.Multi.2 <- rep(NA,50)
for (i in 1:50){
  set.seed(1)
  inf.knn.pred <- knn(train.var1.2,test.var1.2,train.dep1.2$direction,k = i)
  KNN.Multi.2[i] <- mean(inf.knn.pred==test.dep1.2$direction)
}
KN2 <- c(1:50)
KNN.Multi.KN2 <- cbind(KNN.Multi.2,KN2)
inf.knn2 <- KNN.Multi.KN2[which.max(KNN.Multi.2), ]
inf.knn2
inf.knn.NoLag2 <- round(mean(inf.knn2[1])*100, 4)
inf.knn.NoLag2

#Success rate of 51.7997% when K = 32

# Model 3 for Infation Indexed Long Term Average Direction
# LDA for Inflaction Indexed Long Term Average Direction

inf.d.lda.fit3 <- lda(direction~
                        prime_rate+
                        week_4_treasury_bill+
                        month_3_treasury_bill+
                        month_6_treasury_bill+
                        year_1_treasury_bill+
                        month_1_treasury_constant_maturity+
                        month_3_treasury_constant_maturity+
                        month_6_treasury_constant_maturity+
                        year_1_treasury_constant_maturity+
                        year_2_treasury_constant_maturity+
                        year_3_treasury_constant_maturity+
                        year_5_treasury_constant_maturity+
                        year_7_treasury_constant_maturity+
                        year_10_treasury_constant_maturity+
                        year_20_treasury_constant_maturity+
                        year_30_treasury_constant_maturity+
                        year_5_inflation_indexed_treasury_constant_maturity+
                        year_7_inflation_indexed_treasury_constant_maturity+
                        year_10_inflation_indexed_treasury_constant_maturity+
                        year_20_inflation_indexed_treasury_constant_maturity+
                        year_30_inflation_indexed_treasury_constant_maturity+
                        inflation_indexed_long_term_average, data=TrainingSet)

inf.d.lda.pred3 <- predict(inf.d.lda.fit3,ValidationSet)
inf.d.lda.class3 <- inf.d.lda.pred3$class
table(inf.d.lda.class3,inf.d.Val)
inf.d.LDA3 <- round(mean(inf.d.lda.class3==inf.d.Val)*100, 4)
inf.d.LDA3
#Success rate of 51.0172%

#QDA for Inflaction Indexed Long Term Average Direction
inf.d.qda.fit3 <- qda(direction~
                        prime_rate+
                        week_4_treasury_bill+
                        month_3_treasury_bill+
                        month_6_treasury_bill+
                        year_1_treasury_bill+
                        month_1_treasury_constant_maturity+
                        month_3_treasury_constant_maturity+
                        month_6_treasury_constant_maturity+
                        year_1_treasury_constant_maturity+
                        year_2_treasury_constant_maturity+
                        year_3_treasury_constant_maturity+
                        year_5_treasury_constant_maturity+
                        year_7_treasury_constant_maturity+
                        year_10_treasury_constant_maturity+
                        year_20_treasury_constant_maturity+
                        year_30_treasury_constant_maturity+
                        year_5_inflation_indexed_treasury_constant_maturity+
                        year_7_inflation_indexed_treasury_constant_maturity+
                        year_10_inflation_indexed_treasury_constant_maturity+
                        year_20_inflation_indexed_treasury_constant_maturity+
                        year_30_inflation_indexed_treasury_constant_maturity+
                        inflation_indexed_long_term_average, data=TrainingSet)

inf.d.qda.pred3 <- predict(inf.d.qda.fit3,ValidationSet)
inf.d.qda.class3 <- inf.d.qda.pred3$class
table(inf.d.qda.class3,inf.d.Val)
inf.d.QDA3 = round(mean(inf.d.qda.class3==inf.d.Val)*100, 4)
inf.d.QDA3
#Success rate of 50.3912%

#KNN for Inflaction Indexed Long Term Average Direction

var1.3 <- c("prime_rate",
             "week_4_treasury_bill",
             "month_3_treasury_bill",
             "month_6_treasury_bill",
             "year_1_treasury_bill",
             "month_1_treasury_constant_maturity" ,
             "month_3_treasury_constant_maturity",
             "month_6_treasury_constant_maturity" ,
             "year_1_treasury_constant_maturity",
             "year_2_treasury_constant_maturity",
             "year_3_treasury_constant_maturity",
             "year_5_treasury_constant_maturity",
             "year_7_treasury_constant_maturity",
             "year_10_treasury_constant_maturity",
             "year_20_treasury_constant_maturity",
             "year_30_treasury_constant_maturity",
             "year_5_inflation_indexed_treasury_constant_maturity", 
             "year_7_inflation_indexed_treasury_constant_maturity",
             "year_10_inflation_indexed_treasury_constant_maturity",
             "year_20_inflation_indexed_treasury_constant_maturity", 
             "year_30_inflation_indexed_treasury_constant_maturity",
             "inflation_indexed_long_term_average")

var2.3 <- c("direction")
train.var1.3 <- TrainingSet[var1.3]
test.var1.3 <- ValidationSet[var1.3]
test.var1.3 <- test.var1.3[var1.3]
train.dep1.3 <- TrainingSet[var2.3]
test.dep1.3 <- ValidationSet[var2.3]

KNN.Multi.3 <- rep(NA,50)
for (i in 1:50){
  set.seed(1)
  inf.knn.pred <- knn(train.var1.3,test.var1.3,train.dep1.3$direction,k = i)
  KNN.Multi.3[i] <- mean(inf.knn.pred==test.dep1.3$direction)
}
KN3 <- c(1:50)
KNN.Multi.KN3 <- cbind(KNN.Multi.3,KN3)
inf.knn3 <- KNN.Multi.KN3[which.max(KNN.Multi.3), ]
inf.knn3
inf.knn.NoLag3 <- round(mean(inf.knn3[1])*100, 4)
inf.knn.NoLag3

#Success rate of 51.4867% when K = 15


# Model 4 for Infation Indexed Long Term Average Direction
# LDA for Inflaction Indexed Long Term Average Direction

inf.d.lda.fit4 <- lda(direction~
                        prime_rate+
                        week_4_treasury_bill+
                        month_3_treasury_bill+
                        month_6_treasury_bill+
                        year_1_treasury_bill+
                        month_1_treasury_constant_maturity+
                        month_3_treasury_constant_maturity+
                        month_6_treasury_constant_maturity+
                        year_1_treasury_constant_maturity+
                        year_2_treasury_constant_maturity+
                        year_3_treasury_constant_maturity+
                        year_5_treasury_constant_maturity+
                        year_7_treasury_constant_maturity+
                        year_10_treasury_constant_maturity+
                        year_20_treasury_constant_maturity+
                        year_30_treasury_constant_maturity, data=TrainingSet)

inf.d.lda.pred4 <- predict(inf.d.lda.fit4,ValidationSet)
inf.d.lda.class4 <- inf.d.lda.pred4$class
table(inf.d.lda.class4,inf.d.Val)
inf.d.LDA4 <- round(mean(inf.d.lda.class4==inf.d.Val)*100, 4)
inf.d.LDA4
#Success rate of 46.4789%

#QDA for Inflaction Indexed Long Term Average Direction
inf.d.qda.fit4 <- qda(direction~
                        prime_rate+
                        week_4_treasury_bill+
                        month_3_treasury_bill+
                        month_6_treasury_bill+
                        year_1_treasury_bill+
                        month_1_treasury_constant_maturity+
                        month_3_treasury_constant_maturity+
                        month_6_treasury_constant_maturity+
                        year_1_treasury_constant_maturity+
                        year_2_treasury_constant_maturity+
                        year_3_treasury_constant_maturity+
                        year_5_treasury_constant_maturity+
                        year_7_treasury_constant_maturity+
                        year_10_treasury_constant_maturity+
                        year_20_treasury_constant_maturity+
                        year_30_treasury_constant_maturity, data=TrainingSet)

inf.d.qda.pred4 <- predict(inf.d.qda.fit4,ValidationSet)
inf.d.qda.class4 <- inf.d.qda.pred4$class
table(inf.d.qda.class4,inf.d.Val)
inf.d.QDA4 = round(mean(inf.d.qda.class4==inf.d.Val)*100, 4)
inf.d.QDA4
#Success rate of 48.3568%

#KNN for Inflaction Indexed Long Term Average Direction

var1.4 <- c("prime_rate",
            "week_4_treasury_bill",
            "month_3_treasury_bill",
            "month_6_treasury_bill",
            "year_1_treasury_bill",
            "month_1_treasury_constant_maturity" ,
            "month_3_treasury_constant_maturity",
            "month_6_treasury_constant_maturity" ,
            "year_1_treasury_constant_maturity",
            "year_2_treasury_constant_maturity",
            "year_3_treasury_constant_maturity",
            "year_5_treasury_constant_maturity",
            "year_7_treasury_constant_maturity",
            "year_10_treasury_constant_maturity",
            "year_20_treasury_constant_maturity",
            "year_30_treasury_constant_maturity")

var2.4 <- c("direction")
train.var1.4 <- TrainingSet[var1.4]
test.var1.4 <- ValidationSet[var1.4]
test.var1.4 <- test.var1.4[var1.4]
train.dep1.4 <- TrainingSet[var2.4]
test.dep1.4 <- ValidationSet[var2.4]

KNN.Multi.4 <- rep(NA,50)
for (i in 1:50){
  set.seed(1)
  inf.knn.pred <- knn(train.var1.4,test.var1.4,train.dep1.4$direction,k = i)
  KNN.Multi.4[i] <- mean(inf.knn.pred==test.dep1.4$direction)
}
KN4 <- c(1:50)
KNN.Multi.KN4 <- cbind(KNN.Multi.4,KN4)
inf.knn4 <- KNN.Multi.KN4[which.max(KNN.Multi.4), ]
inf.knn4
inf.knn.NoLag4 <- round(mean(inf.knn4[1])*100, 4)
inf.knn.NoLag4

#Success rate of 49.9218% when K = 18
