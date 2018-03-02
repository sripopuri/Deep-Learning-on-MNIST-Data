rm(list = ls())

setwd("D:/New folder/Deep Learning")
train = read.csv("train.csv")
dim(train)
# 28*28 + 1 columns

head(colnames(train))
tail(colnames(train))

str(train[,1:4])
# label is int
# converting it into factor

train[,1] = factor(train[,1],levels = 0:9)
str(train[,1:4])

i = 1:5000
digits.x = train[i,-1]
digits.y = train[i,1]

barplot(table(digits.y), 
        main = "Frequency plot of Digits")

library(nnet)
library(caret)

## 5 Neurons
set.seed(1234)
digits = train(x = digits.x, y = digits.y, method = "nnet",
               tuneGrid = expand.grid(.size = c(5),.decay = 0.1),
               trControl = trainControl(method = "none"),MaxNWts = 10000,
               maxit = 100)

digits.yhat1 = predict(digits)
barplot(table(digits.yhat1))

caret::confusionMatrix(digits.yhat1,digits.y)

## 10 Neurons
set.seed(1234)
digits2 = train(x = digits.x, y = digits.y, method = "nnet",
               tuneGrid = expand.grid(.size = c(10),.decay = 0.1),
               trControl = trainControl(method = "none"),MaxNWts = 50000,
               maxit = 100)


digits.yhat2 = predict(digits2)
barplot(table(digits.yhat2))

confusionMatrix(digits.yhat2,digits.y)


## 40 Neurons

set.seed(1234)
digits3 = train(x = digits.x, y = digits.y, method = "nnet",
                tuneGrid = expand.grid(.size = c(40),.decay = 0.1),
                trControl = trainControl(method = "none"),MaxNWts = 50000,
                maxit = 100)


digits.yhat3 = predict(digits3)
barplot(table(digits.yhat3))

confusionMatrix(digits.yhat3,digits.y)


library(RSNNS)
####  RSNNS Package
head(decodeClassLabels(digits.y))
h = decodeClassLabels(digits.y)

set.seed(1234)
digits4 = mlp(as.matrix(digits.x),
              decodeClassLabels(digits.y),
              size = 40,
              learnFunc = "Rprop",
              shufflePatterns = FALSE,
              maxit = 60)

digits.yhat4 = fitted.values(digits4)
digits.yhat4 = encodeClassLabels(digits.yhat4)
barplot(table(digits.yhat4))

caret::confusionMatrix(digits.yhat4,as.integer(digits.y))

digits.yhat4.insample = fitted.values(digits4)
head(round(digits.yhat4.insample,3))

table(encodeClassLabels(digits.yhat4.insample, method = "WTA",
                        l = 0, h = 0))
table(encodeClassLabels(digits.yhat4.insample, method = "WTA",
                        l = 0, h = 0.5))
table(encodeClassLabels(digits.yhat4.insample, method = "WTA",
                        l = 0.2, h = 0.5))
table(encodeClassLabels(digits.yhat4.insample, method = "402040",
                        l = 0.4, h = 0.6))

i2 = 5001:10000
digits.yhat4.pred = predict(digits4, as.matrix(train[i2,-1]))
table(encodeClassLabels(digits.yhat4.pred,method = "WTA",l=0,h=0))

caret::confusionMatrix(encodeClassLabels(digits.yhat4.pred),as.integer(train[i2,1]))
