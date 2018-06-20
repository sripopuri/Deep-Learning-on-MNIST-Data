#Clearing my work space
rm(list = ls()) 

#Loading the required libraries
library(ggplot2) 
library(randomForest)
library(caret)

# Setting the required working directory
setwd("D:/New folder/Kaggle/Digit Recognizer")

# Reading the data from the working directory
data = read.csv("train.csv",header = TRUE)
dim(data)
# 42000 rows and 785 columns

# Checking if there are any missing values
sum(is.na(data))
# There are no missing values identified

# str(data) --> You will get a huge structure of 785 columns
head(order(colnames(data)))
str(data[,1:6])

# Since label is int, we need to convert it to factor
data$label = as.factor(data$label)

class = character(0)
# Lets see what are the data types of the rest of 784 columns
for(i in 1:ncol(data)){
  class[i] = class(data[,i])
}

unique(class)
# Hence all the other columns are integers in the data

# checking the entries for each label
table(data$label)

# plotting the data
ggplot(data = data,aes(x = label, fill = as.numeric(label))) +
  geom_bar(stat = "count", color = "white") +
  scale_fill_gradient(low = "lightblue", high = "pink", guide = F) +
  labs(title = "counts of records label wise", x = "label")

# creating the images
set.seed(1601)
sample = sample(1:nrow(data),30)

dev.off()
par(mfrow=c(3,10),mar=c(.1,.1,.1,.1))

# Re-creating the digits using the image function.
# This is done by unwrapping the 784 features of the 
# data set given

for(j in 1:length(sample)){
  first = data[sample[j],-1]
  result = matrix(NA,nrow = 28, ncol = 28)
  for(i in 1:28){
    result[,29-i] = as.matrix(first[,((28*(i-1)) + 1):(28*i)])
  }
  
  image(result,col=grey.colors(225),axes=F)
}

data[sample,"label"]
# The labels in the data set are matching with that of the 
# labels in the images generated

# trying to find the columns which are having no value
# in the entire rows of the dataset
sums = integer(0)

for(i in 2:ncol(data)){
  sums[i-1] = sum(data[,i])
}

length(sums)
length(sums[sums == 0])
# There are 76 columns which have no value in the 
# entire rows of the data set

# Lets check which are those columns
to_exclude = which(sums == 0)
to_exclude

# These columns can be excluded from our classifier
# since these columns are always 0, no matter what
# the observation is
var = apply(data,2,var)
summary(var)

temp = data[,-1]
train = temp[,-to_exclude]

#####

# Try to remove the near zero variance
# predictors from the dataset for predicting
# the labels

#####
# Given Test data
test = read.csv('test.csv',header = TRUE)
test = test[,-to_exclude]

# # Creating training and test samples from given train data
# index = createDataPartition(data$label, 
#                             p = 0.75, 
#                             times = 1, 
#                             list = F)
# 
# train_model = data_clean[index,]
# test_model = data_clean[-index,]

###### Use classifier now  ######
label = as.factor(data[,1])
rf_model = randomForest(train, label, test, ntree = 500)
length(rf_model$predicted)
length(rf_model$test$predicted)


confusionMatrix(rf_model$predicted,label)
# 96.77% accuracy

csv = data.frame(ImageId = 1:28000, Label = rf_model$test$predicted)
submission = write.csv(csv,'Submission.csv',row.names = FALSE)
