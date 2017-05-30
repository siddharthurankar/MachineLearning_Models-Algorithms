library(ggplot2)
library(dplyr)
library(class)

german <- read.csv("german_credit_data1.csv")
print(german)
summary(german)

ggplot(german,
       aes(y = Credit.amount, x = Age)) +
  geom_point(aes(color = Credit.Risks, shape = Sex)) 
#plot of age vs credit.amount for the german data, with the color as Risks and shape as the sex


n.points <- 1000 # number of rows in the dataset
sampling.rate <- 0.8


# we need the number of points in the test set to calculate
# the misclassification rate
num.test.set.labels <- n.points * (1 - sampling.rate)

# randomly sample which rows will go in the training set
training <- sample(1:n.points, sampling.rate * n.points,
                   replace=FALSE)

train <- subset(german[training, ], select = c(Age, Credit.amount))
# define the training set to be those rows
# the other rows are going into the test set
testing <- setdiff(1:n.points, training)
# define the test set to be the other rows
test <- subset(german[testing, ], select = c(Age, Credit.amount))

cl <- german$Credit.Risks[training]
# this is the subset of labels for the training set

true.labels <- german$Credit.Risks[testing]
# subset of labels for the test set, we're withholding these

knn (train, test, cl, k=3)

# we'll loop through and see what the misclassification rate
# is for different values of k
sampling_rate_80 <- c()
for (k in 1:20) {
  print(k)
  predicted.labels <- knn(train, test, cl, k)
  # We're using the R function knn()
  num.incorrect.labels <- sum(predicted.labels != true.labels)
  misclassification.rate <- num.incorrect.labels /
    num.test.set.labels
  print(misclassification.rate)
  sampling_rate_80 <- c(sampling_rate_80,misclassification.rate)
}

ggplot(german,
       aes(y = Credit.amount, x = Age)) +
  geom_point(aes(color = Credit.Risks, shape = Sex)) 
#plot of age vs credit.amount for the german data, with the color as Risks and shape as the sex

#now predict using k with lowest mis-classification rate

testcase<-c(57,1000)
knn(train,testcase,cl,k=5)
knn(train,testcase,cl,k=6)
knn(train,testcase,cl,k=7)
knn(train,testcase,cl,k=8)
knn(train,testcase,cl,k=9)
knn(train,testcase,cl,k=10)
knn(train,testcase,cl,k=11)
knn(train,testcase,cl,k=12)
knn(train,testcase,cl,k=13)
knn(train,testcase,cl,k=14)
knn(train,testcase,cl,k=15)
knn(train,testcase,cl,k=16)
knn(train,testcase,cl,k=17)
knn(train,testcase,cl,k=18)
knn(train,testcase,cl,k=19)
knn(train,testcase,cl,k=20)

testcase<-c(27,2300)
knn(train,testcase,cl,k=5)
knn(train,testcase,cl,k=6)
knn(train,testcase,cl,k=7)
knn(train,testcase,cl,k=8)
knn(train,testcase,cl,k=9)
knn(train,testcase,cl,k=10)
knn(train,testcase,cl,k=11)
knn(train,testcase,cl,k=12)
knn(train,testcase,cl,k=13)
knn(train,testcase,cl,k=14)
knn(train,testcase,cl,k=15)
knn(train,testcase,cl,k=16)
knn(train,testcase,cl,k=17)
knn(train,testcase,cl,k=18)
knn(train,testcase,cl,k=19)
knn(train,testcase,cl,k=20)
#high density area
testcase<-c(43,5000)
knn(train,testcase,cl,k=5)
knn(train,testcase,cl,k=6)
knn(train,testcase,cl,k=7)
knn(train,testcase,cl,k=8)
knn(train,testcase,cl,k=9)
knn(train,testcase,cl,k=10)
knn(train,testcase,cl,k=11)
knn(train,testcase,cl,k=12)
knn(train,testcase,cl,k=13)
knn(train,testcase,cl,k=14)
knn(train,testcase,cl,k=15)
knn(train,testcase,cl,k=16)
knn(train,testcase,cl,k=17)
knn(train,testcase,cl,k=18)
knn(train,testcase,cl,k=19)
knn(train,testcase,cl,k=20)
#not that high of a density area but still changes with the k value

sampling.rate <- 0.2
#change sampling.rate to 0.2 or 80%

# we need the number of points in the test set to calculate
# the misclassification rate
num.test.set.labels <- n.points * (1 - sampling.rate)

# randomly sample which rows will go in the training set
training <- sample(1:n.points, sampling.rate * n.points,
                   replace=FALSE)

train <- subset(german[training, ], select = c(Age, Credit.amount))
# define the training set to be those rows
# the other rows are going into the test set
testing <- setdiff(1:n.points, training)
# define the test set to be the other rows
test <- subset(german[testing, ], select = c(Age, Credit.amount))

cl <- german$Credit.Risks[training]
# this is the subset of labels for the training set

true.labels <- german$Credit.Risks[testing]
# subset of labels for the test set, we're withholding these

knn (train, test, cl, k=3)

# we'll loop through and see what the misclassification rate
# is for different values of k
sampling_rate_20 <- c()
for (k in 1:20) {
  print(k)
  predicted.labels <- knn(train, test, cl, k)
  # We're using the R function knn()
  num.incorrect.labels <- sum(predicted.labels != true.labels)
  misclassification.rate <- num.incorrect.labels /
    num.test.set.labels
  print(misclassification.rate)
  sampling_rate_20 <- c(sampling_rate_20,misclassification.rate)
  
}

ggplot(german,
       aes(y = Credit.amount, x = Age)) +
  geom_point(aes(color = Credit.Risks, shape = Sex)) 
#plot of age vs credit.amount for the german data, with the color as Risks and shape as the sex


#now predict using k with lowest mis-classification rate
testcase<-c(30,2500)
knn(train,testcase,cl,k=5)
knn(train,testcase,cl,k=6)
knn(train,testcase,cl,k=7)
knn(train,testcase,cl,k=8)
knn(train,testcase,cl,k=9)
knn(train,testcase,cl,k=10)
knn(train,testcase,cl,k=11)
knn(train,testcase,cl,k=12)
knn(train,testcase,cl,k=13)
knn(train,testcase,cl,k=14)
knn(train,testcase,cl,k=15)
knn(train,testcase,cl,k=16)
knn(train,testcase,cl,k=17)
knn(train,testcase,cl,k=18)
knn(train,testcase,cl,k=19)
knn(train,testcase,cl,k=20)

testcase<-c(42,7500)
knn(train,testcase,cl,k=5)
knn(train,testcase,cl,k=6)
knn(train,testcase,cl,k=7)
knn(train,testcase,cl,k=8)
knn(train,testcase,cl,k=9)
knn(train,testcase,cl,k=10)
knn(train,testcase,cl,k=11)
knn(train,testcase,cl,k=12)
knn(train,testcase,cl,k=13)
knn(train,testcase,cl,k=14)
knn(train,testcase,cl,k=15)
knn(train,testcase,cl,k=16)
knn(train,testcase,cl,k=17)
knn(train,testcase,cl,k=18)
knn(train,testcase,cl,k=19)
knn(train,testcase,cl,k=20)
#here you can notice the change since we're at a high density area on the graph

testcase<-c(40,10000)
knn(train,testcase,cl,k=5)
knn(train,testcase,cl,k=6)
knn(train,testcase,cl,k=7)
knn(train,testcase,cl,k=8)
knn(train,testcase,cl,k=9)
knn(train,testcase,cl,k=10)
knn(train,testcase,cl,k=11)
knn(train,testcase,cl,k=12)
knn(train,testcase,cl,k=13)
knn(train,testcase,cl,k=14)
knn(train,testcase,cl,k=15)
knn(train,testcase,cl,k=16)
knn(train,testcase,cl,k=17)
knn(train,testcase,cl,k=18)
knn(train,testcase,cl,k=19)
knn(train,testcase,cl,k=20)

sampling.rate <- 0.5
#change sampling.rate to 0.5 or 50%

# we need the number of points in the test set to calculate
# the misclassification rate
num.test.set.labels <- n.points * (1 - sampling.rate)

# randomly sample which rows will go in the training set
training <- sample(1:n.points, sampling.rate * n.points,
                   replace=FALSE)

train <- subset(german[training, ], select = c(Age, Credit.amount))
# define the training set to be those rows
# the other rows are going into the test set
testing <- setdiff(1:n.points, training)
# define the test set to be the other rows
test <- subset(german[testing, ], select = c(Age, Credit.amount))

cl <- german$Credit.Risks[training]
# this is the subset of labels for the training set

true.labels <- german$Credit.Risks[testing]
# subset of labels for the test set, we're withholding these

knn (train, test, cl, k=3)

# we'll loop through and see what the misclassification rate
# is for different values of k
sampling_rate_50 <- c()
#
for (k in 1:20) {
  print(k)
  predicted.labels <- knn(train, test, cl, k)
  # We're using the R function knn()
  num.incorrect.labels <- sum(predicted.labels != true.labels)
  misclassification.rate <- num.incorrect.labels /
    num.test.set.labels
  print(misclassification.rate)
  sampling_rate_50 <- c(sampling_rate_50,misclassification.rate)
  
}

ggplot(german,
       aes(y = Credit.amount, x = Age)) +
  geom_point(aes(color = Credit.Risks, shape = Sex)) 
#plot of age vs credit.amount for the german data, with the color as Risks and shape as the sex


#now predict using k with lowest mis-classification rate
testcase<-c(38,12500)
knn(train,testcase,cl,k=5)
knn(train,testcase,cl,k=6)
knn(train,testcase,cl,k=7)
knn(train,testcase,cl,k=8)
knn(train,testcase,cl,k=9)
knn(train,testcase,cl,k=10)
knn(train,testcase,cl,k=11)
knn(train,testcase,cl,k=12)
knn(train,testcase,cl,k=13)
knn(train,testcase,cl,k=14)
knn(train,testcase,cl,k=15)
knn(train,testcase,cl,k=16)
knn(train,testcase,cl,k=17)
knn(train,testcase,cl,k=18)
knn(train,testcase,cl,k=19)
knn(train,testcase,cl,k=20)

testcase<-c(41,8000)
knn(train,testcase,cl,k=5)
knn(train,testcase,cl,k=6)
knn(train,testcase,cl,k=7)
knn(train,testcase,cl,k=8)
knn(train,testcase,cl,k=9)
knn(train,testcase,cl,k=10)
knn(train,testcase,cl,k=11)
knn(train,testcase,cl,k=12)
knn(train,testcase,cl,k=13)
knn(train,testcase,cl,k=14)
knn(train,testcase,cl,k=15)
knn(train,testcase,cl,k=16)
knn(train,testcase,cl,k=17)
knn(train,testcase,cl,k=18)
knn(train,testcase,cl,k=19)
knn(train,testcase,cl,k=20)
#here you can notice the change since we're at a high density area on the graph

testcase<-c(24,2500)
knn(train,testcase,cl,k=5)
knn(train,testcase,cl,k=6)
knn(train,testcase,cl,k=7)
knn(train,testcase,cl,k=8)
knn(train,testcase,cl,k=9)
knn(train,testcase,cl,k=10)
knn(train,testcase,cl,k=11)
knn(train,testcase,cl,k=12)
knn(train,testcase,cl,k=13)
knn(train,testcase,cl,k=14)
knn(train,testcase,cl,k=15)
knn(train,testcase,cl,k=16)
knn(train,testcase,cl,k=17)
knn(train,testcase,cl,k=18)
knn(train,testcase,cl,k=19)
knn(train,testcase,cl,k=20)


kvalue_dataframe <- data.frame(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)
sampling_rate_80 <- c()

kvalues <-c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)
sampling_rate_80
sampling_rate_20
sampling_rate_50

chart <- data.frame(kvalues,sampling_rat)
#show the separate vectors with misclassification rates inputed

