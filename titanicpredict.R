setwd("D:/dataset/titanic")
dataset <- read.csv("train.csv")
colnames(dataset)
head(dataset)
summmary(dataset)
table(dataset$Survived, dataset$Sex)
table(dataset$Survived, dataset$Sex)/nrow(dataset)
testset <- read.csv("test.csv")
colname(testset)
testset$Survived <- 0
testset$Survived[testset$Sex == 'female'] <- 1
submit <- data.frame(PassengerId=testset$PassengerId, Survived=testset$Survived)
write.csv(submit, file="all_femailes_survive.csv", row.names=FALSE)