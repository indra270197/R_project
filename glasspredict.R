install.packages("party")
library(party)
set.seed(214)
ind <- sample(2, nrow(glass), replace=TRUE, prob=c(0.7,0.3))
train.data <- glass[ind==1,]
test.data <- glass[ind==2,]
myf <- Type ~ Na+Mg+Al+Si+K+Ca+Ba+Fe
glass_ctree <- ctree(myf, data=train.data)
table(predict(glass_ctree), train.data$Type)
testpred <- predict(glass_ctree, newdata=test.data)
table(testpred, test.data$Type)
plot(glass_ctree)