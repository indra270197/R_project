install.packages("party")
install.packages("rpart")
install.packages("rpart.plot")

> x2 <- rpart(Loan_Status ~ Gender + Married + Dependents + Education + Self_Employed + ApplicantIncome + CoapplicantIncome + LoanAmount + Loan_Amount_Term + Property_Area + Credit_History, data=pelatihan_fix, method="anova") 
> library(rpart.plot) 
> rpart.plot(x2)

> x3 <- rpart(Loan_Status ~ Gender + Married + Dependents + Education + Self_Employed + ApplicantIncome + CoapplicantIncome + LoanAmount + Loan_Amount_Term + Property_Area + Credit_History, data=pelatihan_fix, method="class") 
> library(rpart.plot) 
> rpart.plot(x3)

> x4 <- rpart(Loan_Status ~ Gender + Married + Education + Self_Employed + ApplicantIncome + LoanAmount + Loan_Amount_Term + Property_Area + Credit_History, data=pelatihan_fix, method="anova") 
> library(rpart.plot) 
> rpart.plot(x4)

> tree <- rpart(Loan_Status ~ Gender + Married + Education + Self_Employed + ApplicantIncome + LoanAmount + Loan_Amount_Term + Property_Area + Credit_History, data=pelatihan_fix, method="anova") 
> fancyRpartPlot(tree)

> tree1 <- rpart(Loan_Status ~ Gender + Married + Education + Self_Employed + ApplicantIncome + LoanAmount + Property_Area, data=pelatihan_fix, method="anova") 
> fancyRpartPlot(tree1)

> tree2 <- rpart(Loan_Status ~ Gender + Married + Dependents + Education + Self_Employed + ApplicantIncome + CoapplicantIncome + LoanAmount + Loan_Amount_Term + Property_Area + Credit_History, data=pelatihan_fix, method="anova") 
> fancyRpartPlot(tree2)

> train_control <- trainControl(method="cv", number=10) 
> model <- train(Loan_Status~., data=pelatihan_fix, trControl=train_control, method="rpart") 
> print(model)

> printcp(tree2)

> library(tree) 
> summary(tree(Loan_Status ~ Gender + Married + Dependents + Education + Self_Employed + ApplicantIncome + CoapplicantIncome + LoanAmount + Loan_Amount_Term + Property_Area + Credit_History, data=pelatihan_fix, method="anova"))

> dataPelatihanKNN <- read.csv2("E:/dataPelatihanKNN.csv")
> View(dataPelatihanKNN)

> require("class") 
> require("datasets")

> str(dataPelatihanKNN)
> summary(dataPelatihanKNN)

> head(dataPelatihanKNN)

> install.packages("ISLR") 
> install.packages(“caret") 
> library(caret) 
Loading required package: lattice 
Loading required package: ggplot2 
> library(ISLR) 
> trainX <- dataPelatihanKNN[,names(dataPelatihanKNN) != "Direction"] 
> preProcValues <- preProcess(x = trainX,method = c("center", "scale")) 
> preProcValues

> set.seed(400) # diperlukan untuk mereproduksi hasil 
> ctrl <- trainControl(method="repeatedcv",repeats = 3) 
> knnFit <- train(Loan_Status ~ ., data = dataPelatihanKNN, method = "knn", trControl = ctrl, preProcess = c("center","scale"), tuneLength = 20) 
> knnFit

> knnPredict <- predict(knnFit,newdata = testingKNN ) 
> confusionMatrix(knnPredict, testingKNN$Loan_Status )

> library("e1071") 
> library("caret") 
> trControl <- trainControl(method = "cv", number = 10) 
> 
> fit <- train(Loan_Status ~ ., + method = "knn", + tuneGrid = expand.grid(k = 1:30), + trControl = trControl, + metric = "Accuracy", + data = dataPelatihanKNN) > 
> print(fit)

> summary(pelatihan_fix)
> str(pelatihan_fix)

> table(pelatihan_fix$Loan_Status)

> sample_data=sample(480,100,replace = FALSE) 
> training=pelatihan_fix[sample_data,] 
> testing=pelatihan_fix[-sample_data,] 
> training_labels=pelatihan_fix[sample_data,]$Loan_Status 
> testing_labels=pelatihan_fix[-sample_data,]$Loan_Status 
> table(training$Loan_Status) 
N Y 
38 62 
> table(testing$Loan_Status) 
N Y 
115 273 
> library(e1071) 
> pelatihan_classifier=naiveBayes(training,training_labels)
> test_pred=predict(pelatihan_classifier,testing) 
> test_pred

> library(gmodels) 
> CrossTable(test_pred,testing_labels,prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE, dnn = c('predicted', 'actual'))

> library(klaR) Loading required package: MASS 
> library(caret) Loading required package: lattice Loading required package: ggplot2 
> x = pelatihan_fix[,-13] 
> y = pelatihan_fix$Loan_Status 
> library(e1071) 
> train_control <- trainControl(method="cv", number=10) 
> model = train(x,y,'nb',trControl = trainControl(method = 'cv', number = 10)) 
There were 50 or more warnings (use warnings() to see the first 50) 
> print(model)

>test_pred
> data_training$Loan_Status
> mse(as.numeric(data_training$Loan_Status), as.numeric(test_pred))
> sd(test_pred)

library(C50)
library(recipes)

vars <- c("Gender", "Married", "Education", "Self_Employed", "ApplicantIncome", "CoapplicantIncome", "LoanAmount", "Loan_Amount_Term", "Credit_History", "Property_Area")
str(pelatihan_fix[, c(vars, "Loan_Status")])

rule_mod <- C5.0(x = pelatihan_fix[, vars], y = pelatihan_fix$Loan_Status, rules = TRUE)

folds <- cut(seq(1,nrow(dataTrain)),breaks = 10, labels = FALSE) for (i in 1:10) { testIndexes <- which(folds==i, arr.ind = TRUE) testData <- dataTrain[testIndexes, ] trainData <- dataTrain[-testIndexes,] }

rules <- C5.0(Loan_Status~., data = trainData, rules = TRUE) postResample(predict(rules,testData), testData$Loan_Status)

confusionMatrix(rulesPred10, testData$Loan_Status)

