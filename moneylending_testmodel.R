> library(caret) 
> data.train <- read.csv("D:/Kuliah/7/PD/pelatihan_fix.csv") 
> data.testing <- read.csv("D:/Kuliah/7/PD/Data tes.csv") 
> tc <- trainControl("cv",10) 
> rpart.grid <- expand.grid(.cp=0.008714597) 
> data.train$Loan_Status <- as.factor(data.train$Loan_Status) > (data.train.rpart <- train(Loan_Status ~ Gender + Married + Dependents + Education + Self_Employed + ApplicantIncome + CoapplicantIncome + LoanAmount + Loan_Amount_Term + Property_Area + Credit_History, + data=data.train, + method="rpart", + trControl=tc, + na.action = na.omit, + tuneGrid=rpart.grid))

> pred <- predict(data.train.rpart, data.train) 
> cf <- confusionMatrix(pred, data.train.rpart$trainingData$.outcome, mode = "everything") 
> print(cf)

> library(caret) 
Loading required package: lattice 
Loading required package: ggplot2 
> library(ISLR) 
> set.seed(400) # diperlukan untuk mereproduksi hasil 
> ctrl <- trainControl(method="repeatedcv",repeats = 3) 
> knnFit <- train(Loan_Status ~ ., data = dataPelatihanKNN, method = "knn", trControl = ctrl, preProcess = c("center","scale"), tuneLength = 20) > knnFit Selanjutnya melakukan prediksi data testing menggnakan data training dan melakukan running confusion matrix seperti dibawah ini : > knnPredict <- predict(knnFit,newdata = testingKNN ) 
> confusionMatrix(knnPredict, testingKNN$Loan_Status )

> library(e1071) 
> library(caTools) 
> library(caret) 
> data.train <- read.csv("D:/Kuliah/7/PD/pelatihan_fix.csv") > data.testing <- read.csv("D:/Kuliah/7/PD/Data tes.csv") 
> pelatihan.nb <- naiveBayes(Loan_Status ~ ., data = data.train) 
> pelatihan_train_predict <- predict(pelatihan.nb, data.testing[ , names(data.testing) != "Loan_Status"]) 
> cfm <- confusionMatrix(pelatihan_train_predict, data.testing$Loan_Status) 
> cfm

> lvs <- c("Y","N") 
> NB_Reference <- factor(rep(lvs, times = c(300, 67)), + levels = rev(lvs)) 
> NB_Prediction <- factor( + c( + rep(lvs, times = c(276, 24)), + rep(lvs, times = c(8, 59))), + levels = rev(lvs)) 
> NB <- table(NB_Prediction, NB_Reference) 
> precision(NB) 
[1] 0.7108434 
> recall(NB) 
[1] 0.880597 
> NB_precision <- precision(NB) 
> NB_recall <- recall(NB) 
> Fmeasure <- 2 * NB_precision * NB_recall / (NB_precision + NB_recall) 
> Fmeasure 
[1] 0.7866667

rulesPred <- predict(rule_mod, dataTest)
postResample(predict(rule_mod,dataTest), dataTest$Loan_Status) 
Accuracy Kappa 
0.9782016 0.9234140
confusionMatrix(rulesPred, dataTest$Loan_Status)

lvs <- c("Y","N") 
> Reference <- factor(rep(lvs, times = c(300, 67)), + levels = rev(lvs)) 
> Prediction <- factor( + c( + rep(lvs, times = c(300, 0)), + rep(lvs, times = c(8, 59))), + levels = rev(lvs)) 
> RB <- table(Prediction, Reference) 
> precision(RB) 
[1] 1 
> recall(RB)
[1] 0.880597 
> RB_precision <- precision(RB) 
> RB_recall <- recall(RB) 
> Fmeasure <- 2 * RB_precision * RB_recall / (RB_precision + RB_recall) 
> Fmeasure 
[1] 0.9365079