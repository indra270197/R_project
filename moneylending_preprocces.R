Mode <- function (x, na.rm) {
xtab <- table(x)
xmode <- names(which(xtab == max(xtab)))
if (length(xmode) > 1)
xmode <- ">1 mode"
return(xmode)
}

DataTrain$Gender <- ifelse(is.na(DataTrain$Gender),
Mode(DataTrain$Gender),
DataTrain$Gender)
DataTrain$Married <- ifelse(is.na(DataTrain$Married),
Mode(DataTrain$Married),
DataTrain$Married)
DataTrain$Dependents <- ifelse(is.na(DataTrain$Dependents),
Mode(DataTrain$Dependents),
DataTrain$Dependents)
DataTrain$Education <- ifelse(is.na(DataTrain$Education),
Mode(DataTrain$Education),
DataTrain$Education)
DataTrain$Self_Employed <- ifelse(is.na(DataTrain$Self_Employed),
Mode(DataTrain$Self_Employed),
DataTrain$ Self_Employed)
DataTrain$Loan_Amount_Term <- ifelse(is.na(DataTrain$Loan_Amount_Term),
Mode(DataTrain$Loan_Amount_Term),
DataTrain$Loan_Amount_Term)
DataTrain$Credit_History <- ifelse(is.na(DataTrain$Credit_History),
Mode(DataTrain$Credit_History),
DataTrain$Credit_History)
DataTrain$Property_Area <- ifelse(is.na(DataTrain$Property_Area),
Mode(DataTrain$Property_Area),
DataTrain$Property_Area

DataTrain$LoanAmount <- ifelse(is.na(DataTrain$LoanAmount),
ave(DataTrain$LoanAmount,
FUN = function(x) mean(x, na.rm = TRUE)),
DataTrain$LoanAmount)
DataTrain$ApplicantIncome <- ifelse(is.na(DataTrain$ApplicantIncome),
ave(DataTrain$ApplicantIncome,
FUN = function(x) mean(x, na.rm = TRUE)),
DataTrain$ApplicantIncome)
DataTrain$CoapplicantIncome <- ifelse(is.na(DataTrain$CoapplicantIncome),
ave(DataTrain$CoapplicantIncome,
FUN = function(x) mean(x, na.rm = TRUE)),
DataTrain$CoapplicantIncome)

dataTrain_new <- DataTrain

write.csv(dataTrain_new, file = ".../dataTrain_new.csv",
row.names = FALSE)

remove_outliers <- function(x, na.rm = TRUE, ...) {
qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
H <- 1.5 * IQR(x, na.rm = na.rm)
y <- x
y[x < (qnt[1] - H)] <- NA
y[x > (qnt[2] + H)] <- NA
y
}

dataTrain$LoanAmount <- remove_outliers(dataPelatihan_new$LoanAmount)
dataTrain$ApplicantIncome <- remove_outliers(dataTrain_new$ApplicantIncome)
dataTrain$CoapplicantIncome<-remove_outliers(dataTrain_new$CoapplicantIncome)

ggplot(dataTrain, aes(x=Loan_Status, y=LoanAmount)) + geom_boxplot()
ggplot(dataTrain, aes(x=Loan_Status, y=ApplicantIncome)) + geom_boxplot()
ggplot(dataTrain, aes(x=Loan_Status, y=CoapplicantIncome)) + geom_boxplot()

dataTrain_new <- dataTrain[complete.cases(dataTrain), ]

write.csv(dataTrain_new, file = ".../dataTrain_new.csv",
row.names = FALSE)

