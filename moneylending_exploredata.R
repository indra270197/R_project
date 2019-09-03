print(head(dataTrain, n = 100))
ls(dataTrain)
ncol(dataTrain) 
[1] 13 
nrow(dataTrain) 
[1] 614
str(dataTrain)
subset.datafix <- subset(dataTrain, select = c("Loan_ID", "Gender", "Married", "Dependents", "Education", "Self_Employed", "ApplicantIncome", "CoapplicantIncome", "LoanAmount", "Credit_History", "Property_Area", "Loan_Status"))
str(subset.datafix)
mean(subset.datafix$ApplicantIncome)
[1] 5403.459
mean(subset.datafix$CoapplicantIncome)
[1] 1621.246
mean(subset.datafix$LoanAmount)
[1] NA
mean(subset.datafix$Credit_History)
[1] NA
sd(subset.datafix$ApplicantIncome)
[1] 6109.042
sd(subset.datafix$CoapplicantIncome)
[1] 2926.248
sd(subset.datafix$Credit_History)
[1] NA
sd(subset.datafix$LoanAmount)
[1] NA
min(subset.datafix$ApplicantIncome);
max(subset.datafix$ApplicantIncome)
[1] 150
[1] 81000
min(subset.datafix$CoapplicantIncome);
max(subset.datafix$CoapplicantIncome)
[1] 0
[1] 41667
min(subset.datafix$Credit_History);
max(subset.datafix$Credit_History)
[1] NA
[1] NA
min(subset.datafix$LoanAmount);
max(subset.datafix$LoanAmount)
[1] NA
[1] NA
range(subset.datafix$LoanAmount)
[1] NA NA
range(subset.datafix$Credit_History)
[1] NA NA
range(subset.datafix$ApplicantIncome)
[1] 150 81000
range(subset.datafix$CoapplicantIncome)
[1] 0 41667
quantile(subset.datafix$ApplicantIncome, prob = c(.25, .50, .75, .95)) 
25% 50% 75% 95% 
2877.5 3812.5 5795.0 14583.0 
quantile(subset.datafix$ApplicantIncome, c(.25, .50, .75, .95)) 
25% 50% 75% 95% 
2877.5 3812.5 5795.0 14583.0 
quantile(subset.datafix$ApplicantIncome, c(.25, .50, .75, .95, 1)) 
25% 50% 75% 95% 100% 2877.5 
3812.5 5795.0 14583.0 81000.0 
quantile(subset.datafix$ApplicantIncome, c(0, .25, .50, .75, .95, 1)) 
0% 25% 50% 75% 95% 100% 
150.0 2877.5 3812.5 5795.0 14583.0 81000.0
length(subset.datafix$ApplicantIncome[subset.datafix$ApplicantIncome <= 20000]) / length(subset.datafix$ApplicantIncome) * 100 
[1] 98.0456 
length(subset.datafix$ApplicantIncome[subset.datafix$ApplicantIncome >= 20000]) / length(subset.datafix$ApplicantIncome) * 100 
[1] 1.954397
length(subset.datafix$ApplicantIncome[subset.datafix$ApplicantIncome <= 15000]) / length(subset.datafix$ApplicantIncome) * 100 
[1] 95.92834 
length(subset.datafix$ApplicantIncome[subset.datafix$ApplicantIncome >= 15000]) / length(subset.datafix$ApplicantIncome) * 100 
[1] 4.234528 
length(subset.datafix$ApplicantIncome[subset.datafix$ApplicantIncome >= 14000]) / length(subset.datafix$ApplicantIncome) * 100 
[1] 5.211726 
length(subset.datafix$ApplicantIncome[subset.datafix$ApplicantIncome <= 14000]) / length(subset.datafix$ApplicantIncome) * 100 
[1] 94.78827
length(subset.datafix$ApplicantIncome[subset.datafix$ApplicantIncome <= 4000]) / length(subset.datafix$ApplicantIncome) * 100 
[1] 54.39739 
length(subset.datafix$ApplicantIncome[subset.datafix$ApplicantIncome >= 4000]) / length(subset.datafix$ApplicantIncome) * 100 
[1] 46.09121
print(summary(subset.datafix))
plot(subset.datafix$LoanAmount, subset.datafix$ApplicantIncome, xlab = "Loan Amount", ylab = "Applicant Income", pch = 16, col = "blue", main = " Scatterplot")
ggplot(subset.datafix) + geom_point(aes(x = LoanAmount, y = ApplicantIncome), col = 'blue', size = 3) + ggtitle("income vs. loan amount") + theme(plot.title = element_text(hjust = 0.5))
coef(lm(ApplicantIncome ~ LoanAmount, data = subset.datafix)) 
(Intercept) LoanAmount 
-615.77775 41.11736
ggplot(subset.datafix) + geom_point(aes(x = LoanAmount, y = ApplicantIncome), col = "blue", size = 3) + geom_abline(aes(intercept = -615.77775, slope = 41.11736), col = "darkred") + ggtitle("income vs. loan amount Scatterplot With The Best Fit Line") + theme(plot.title = element_text(hjust = 0.5))
ggplot(pelatihan_new, aes(x = LoanAmount, y = ApplicantIncome)) + geom_text(aes(label = Loan_Status), size = 4) + ggtitle("Scatterplot With Data Labels") + theme(plot.title = element_text(hjust = 0.5))
ggplot(pelatihan_new, mapping = aes(x = LoanAmount, y = ApplicantIncome)) + geom_point(aes(color = Loan_Status, shape = Property_Area)) + ggtitle("scatter plot") + theme(plot.title = element_text(hjust = 0.5))

