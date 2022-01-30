wage <- read.csv("Wage(1).csv")
dim(wage)
summary(wage)


#check for missing values
table(is.na(wage$wage))
table(is.na(wage$age))
table(is.na(wage$education))
table(is.na(wage$jobclass))
table(is.na(wage$year))
table(is.na(wage$race))




#measures of location for continuos variables
mean(wage$wage)
median(wage$wage)
mean(wage$age)
median(wage$age)
mean(wage$logwage)
median(wage$logwage)
mean(wage$year)
median(wage$year)





#measures of spread for continuous variables
sd(wage$wage)
quantile(wage$wage)
range(wage$wage)
sd(wage$age)
quantile(wage$age)
range(wage$age)
sd(wage$logwage)
quantile(wage$logwage)
range(wage$logwage)





#frequencies of continuous variables
hist(wage$wage, col = "skyblue", main = "WAGE", xlab = "Wage", cex.lab = 1.5)
hist(wage$age, col = "skyblue", main = "AGE", xlab = "Age", cex.lab = 1.5)
hist(wage$logwage, col = "skyblue", main = "LOGWAGE", xlab = "LogWage", cex.lab = 1.5)
hist(wage$year, col = "skyblue", main = "YEAR", xlab = "Year", cex.lab = 1.5)





#frequencies of categorical variables
label <- paste(names(table(wage$maritl)), "\n", table(wage$maritl))
titles <- "Pie Chart for Maritl"
pie(table(wage$maritl), labels = label, main = titles, cex = 1.5 , cex.main = 2 )
pie(table(wage$race), labels = paste(names(table(wage$race)), "\n", table(wage$race)), main = "Pie Chart for Race", cex = 1.5 , cex.main = 2)
pie(table(wage$education), labels = paste(names(table(wage$education)), "\n", table(wage$education)), main = "Pie Chart for Education", cex = 1.5 , cex.main = 2)
pie(table(wage$jobclass), labels = paste(names(table(wage$jobclass)), "\n", table(wage$jobclass)), main = "Pie Chart for Job Class", cex = 1.5 , cex.main = 2)
pie(table(wage$health), labels = paste(names(table(wage$health)), "\n", table(wage$health)), main = "Pie Chart for Health", cex = 1.5 , cex.main = 2)
pie(table(wage$health_ins), labels = paste(names(table(wage$health_ins)), "\n", table(wage$health_ins)), main = "Pie Chart for Health Insurance", cex = 1.5 , cex.main = 2)
barplot(table(wage$maritl))
barplot(table(wage$race))
barplot(table(wage$education))
barplot(table(wage$jobclass))
barplot(table(wage$health))
barplot(table(wage$health_ins))






#normality of variables
qqnorm(wage$wage, col = "darkred", main = "QQ-Plot for Wage", cex.main = 2, cex.lab = 2)
qqline(wage$wage)
qqnorm(log(wage$wage), col = "darkred", main = "QQ-Plot for Log Wage", cex.main = 2, cex.lab = 1.5)
qqline(log(wage$wage))
qqnorm(wage$logwage, col = "darkred")
qqline(wage$logwage)
qqnorm(wage$age, col = "darkred", main = "QQ-Plot for Age", cex.main = 2, cex.lab = 2)
qqline(wage$age)






#distribution of variable
hist(wage$wage, freq = FALSE, col = "skyblue", main = "WAGE", xlab = "Wage")
points(wage$wage, dnorm(wage$wage, mean = mean(wage$wage), sd = sd(wage$wage)))
hist(wage$age, freq = FALSE, col = "skyblue", main = "AGE", xlab = "Age")
points(wage$age, dnorm(wage$age, mean = mean(wage$age), sd = sd(wage$age)))
plot(density(wage$wage))
plot(density(wage$age))
plot(density(wage$logwage))





#check for outliers
boxplot(wage$wage, main = "Boxplot for Wage", cex.main = 2, cex.lab = 1.5, col = "pink")
boxplot(wage$age, main = "Boxplot for Age", cex.main = 2, cex.lab = 1.5, col = "pink")
boxplot(wage$year, main = "Boxplot for Year", cex.main = 2, cex.lab = 1.5, col = "pink")
boxplot(wage$logwage, main = "Boxplot for Log Wage", cex.main = 2, cex.lab = 1.5, col = "pink")






#Relation between variables
boxplot(wage$wage~wage$jobclass, main = "Wage Categorized by Job Class", cex.main = 2, xlab = "Job Class", ylab = "Wage", cex.lab = 1.5, cex.axis = 1.5, col = c("mistyrose", "powderblue"), medcol = c("red","darkblue"), whiskcol = c("red","darkblue"))
boxplot(wage$wage~wage$education, main = "Wage Categorized by Education", cex.main = 2, xlab = "Education Level", ylab = "Wage", cex.lab = 1.5, col = rainbow(length(unique(wage$education)), alpha = 0.2))
boxplot(wage$wage~wage$maritl, main = "Wage Categorized by Marital Status", cex.main = 2, xlab = "Marital Status", ylab = "Wage", cex.lab = 1.5, col = rainbow(length(unique(wage$education)), alpha = 0.2))
boxplot(wage$wage~wage$race, main = "Wage Categorized by Race", cex.main = 2, xlab = "Race", ylab = "Wage", cex.lab = 1.5, col = rainbow(length(unique(wage$education)), alpha = 0.2))
boxplot(wage$wage~wage$health_ins)
boxplot(wage$age~wage$health_ins)
boxplot(wage$age~wage$health)

boxplot(wage$logwage~wage$jobclass, main = "Log Wage Categorized by Job Class", cex.main = 2, xlab = "Job Class", ylab = "Wage", cex.lab = 1.5, col = c("mistyrose", "powderblue"), medcol = c("red","darkblue"), whiskcol = c("red","darkblue"))
boxplot(wage$logwage~wage$education, main = "Log Wage Categorized by Education", cex.main = 2, xlab = "Education Level", ylab = "Wage", cex.lab = 1.5, col = rainbow(length(unique(wage$education)), alpha = 0.2))
boxplot(wage$logwage~wage$maritl, main = "Log Wage Categorized by Marital Status", cex.main = 2, xlab = "Marital Status", ylab = "Wage", cex.lab = 1.5, col = rainbow(length(unique(wage$education)), alpha = 0.2))





#Pairwaise Association between varibales

#Pearson's correlation test for associtaion between two continuous variables
plot(wage$wage, wage$age, col = rgb(0,0,0, alpha = 0.4), main = "Wage VS Age", xlab = "Wage", ylab = "Age", cex.main = 2, cex.lab = 1.5)
abline(lm(wage$age~wage$wage), col = "red", lty = "dashed", lwd = 3)
cor(wage$age, wage$wage, use = "everything", method = "pearson")
cor.test(wage$age, wage$wage, method = "pearson")

#Chi-square test for association between two categorical variables
tbl3 <- table(wage$education,wage$maritl)
tbl1 <- table(wage$education,wage$race)
tbl2 <-table(wage$race,wage$education)
tbl <- table(wage$jobclass,wage$education)
tbl
chisq.test(tbl)
chisq.test(tbl1)
chisq.test(tbl2)
chisq.test(tbl3)


#Logistic Regression for association between categorical and continuous variables
reg.model <- lm(wage$wage~wage$education)
summary(reg.model)






#Descriptive Analysis
#Why one could focus on predicting log-wage, not wage directly?
mean(wage$wage)
median(wage$wage)
plot(density(wage$wage), col = "darkred", col.fill = "pink", main = "Density Curve of Wage", xlab = "Wage", cex.main = 2, cex.lab = 1.5, lwd = 2)
abline(v = mean(wage$wage), col = "blue", lwd = 2)
abline(v = median(wage$wage), col = "darkgreen", lwd =2)
legend("topright", legend = c("Mean","Median"), col = c("blue","darkgreen"), lty = 1:1, lwd = 2)
legend("topleft", legend = c(paste("Mean = ",mean(wage$wage)),paste("Median = ",median(wage$wage))))

mean(wage$logwage)
median(wage$logwage)
plot(density(wage$logwage), col = "darkred", col.fill = "pink", main = "Density Curve of Log-Wage", xlab = "Log-Wage", cex.main = 2, cex.lab = 1.5, lwd = 2)
abline(v = mean(wage$logwage), col = "blue", lwd = 2)
abline(v = median(wage$logwage), col = "darkgreen", lwd =2)
legend("topright", legend = c("Mean","Median"), col = c("blue","darkgreen"), lty = 1:1, lwd = 2)
legend("topleft", legend = c(paste("Mean = ",mean(wage$logwage)),paste("Median = ",median(wage$logwage))))








#Multiple Regression
#Multiple Regression to establish which variables are associated with wage?
AGE <- wage$age
YEAR <- wage$year
MARITAL_STATUS <- wage$maritl
RACE <- wage$race
EDUCATION <- wage$education
REGION <- wage$region
JOB_CLASS <- wage$jobclass
HEALTH <- wage$health
HEALTH_INSURANCE <- wage$health_ins
Wage <- wage$wage
LOG_WAGE <- wage$logwage

mlr1 <- lm(Wage~AGE+YEAR+MARITAL_STATUS+RACE+EDUCATION+JOB_CLASS+HEALTH+HEALTH_INSURANCE)
summlr1 <- summary(mlr1)
summlr1
anova(mlr1)

#Multiple Linear Regression of log-wage

mlr2 <- lm(LOG_WAGE~AGE+YEAR+MARITAL_STATUS+RACE+EDUCATION+JOB_CLASS+HEALTH+HEALTH_INSURANCE)
summlr2 <- summary(mlr2)
summlr2
mlr3 <- lm(LOG_WAGE~AGE+YEAR+MARITAL_STATUS+EDUCATION+JOB_CLASS+HEALTH+HEALTH_INSURANCE)
summlr3 <- summary(mlr3)
summlr3
anova(mlr2, mlr3)
mlr4 <- lm(LOG_WAGE~AGE+YEAR+MARITAL_STATUS+EDUCATION+HEALTH+HEALTH_INSURANCE)
summlr4 <- summary(mlr4)
summlr4
anova(mlr2, mlr4)


mlr5 <- lm(LOG_WAGE~AGE+YEAR+EDUCATION+HEALTH+HEALTH_INSURANCE)
summlr5 <- summary(mlr5)
summlr5

anova(mlr2, mlr5)







#Residual plots
layout(matrix(c(1,2,3,4),2,2))
plot(mlr4)

plot(mlr2)






#Extra
plot(wage$wage, wage$age, pch = unclass(wage$education),col = rainbow(length(unique(wage$education))), main = "Wage VS Age", xlab = "Wage", ylab = "Age", cex.main = 2, cex.lab = 1.5)
abline(lm(wage$age~wage$wage), col = "red", lty = "dashed", lwd = 3)
legend("bottomright", legend=levels(wage$education), pch=c(1:5), col = rainbow(length(unique(wage$education))))


plot(wage$wage, wage$age, pch = unclass(wage$health_ins),col = rainbow(length(unique(wage$health_ins))), main = "Wage VS Age", xlab = "Wage", ylab = "Age", cex.main = 2, cex.lab = 1.5)
abline(lm(wage$age~wage$wage), col = "red", lty = "dashed", lwd = 3)
legend("bottomright", legend=levels(wage$health_ins), pch=c(1:5), col = rainbow(length(unique(wage$health_ins))))


#reducing variable
mlr3 <- lm(Wage~AGE+YEAR+MARITAL_STATUS+RACE+EDUCATION+JOB_CLASS+HEALTH+HEALTH_INSURANCE)
summlr3 <- summary(mlr3)
summlr3

mlr4 <- lm(LOG_WAGE~AGE+YEAR+MARITAL_STATUS+RACE+EDUCATION+JOB_CLASS+HEALTH+HEALTH_INSURANCE)
summlr4 <- summary(mlr4)
summlr4

#remove health_ins
mlr5 <- lm(Wage~AGE+YEAR+MARITAL_STATUS+RACE+EDUCATION+JOB_CLASS+HEALTH)
summlr5 <- summary(mlr5)
summlr5
anova(mlr5)

#remove health
mlr6 <- lm(Wage~AGE+YEAR+MARITAL_STATUS+RACE+EDUCATION+JOB_CLASS)
summlr6 <- summary(mlr6)
summlr6
anova(mlr6)

#remove jobclass
mlr7 <- lm(Wage~AGE+YEAR+MARITAL_STATUS+RACE+EDUCATION)
summlr7 <- summary(mlr7)
summlr7
anova(mlr7)

#remove education
mlr8 <- lm(Wage~AGE+YEAR+MARITAL_STATUS+RACE)
summlr8 <- summary(mlr8)
summlr8
anova(mlr8)

#remove race
mlr9 <- lm(Wage~AGE+YEAR+MARITAL_STATUS)
summlr9 <- summary(mlr9)
summlr9
anova(mlr9)