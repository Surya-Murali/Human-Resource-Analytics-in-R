setwd("C:/Users/surya/Desktop/SpringSemester/Flex 4")

#load the data
mydata = read.csv("HR_comma_sep.csv")

#********************DATA MANIPULATION AND DATA PREPARATION********************

#Adding a new column called 'salaryOrder'
mydata$salaryOrder[which(mydata$salary == "low")] = 1
mydata$salaryOrder[which(mydata$salary == "medium")] = 2
mydata$salaryOrder[which(mydata$salary == "high")] = 3

#Adding a new column called 'employee_satisfaction'
mydata$employee_satisfaction[mydata$satisfaction_level >= 0.9] = '1.Maximum'
mydata$employee_satisfaction[mydata$satisfaction_level >= 0.8 & mydata$satisfaction_level < 0.9 ] = '2.High'
mydata$employee_satisfaction[mydata$satisfaction_level >= 0.6 & mydata$satisfaction_level < 0.8 ] = '3.Good'
mydata$employee_satisfaction[mydata$satisfaction_level >= 0.4 & mydata$satisfaction_level < 0.6 ] = '4.Average'
mydata$employee_satisfaction[mydata$satisfaction_level >= 0.2 & mydata$satisfaction_level < 0.4 ] = '5.Low'
mydata$employee_satisfaction[mydata$satisfaction_level <  0.2] = '6.Minimum'

#Converting the employee_satisfaction column as a factor
mydata$employee_satisfaction = as.factor(mydata$employee_satisfaction)

#One more new variable for 'left' for string representation.
mydata$leftFlag[mydata$left ==  1] = 'Left'
mydata$leftFlag[mydata$left ==  0] = 'Not Left'

#********************EDA********************

#********************SUMMARY********************

#Data Summary
dim(mydata)
str(mydata)
summary(mydata)
#Get the class
sapply(mydata,class)

#par(mfrow=c(1,1))
#attach(mydata)

#********************HISTOGRAMS********************

par(mfrow=c(2,2))
hist(mydata$satisfaction_level, main = "Histogram of Satisfaction", xlab = "Satisfaction")
hist(mydata$last_evaluation, main = "Histogram of Last Evaluation", xlab = "Last Evaluation")
#hist(mydata$number_project, main = "Histogram of No. of Projects", xlab = "No. of Projects")
hist(mydata$average_montly_hours, main = "Histogram of Avg Hours Spent", xlab = "Avg Hours Spent")
hist(mydata$time_spend_company, main = "Histogram of Time Spent in Company", xlab = "Time Spent in Company")

#********************BOXPLOTS********************

par(mfrow=c(2,2))
boxplot(mydata$satisfaction_level, main = "Satisfaction")
boxplot(mydata$last_evaluation, main = "Last Evaluation")
#boxplot(mydata$number_project, main = "No. of Projects")
boxplot(mydata$average_montly_hours, main = "Avg Hours Spent")
boxplot(mydata$time_spend_company, main = "Time Spent in Company")

#********************Satisfaction Vs Employees Left / Not Left********************

par(mfrow=c(1,1))
#Create a barplot 'Employees left vs Satisfaction'
SatisfactionAndLeftTable <- table(mydata$leftFlag, mydata$employee_satisfaction)
barplot(SatisfactionAndLeftTable, main="Satisfaction Vs Employees Left / Not Left",
        xlab="Satisfaction Level", col=c("purple","orange"),
        legend = rownames(SatisfactionAndLeftTable), beside=TRUE)

#********************Employees Left / Not Left vs No. of Projects********************

projectsPlotData <- table(mydata$leftFlag, mydata$number_project)
barplot(projectsPlotData, main="Employees Left / Not Left vs No. of Projects",
        xlab="Number of Projects", col=c("purple","orange"),
        legend = rownames(projectsPlotData), beside=TRUE)

#********************PIE CHART********************

p = ggplot(subset(mydata,left==1), aes(x = factor('Salary'), fill = factor(salary))) +
  geom_bar(width = 1, position = "fill", color = "black") + coord_polar(theta = "y")+theme_bw()+
  ggtitle("Salary Splitup") +xlab("")+ylab("") + scale_fill_discrete(name="Salary")

p = p + theme(
  plot.title = element_text(color="Black", size=14, face="bold.italic", hjust = 0.5),
  axis.title.x = element_text(color="Black", size=14, face="bold"),
  axis.title.y = element_text(color="Black", size=14, face="bold")
)
print(p)

#********************Frequency By Salary Order of Employees********************

table1<-table(mydata$salaryOrder,(mydata$employee_satisfaction))
#print(table1)
table1<-as.data.frame(table1)
table1$salaryOrder = table1$Var1
table1$employee_satisfaction = table1$Var2
table1$Var1= NULL
table1$Var2= NULL

print(table1)
library(ggplot2)

p<-ggplot(table1, aes(x=salaryOrder,y=Freq,fill=employee_satisfaction)) +
  geom_bar(position="dodge",stat='identity') +
  ggtitle("Frequency By Salary Order of Employees") +xlab("Salary Order") +ylab("Frequency")

p = p + theme(
  plot.title = element_text(color="Black", size=14, face="bold.italic", hjust = 0.5),
  axis.title.x = element_text(color="Black", size=14, face="bold"),
  axis.title.y = element_text(color="Black", size=14, face="bold")  
)
print(p)

#********************Number of Employees left for each Department********************

roleTable<-table(mydata$role,mydata$left)
roledf<-as.data.frame(roleTable)
roledf$role = roledf$Var1
roledf$leftFlag = roledf$Var2
roledf$Var1= NULL
roledf$Var2= NULL

roledfLeft<-subset(roledf,leftFlag==1)
print(roledfLeft)

#Employees Left By Department
roledfLeft$role <- factor(roledfLeft$role, levels = roledfLeft$role[order(-roledfLeft$Freq)])
e<-ggplot(roledfLeft, aes(x=role,y=Freq,fill="Orange")) +
  geom_bar(stat='identity') +theme(axis.text.x = element_text(angle = 45, hjust = 1))+ guides(fill=FALSE) +
  ggtitle("Number of Employees left for each Department") +xlab("Department")

e = e + theme(
  plot.title = element_text(color="Black", size=14, face="bold.italic", hjust = 0.5),
  axis.title.x = element_text(color="Black", size=14, face="bold"),
  axis.title.y = element_text(color="Black", size=14, face="bold")
)
print(e)

#********************Department Vs Satisfaction of Employees********************

library(dplyr)
groupedByRole = mydata %>%
  group_by(role) %>%
  summarise(mean=mean(satisfaction_level), sd=sd(satisfaction_level), count=n())

groupedByRole = data.frame(groupedByRole)
groupedByRole = groupedByRole[order(groupedByRole$mean),]
p<-ggplot(groupedByRole, aes(x=reorder(role, -mean),y=mean,fill="Orange")) +
  geom_bar(stat='identity') +theme(axis.text.x = element_text(angle = 45, hjust = 1))+ guides(fill=FALSE) +coord_cartesian(ylim = c(0.55, 0.63)) +
  ggtitle("Department Vs Satisfaction of Employees") +xlab("Department")

p = p + theme(
  plot.title = element_text(color="Black", size=14, face="bold.italic", hjust = 0.5),
  axis.title.x = element_text(color="Black", size=14, face="bold"),
  axis.title.y = element_text(color="Black", size=14, face="bold")
)
print(p)

#********************Number of Projects Vs Satisfaction of Employees********************

p<-ggplot(mydata, aes(x = factor(number_project), y = satisfaction_level, fill=factor(left))) +
  geom_boxplot() + scale_fill_manual(values = c("orange", "purple"))+
  ggtitle("Number of Projects Vs Satisfaction of Employees") +xlab("Number of Projects") +ylab("Satisfaction Level")

p = p + theme(
  plot.title = element_text(color="Black", size=14, face="bold.italic", hjust = 0.5),
  axis.title.x = element_text(color="Black", size=14, face="bold"),
  axis.title.y = element_text(color="Black", size=14, face="bold")
)
print(p)

#********************Number of Projects Vs  Last Evaluation Score of Employees********************

p<-ggplot(mydata, aes(x = factor(number_project), y = last_evaluation, fill=factor(left))) +
  geom_boxplot() + scale_fill_manual(values = c("orange", "purple"))+
  ggtitle("Number of Projects Vs  Last Evaluation Score of Employees") +xlab("Number of Projects") +ylab("Last Evaluation")

p = p + theme(
  plot.title = element_text(color="Black", size=14, face="bold.italic", hjust = 0.5),
  axis.title.x = element_text(color="Black", size=14, face="bold"),
  axis.title.y = element_text(color="Black", size=14, face="bold")
)
print(p)

#********************Number of Projects Vs  Average Montly Hours of Employees********************

p<-ggplot(mydata, aes(x = factor(number_project), y = average_montly_hours, fill=factor(left))) +
  geom_boxplot() + scale_fill_manual(values = c("orange", "purple"))+
  ggtitle("Number of Projects Vs  Average Montly Hours of Employees") +xlab("Number of Projects") +ylab("Average Montly Hours")

p = p + theme(
  plot.title = element_text(color="Black", size=14, face="bold.italic", hjust = 0.5),
  axis.title.x = element_text(color="Black", size=14, face="bold"),
  axis.title.y = element_text(color="Black", size=14, face="bold")
)
print(p)

#********************CORRELATIONS********************

par(mfrow=c(1,1))
#install.packages("corrplot")
library(corrplot)
#Check for correlations for all the variables
corrplot(cor(mydata[ ,c("last_evaluation","number_project","average_montly_hours", "time_spend_company","Work_accident", "satisfaction_level", "left", "promotion_last_5years", "salaryOrder")]), method = "square", type="lower") 
#Check for correlations for the variables of interest
corrplot(cor(mydata[ ,c("satisfaction_level","last_evaluation","number_project","average_montly_hours","time_spend_company","left")]), method = "square", type="full")


#********************LOGISTIC REGRESSION FOR 'LEFT' VARIABLE********************


logisticModel<-glm(left ~ satisfaction_level + last_evaluation + number_project 
                + average_montly_hours + time_spend_company + Work_accident
                + promotion_last_5years + salaryOrder, 
                data=mydata, family=binomial(link="logit"))
summary(logisticModel)
1-(logisticModel$deviance/logisticModel$null.deviance)

library(car)
vif(logisticModel)

head(mydata[,c(1:6,8,11)])

#Standardising Variables
stdVariables=as.data.frame(apply(mydata[,c(1:6,8,11)],2,function(x){(x-mean(x))/sd(x)}))
leftVar<-mydata[,7]
stdVariables<-cbind(stdVariables,leftVar)
head(stdVariables)

stdLogisticModel = glm(leftVar ~ satisfaction_level + last_evaluation + number_project + average_montly_hours + time_spend_company + Work_accident
                   + promotion_last_5years + salaryOrder, data=stdVariables, family=binomial(link="logit"))

summary(stdLogisticModel)

#Calculating psedo R-squared: (% variance explained by regression)
1-(stdLogisticModel$deviance/stdLogisticModel$null.deviance)

#Left Vs Salary:

lowSalary<-mydata[which(mydata$salaryOrder==1), c(1:8,11)]
medSalary<-mydata[which(mydata$salaryOrder==2), c(1:8,11)]
highSalary<-mydata[which(mydata$salaryOrder==3), c(1:8,11)]

lowSalaryModel<-glm(left ~ satisfaction_level + last_evaluation + number_project 
                    + average_montly_hours + time_spend_company + Work_accident
                    + promotion_last_5years, 
                    data=lowSalary, family=binomial(link="logit"))
summary(lowSalaryModel)
#Calculating psedo R-squared: (% variance explained by regression)
1-(lowSalaryModel$deviance/lowSalaryModel$null.deviance)

medSalaryModel<-glm(left ~ satisfaction_level + last_evaluation + number_project 
                    + average_montly_hours + time_spend_company + Work_accident
                    + promotion_last_5years, 
                    data=medSalary, family=binomial(link="logit"))
summary(medSalaryModel)
#Calculating psedo R-squared: (% variance explained by regression)
1-(medSalaryModel$deviance/medSalaryModel$null.deviance)

highSalaryModel<-glm(left ~ satisfaction_level + last_evaluation + number_project 
                    + average_montly_hours + time_spend_company + Work_accident
                    + promotion_last_5years, 
                    data=highSalary, family=binomial(link="logit"))
summary(highSalaryModel)
#Calculating psedo R-squared: (% variance explained by regression)
1-(highSalaryModel$deviance/highSalaryModel$null.deviance)

#Satisfaction Vs Left

lowSatisfaction<-mydata[which(mydata$employee_satisfaction=="6.Minimum" | mydata$employee_satisfaction == "5.Low"), c(1:8,11)]
medSatisfaction<-mydata[which(mydata$employee_satisfaction=="4.Average" | mydata$employee_satisfaction == "3.Good"), c(1:8,11)]
highSatisfaction<-mydata[which(mydata$employee_satisfaction=="2.High" | mydata$employee_satisfaction == "1.Maximum"), c(1:8,11)]

lowSatisfactionModel<-glm(left ~ salaryOrder + last_evaluation + number_project 
                     + average_montly_hours + time_spend_company + Work_accident
                     + promotion_last_5years, 
                     data=lowSatisfaction, family=binomial(link="logit"))
summary(lowSatisfactionModel)
#Calculating psedo R-squared: (% variance explained by regression)
1-(lowSatisfactionModel$deviance/lowSatisfactionModel$null.deviance)

medSatisfactionModel<-glm(left ~ salaryOrder + last_evaluation + number_project 
                          + average_montly_hours + time_spend_company + Work_accident
                          + promotion_last_5years, 
                          data=medSatisfaction, family=binomial(link="logit"))
summary(medSatisfactionModel)
#Calculating psedo R-squared: (% variance explained by regression)
1-(medSatisfactionModel$deviance/medSatisfactionModel$null.deviance)

highSatisfactionModel<-glm(left ~ salaryOrder + last_evaluation + number_project 
                          + average_montly_hours + time_spend_company + Work_accident
                          + promotion_last_5years, 
                          data=highSatisfaction, family=binomial(link="logit"))
summary(highSatisfactionModel)
#Calculating psedo R-squared: (% variance explained by regression)
1-(highSatisfactionModel$deviance/highSatisfactionModel$null.deviance)

#********************IMPORTANT PREDICTORS********************

#Finding out the most important predictor covariates for "Left" column
#install.packages("Boruta")
library(Boruta)
boruta.train <- Boruta(left~last_evaluation+ number_project+ average_montly_hours+ 
                time_spend_company+ Work_accident+ satisfaction_level+ promotion_last_5years+ 
                role+ salaryOrder, data = mydata, doTrace = 2)

print(boruta.train)
plot(boruta.train, xlab = "", xaxt = "n")

lz<-lapply(1:ncol(boruta.train$ImpHistory),function(i)
  boruta.train$ImpHistory[is.finite(boruta.train$ImpHistory[,i]),i])
names(lz) <- colnames(boruta.train$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
     at = 1:ncol(boruta.train$ImpHistory), cex.axis = 0.7)

#********************T TEST********************

#T Test to confirm the hypothesis:

#Let's conduct a t-test at 95% confidence level and see if it correctly rejects the null hypothesis that the sample comes from the same distribution as the employee population. To conduct a one sample t-test, we can use the stats.ttest_1samp() function:
overallSatisfaction <-mean(mydata$satisfaction_level)
left_pop<-subset(mydata,left==1)

emp_turnover_satisfaction <-mean(left_pop$satisfaction_level)

#One Sample T Test
t.test(left_pop$satisfaction_level,mu=overallSatisfaction)
#p<0.05 - keep alt hypo; reject NULL hypo
#Reject the null hypothesis because:
#P-value is lower than confidence level of 5%

#Two Sample T Test
t.test(left_pop$satisfaction_level, mydata$satisfaction_level)
#p>0.05 - reject alt hypo
#p<0.05 - keep alt hypo; reject NULL hypo

#********************LINEAR MODEL FOR SATISFACTION********************

#Initial model - full model
initialModel<-lm(data=mydata, satisfaction_level ~.-employee_satisfaction)
summary(initialModel)

#Variable Selection Techniques and Remodeling:

nullmodel<-lm(data=mydata, satisfaction_level ~1)
summary(nullmodel)

fullmodel<-lm(data=mydata, satisfaction_level ~.-employee_satisfaction)
summary(fullmodel)

backwardModel = step(fullmodel, direction = "backward")
#Step:  AIC=-45048.18
#satisfaction_level ~ last_evaluation + number_project + average_montly_hours + time_spend_company + left

forwardModel = step(nullmodel, scope = list(lower = nullmodel, upper = fullmodel),direction = "forward")
#Step:  AIC=-45048.18
#satisfaction_level ~ leftFlag + number_project + last_evaluation + average_montly_hours + time_spend_company

bothDirectionModel = step(nullmodel, scope = list(lower = nullmodel, upper = fullmodel),direction = "both")
#Step:  AIC=-45048.18
#satisfaction_level ~ leftFlag + number_project + last_evaluation + average_montly_hours + time_spend_company

#Best Model
model<-lm(data=mydata, (satisfaction_level) ~ last_evaluation + number_project + left +average_montly_hours
          + time_spend_company)
summary(model)
#Best model excluding -....

par(mfrow=c(2,3))
#Termplot - Plots regression term(satisfaction_level) against their predictors, optionally with standard errors and partial residuals added.
#Compare the coefficients in the summary output to the slopes of the lines in the graphs. See how the coefficients match the slopes of the lines? That's because they are the slopes of the lines. For example, wt has a coefficient of about -5. Likewise the termplot for that coefficient has a negative slope that appears to be about -5. Using these plots we can see at a glance the respective contributions of the predictors. wt and qsec seem to contribute to the model due to the steepness of their slopes, but disp and drat not so much.
termplot(model)
#Left Variable is our most interested variable

par(mfrow=c(1,1))

#********************MODEL ADEQUACY CHECKING********************

#Check for Multi-collinearity - Using VIF. VIF threshold is 10
library(car)
vif(model)

#Normality Verification
qqnorm(model$residuals)
qqline(model$residuals, col = "red")
#Light tailed - but still the points are much more close to the line. Hence, the normality verification is done. No transformation is required.

#plot(model)
plot(model$fitted.values,model$residuals)

#abline: intercept = 0.5, slope = -0.95
#abline(a = 0.5,-0.95)
abline(h=0, col = "red")

library("MASS")
boxcox(model)

#No Transformation Required:
transformedModel<-lm(data=mydata, (satisfaction_level^1.1) ~ last_evaluation + number_project + left 
                  +average_montly_hours + time_spend_company)
qqnorm(transformedModel$residuals)
qqline(model$residuals, col = "red")

plot(transformedModel$fitted.values, transformedModel$residuals)
abline(h=0, col = "red")
boxcox(transformedModel)

summary(transformedModel)
#Since the R-squared value decreases in the transformed model and there is no significant change in the quality of the model, we do not perform any transformation. 
#We will use the initially found out best model

#Best Model
summary(model)
