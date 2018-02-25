setwd("C:/Users/surya/Desktop/SpringSemester")

install.packages("caret")
library(caret)
install.packages("corrplot")
library(corrplot)
install.packages("dplyr")
library(dplyr) 
install.packages("rattle")
library(rattle)

#load the data
mydata = read.csv("HR_comma_sep.csv")
dim(mydata)
str(mydata)
summary(mydata)
#Get the class
sapply(mydata,class)

par(mfrow=c(1,1))

corrplot(cor(mydata[ ,c("satisfaction_level","last_evaluation","number_project","average_montly_hours","time_spend_company","left")]), method = "square", type="full") # Check for correlations

#tempData = mydata[,c( "roleN","salaryOrder","satisfaction_level","last_evaluation","number_project","promotion_last_5years","Work_accident", "average_montly_hours","time_spend_company","left")]
#corrplot( cor(as.matrix(tempData), method = "pearson", use = "complete.obs") ,is.corr = FALSE, type = "lower", order = "hclust", 
 #         tl.col = "black", tl.srt = 45)
mydata$salaryOrder
attach(mydata)

#Adding a new column called 'satisfaction'
mydata$satisfaction[satisfaction_level >= 0.9] = '1.Maximum'
mydata$satisfaction[satisfaction_level >= 0.8 & satisfaction_level < 0.9 ] = '2.High'
mydata$satisfaction[satisfaction_level >= 0.6 & satisfaction_level < 0.8 ] = '3.Good'
mydata$satisfaction[satisfaction_level >= 0.4 & satisfaction_level < 0.6 ] = '4.Average'
mydata$satisfaction[satisfaction_level >= 0.2 & satisfaction_level < 0.4 ] = '5.Low'
mydata$satisfaction[satisfaction_level <  0.2] = '6.Minimum'

#Converting the satisfaction column as a factor
mydata$satisfaction = as.factor(mydata$satisfaction)

#one more new variable for 'left' for string representation.
mydata$leftFlag[mydata$left ==  1] = 'Left'
mydata$leftFlag[mydata$left ==  0] = 'Not Left'

#Create a barplot 'Employees left vs Satisfaction'
SatisfactionAndLeftTable <- table(mydata$leftFlag, mydata$satisfaction)
barplot(SatisfactionAndLeftTable, main="Satisfaction Vs Employees Left / Not Left",
        xlab="Satisfaction Level", col=c("purple","orange"),
        legend = rownames(SatisfactionAndLeftTable), beside=TRUE)

projectsPlotData <- table(mydata$leftFlag, mydata$number_project)
barplot(projectsPlotData, main="Employees Left / Not Left vs No. of Projects",
        xlab="Number of Projects", col=c("purple","orange"),
        legend = rownames(projectsPlotData), beside=TRUE)

projectsPlotData <- table(mydata$satisfaction_level, mydata$number_project)
barplot(projectsPlotData, main="Satisfaction Level",
        xlab="Number of Projects", col=c("purple","orange"),
        legend = rownames(projectsPlotData), beside=TRUE)

par(mfrow=c(1,4))
par(mfrow=c(2,2))

hist(mydata$satisfaction_level, main = "Histogram of Satisfaction", xlab = "Satisfaction")
hist(mydata$last_evaluation, main = "Histogram of Last Evaluation", xlab = "Last Evaluation")
#hist(mydata$number_project, main = "Histogram of No. of Projects", xlab = "No. of Projects")
hist(mydata$average_montly_hours, main = "Histogram of Avg Hours Spent", xlab = "Avg Hours Spent")
hist(mydata$time_spend_company, main = "Histogram of Time Spent in Company", xlab = "Time Spent in Company")

boxplot(mydata$satisfaction_level, main = "Satisfaction")
boxplot(mydata$last_evaluation, main = "Last Evaluation")
#boxplot(mydata$number_project, main = "No. of Projects")
boxplot(mydata$average_montly_hours, main = "Avg Hours Spent")
boxplot(mydata$time_spend_company, main = "Time Spent in Company")

vis_1<-table(mydata$salaryOrder,(mydata$satisfaction))
#print(vis_1)
d_vis_1<-as.data.frame(vis_1)
d_vis_1$salaryOrder = d_vis_1$Var1
d_vis_1$leftFlag = d_vis_1$Var2
d_vis_1$Var1= NULL
d_vis_1$Var2= NULL

print(d_vis_1)
library(ggplot2)

p<-ggplot(d_vis_1, aes(x=salaryOrder,y=Freq,fill=leftFlag)) +
  geom_bar(position="dodge",stat='identity') 

print(p)

roleTable<-table(mydata$role,mydata$left)
roledf<-as.data.frame(roleTable)
roledf$role = roledf$Var1
roledf$leftFlag = roledf$Var2
roledf$Var1= NULL
roledf$Var2= NULL

roledfLeft<-subset(roledf,leftFlag==1)
print(roledfLeft)

roledfLeft$role <- factor(roledfLeft$role, levels = roledfLeft$role[order(-roledfLeft$Freq)])
p<-ggplot(roledfLeft, aes(x=role,y=Freq,fill=role)) +
  geom_bar(stat='identity') +theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p)

sales = mydata[mydata$role=='sales',]
mean(sales$satisfaction_level)

p<-ggplot(mydata, aes(x = factor(number_project), y = satisfaction_level, fill=factor(left))) +
  geom_boxplot() + scale_fill_manual(values = c("orange", "purple"))
  print(p)
  
model<-lm(satisfaction_level ~ last_evaluation + number_project + left
                               + time_spend_company, data=mydata)
                
###Build a logistic regression using unstandardized variables
reg_all_us<-glm(left ~ satisfaction_level + last_evaluation + number_project 
                + average_montly_hours + time_spend_company + Work_accident
                + promotion_last_5years + salaryOrder, 
                data=mydata, family=binomial(link="logit"))
summary(reg_all_us)
AIC(reg_all_us)
library("cart")

#model<-lm(satisfaction_level ~ last_evaluation + left, 
#          data=mydata)

#model<-lm(satisfaction_level ~ . - role - salary - satisfaction - leftFlag - Work_accident - promotion_last_5years -salaryOrder, data=mydata)

#Perfect
linearModel<-lm(satisfaction_level ~ last_evaluation + number_project+average_montly_hours+time_spend_company+left)
summary(linearModel)
#plot(model)
b = coef(linearModel)
par(mfrow=c(2,3))
termplot(linearModel, partial.resid=T ,ylabs=c("satisfaction","satisfaction","satisfaction","satisfaction", "satisfaction"), xlabs = c("Last Evaluation", "No. of Projects", "Avg Monthy Hours", "Time spent in company", "Left"))

plot(last_evaluation, satisfaction_level)
abline(model)

with(mydata, {
  plot(number_project, satisfaction_level, pch=16, col=rgb(0,0,0,0.1), ylim=c(0,10))
  abline(b[1], b[2], lwd=2)
}
)

m1=lm(satisfaction_level~., data=mydata1)

m.n = lm(satisfaction_level~1, data = mydata1)

m.f = step(m.n, scope=list(lower=m.n, upper=m1), direction = 'forward')

m2=lm(satisfaction_level ~ left + number_project + last_evaluation + 
        average_montly_hours + time_spend_company + satisfaction, data =mydata)

m3=lm(satisfaction_level~. - role - salary -left, mydata)
summary(m3)


abline(model)

par(mfrow=c(2,3))
termplot(model, ylabs=c("satisfaction","satisfaction","satisfaction","satisfaction"), xlabs = c("Last Evaluation", "No. of Projects", "Left",  "Time spent in company"))

sub = mydata[mydata$number_project>5,]
sub$lasteval[sub$last_evaluation >= 0.8] = '1.Very Good'
sub$lasteval[sub$last_evaluation >= 0.6 & last_evaluation < 0.8 ] = '2.Average'
sub$lasteval[sub$last_evaluation < 0.6 ] = '3.Bad'
summary(sub)
