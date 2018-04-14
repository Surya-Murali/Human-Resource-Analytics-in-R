setwd("C:/Users/surya/Desktop/SpringSemester")

#load the data
mydata = read.csv("HR_comma_sep.csv")

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

#Data Summary
dim(mydata)
str(mydata)
summary(mydata)
#Get the class
sapply(mydata,class)

#par(mfrow=c(1,1))
corrplot(cor(mydata[ ,c("satisfaction_level","last_evaluation","number_project","average_montly_hours","time_spend_company","left")]), method = "square", type="full") # Check for correlations

#tempData = mydata[,c( "roleN","salaryOrder","satisfaction_level","last_evaluation","number_project","promotion_last_5years","Work_accident", "average_montly_hours","time_spend_company","left")]
#corrplot( cor(as.matrix(tempData), method = "pearson", use = "complete.obs") ,is.corr = FALSE, type = "lower", order = "hclust", 
#         tl.col = "black", tl.srt = 45)

#attach(mydata)

#********************EDA********************

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
