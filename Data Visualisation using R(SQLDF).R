
#Importing data
studentresult <- read.delim('studentresult.csv',sep = ',',stringsAsFactors = F)
View(studentresult)

#Some exploratory analysis
str(studentresult)
ncol(studentresult)
nrow(studentresult)
length(studentresult)

names(studentresult)
dim(studentresult)

#ddeleting an object
remove(studentresult)

#summary statistics
summary(studentresult)

#Dataset check at a glance
head(studentresult)
tail(studentresult)

#installing required packages
library(ggplot2)
install.packages("sqldf")
library(sqldf)

#use of sql commands for data analysis
markswritten <- sqldf('select Name, avg(Mark_Written) as Written_marks from studentresult group by Name')
View(markswritten)
#OR
markswritten2 <- aggregate(data=studentresult, Mark_Written~Name, mean)
View(markswritten2)

avg_mark <- sqldf("select avg(Mark_Oral) from studentresult where Name ='Mary Healy' AND Mark_Oral is not 'NA'")
avg_mark

studentresult$Mark_Oral <- ifelse(is.na(studentresult$Mark_Oral),as.numeric(avg_mark),studentresult$Mark_Oral)
studentresult$Mark_Oral

namesAvgmarkplot <- ggplot(data = markswritten,aes(x=Name,y=Written_marks))+ geom_bar(stat='identity',aes(fill=Name),width = 0.25)
namesAvgmarkplot + scale_fill_manual(values = c("Hercule Poirot"='steelblue',"Joe O'Neil"="firebrick", "Mary Healy"="darkgreen"))
ggsave("Exercise1.pdf")

marksoral<-aggregate(data=studentresult, Mark_Oral~Name,mean)
ggplot(data=marksoral, aes(x= Name, y=Mark_Oral)) + geom_bar(stat="identity") + ggsave("Exercise2.pdf")

mby <- sqldf('select name,year,SUM(Mark_written)/5 as mark from studentresult group by year,name')
mby

ggplot(data = mby,aes(x=Name, y=mark, fill=as.factor(Year))) +geom_bar(stat = 'identity') + ggsave("Exercise3.pdf")
  
getAge <- function(d){
  now <- as.Date(Sys.Date())
  then <- as.Date(d,format="%d-%m-%Y")
  result <- now-then
  return(round(as.numeric(result/365)))
}

studentresult$age <- getAge(studentresult$DOB)
View(studentresult)


##Total marks per student per subject

studentresult$total_marks <- (studentresult$Mark_Written+studentresult$Mark_Oral)/2
total_marks <- sqldf('select Name,subject,total_marks as Total from studentresult group by name,subject')
total_marks

Q1plot <- ggplot(data=studentresult,aes(x=Subject,y=total_marks, fill = as.factor(Name))) + geom_bar(stat = 'identity')
Q1plot <- Q1plot + theme(plot.title = element_text(size = 20, face = "bold"))+theme(axis.title.y = element_text(colour="grey20",size=15,face="bold"),axis.title.x = element_text(colour="grey20",size=15,face="bold"))
Q1plot <- Q1plot + scale_fill_manual(values = c("Hercule Poirot"="darkblue","Joe O'Neil"="steelblue","Mary Healy"="Firebrick"))
Q1plot + ggtitle("Total marks per student per subject") + facet_grid(.~Year) +ggsave("Question1.pdf")



##Relation between Age and Marks of a student

Q2plot1 <- ggplot(data=studentresult,aes(x=studentresult$age,y=studentresult$total_marks)) + geom_point() +theme(plot.title = element_text(size = 20, face = "bold"))
Q2plot1 <- Q2plot1 + geom_smooth(method = 'lm',se=F,color='blue')+theme(axis.title.y = element_text(colour="grey20",size=15,face="bold"),axis.title.x = element_text(colour="grey20",size=15,face="bold"))
Q2plot1 + ggtitle("Relation between age and marks") + ggsave("Question2a.pdf")
#There is a negative correlation evident from graph which statistically implies that increase in Age of a student decreases overallmarks.

#Let us check this for each of the subjects
Q2plot2 <- ggplot(data=studentresult,aes(x=studentresult$age,y=studentresult$total_marks)) + geom_point() +theme(plot.title = element_text(size = 20, face = "bold"))
Q2plot2 <- Q2plot2 + geom_smooth(method = 'lm',se=F,color='blue')+theme(axis.title.y = element_text(colour="grey20",size=15,face="bold"),axis.title.x = element_text(colour="grey20",size=15,face="bold"))
Q2plot2 + ggtitle("Relation between age and marks for each subject") + facet_grid(.~Subject)
ggsave("Question2b.pdf") 
# It is evident that for each of the subjects same trend sustains, and for Japanese subject the marks vary largely depending on age.



##Did any students do better on their written compared with their oral (or vice versa)?

marks_stu <- sqldf('select Name,SUM(Mark_Written) as ttl_w,SUM(Mark_Oral) as ttl_o from studentresult group by name')
marks_stu

Q3plot <- ggplot(data = marks_stu,aes(x=ttl_o,y=ttl_w,fill = Name)) + xlab("Marks written") + ylab("Marks Oral") +theme(axis.title.y = element_text(colour="grey20",size=15,face="bold"),axis.title.x = element_text(colour="grey20",size=15,face="bold"))
Q3plot <- Q3plot + ggtitle("Students performance on oral and written exams") + geom_bar(stat='identity') +theme(plot.title = element_text(size = 20, face = "bold"))
Q3plot + ggsave("Question3a.pdf")
# From the graph it is clear that actually all the 3 students did well in written exam than oral but a major difference in performance
# is found in student Mary Healy.

#To see the exact difference

df2 <- melt(marks_stu,id.vars = 'Name')
extraplot <- ggplot(df2,aes(x=Name,y=value,fill=variable)) + geom_bar(stat='identity', position = 'dodge') + theme(plot.title = element_text(size = 20, face = "bold")) 
extraplot <- extraplot + ggtitle("Difference in Oral and Written")+theme(axis.title.y = element_text(colour="grey20",size=15,face="bold"),axis.title.x = element_text(colour="grey20",size=15,face="bold"))
extraplot + ggsave("Exercise3b.pdf")

##Subject with best results on average

bestresult <- sqldf('select Subject,sum(total_marks) as besttotal from studentresult group by subject')
bestresult

Q4plot <- ggplot(data = bestresult,aes(x=Subject,y=besttotal)) + geom_bar(position = "dodge", colour = "black", stat = "identity",aes(fill=Subject))+theme(axis.title.y = element_text(colour="grey20",size=15,face="bold"),axis.title.x = element_text(colour="grey20",size=15,face="bold"))
Q4plot <- Q4plot + geom_text(aes(label = bestresult$besttotal), size = 5) +scale_fill_brewer(palette = 'Pastel2')
Q4plot +theme(plot.title = element_text(size = 20, face = "bold")) +ggtitle("Subject with best results")
ggsave("Question4.pdf") 
# The graph clearly demonstrates that Irish is the subject which got best results