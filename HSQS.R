getwd()
dataset <- read.csv("HSQS.csv")
head(dataset)


#Task A
cor.test(dataset$Quality_Score,dataset$Hospital_ID)

install.packages('ggplot')
library(ggplot2)
#Scatter plot
plot(dataset$Quality_Score ~ dataset$Hospital_ID
     ,xlab='Hospital ID',ylab = 'Quality score',
     xlim = c(101,200), ylim=c(60,90),pch=20,col='red')

#Boxplot
boxplot(dataset$Quality_Score ~ dataset$Hospital_ID,
        xlab = "Hospital ID",
        ylab = "Quality Score",
        col = "lightblue",
        main = "Boxplot of Quality Score by Hospital ID",
        border = "darkblue")
cor.test(dataset$Quality_Score,dataset$Avg_Length_of_Stay)

#Scatter plot 
plot(dataset$Quality_Score ~ dataset$Avg_Length_of_Stay
     ,xlab='Average length of stay',ylab = 'Quality score',
     xlim = c(3.0,6.0), ylim=c(60,90),pch=20,col='red')

#box plot
boxplot(dataset$Quality_Score ~ dataset$Avg_Length_of_Stay,
        xlab = "Average length of stay",
        ylab = "Quality Score",
        col = "lightblue",
        main = "Boxplot of Quality Score by Average Length of Stay",
        border = "darkblue")


cor.test(dataset$Quality_Score,dataset$Readmission_Rate....)

#Scatter plot 
plot(dataset$Quality_Score ~ dataset$Readmission_Rate....,
     xlab = 'Readmission Rate',
     ylab = 'Quality Score',
     xlim = c(1, 20),
     ylim = c(60, 90),
     pch = 20,
     col = 'red')


#boxplott
#first break into group
dataset$Readmission_Rate_Group <- cut(dataset$Readmission_Rate...., 
breaks = 5, labels = c("Very Low", "Low", "Medium", "High", "Very High"))

# Create the boxplot
boxplot(dataset$Quality_Score ~ dataset$Readmission_Rate_Group,
        xlab = "Readmission Rate Group",
        ylab = "Quality Score",
        col = "lightblue",
        main = "Boxplot of Quality Score by Readmission Rate Group",
        border = "darkblue")


cor.test(dataset$Quality_Score,dataset$Patient_Satisfaction....) 

#Scatter plot 
plot(dataset$Quality_Score ~ dataset$Patient_Satisfaction....
     ,xlab='Patient Satisfiction',ylab = 'Quality score',
     xlim = c(70,90), ylim=c(60,90),pch=20,col='red')

#Boxplot
boxplot(dataset$Quality_Score ~ dataset$Patient_Satisfaction....,
        xlab = "Patient Satisfiction",
        ylab = "Quality Score",
        col = "lightblue",
        main = "Boxplot of Quality Score by Patient Satisfiction",
        border = "darkblue")

cor.test(dataset$Quality_Score,dataset$Staff_to_Patient_Ratio) 

#Scatter plot 
plot(dataset$Quality_Score ~ dataset$Staff_to_Patient_Ratio
     ,xlab='Staff to patient ratio',ylab = 'Quality score',
     xlim = c(3.5,5), ylim=c(60,90),pch=20,col='red')

#Boxplot
boxplot(dataset$Quality_Score ~ dataset$Staff_to_Patient_Ratio,
        xlab = "Staff to patient ratio",
        ylab = "Quality Score",
        col = "lightblue",
        main = "Boxplot of Quality Score by Staff to patient ratio",
        border = "darkblue")

cor.test(dataset$Quality_Score,dataset$Operating_Costs....) 

#Scatter plot 
plot(dataset$Quality_Score ~ dataset$Operating_Costs....
     ,xlab='Operating Cost',ylab = 'Quality score',
     xlim = c(1000000,2000000), ylim=c(60,90),pch=20,col='red')

#Boxplot
boxplot(dataset$Quality_Score ~ dataset$Operating_Costs....,
        xlab = "Operating Cost",
        ylab = "Quality Score",
        col = "lightblue",
        main = "Boxplot of Quality Score by Operating Cost",
        border = "darkblue")

#Linear Regression

plot(dataset$Quality_Score ~ dataset$Readmission_Rate....,
     xlab = 'Readmission Rate',
     ylab = 'Quality Score',
     xlim = c(1, 20),
     ylim = c(60, 90),
     pch = 20,
     col = 'red')

Model<-lm(dataset$Quality_Score~dataset$Readmission_Rate....)
abline(Model, col = 'blue', lwd = 2)
summary(Model)






plot(dataset$Quality_Score ~ dataset$Staff_to_Patient_Ratio
     ,xlab='Staff to patient ratio',ylab = 'Quality score',
     xlim = c(3.5,5), ylim=c(60,90),pch=20,col='red')

Model<-lm(dataset$Quality_Score~dataset$Staff_to_Patient_Ratio)
abline(Model, col = 'blue', lwd = 2)
summary(Model)s
