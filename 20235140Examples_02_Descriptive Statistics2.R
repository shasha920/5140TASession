library(e1071)

#Example 2-5
start.salary.df<-read.csv("Data_02_2012StartSalary.csv")
names(start.salary.df)[2]<-"Monthly.Starting.Salary"
#Measures of location
print(paste("80th percentile:",quantile(start.salary.df$Monthly.Starting.Salary,0.8,type=6)))
print(paste("First quartile:",quantile(start.salary.df$Monthly.Starting.Salary,0.25,type=6)))
print(paste("Second quartile:",quantile(start.salary.df$Monthly.Starting.Salary,0.5,type=6)))
print(paste("Third quartile:",quantile(start.salary.df$Monthly.Starting.Salary,0.75,type=6)))
#Measure of variablity
print(paste("Range:",max(start.salary.df$Monthly.Starting.Salary)-min(start.salary.df$Monthly.Starting.Salary)))
print(paste("IQR:",quantile(start.salary.df$Monthly.Starting.Salary,0.75,type=6)-
              quantile(start.salary.df$Monthly.Starting.Salary,0.25,type=6)))
print(paste("Variance:",var(start.salary.df$Monthly.Starting.Salary)))
print(paste("Standard deviation:",sd(start.salary.df$Monthly.Starting.Salary)))
print(paste("Coefficient of variation:",sd(start.salary.df$Monthly.Starting.Salary)/
              mean(start.salary.df$Monthly.Starting.Salary)))
#Measure of distribution shape
#Package e1071 is required for computing skewness
#set type=2 to compute skewness using the formula in book
print(paste("Skewness:",skewness(start.salary.df$Monthly.Starting.Salary,type=2)))
#results shows 1.09 which meaning noticeable skewed right
#Five number summary
print(paste("Smallest value:",min(start.salary.df$Monthly.Starting.Salary)))
print(paste("First quartile:",quantile(start.salary.df$Monthly.Starting.Salary,0.25,type=6)))
print(paste("Median:",quantile(start.salary.df$Monthly.Starting.Salary,0.5,type=6)))
print(paste("Third quartile:",quantile(start.salary.df$Monthly.Starting.Salary,0.75,type=6)))
print(paste("Largest value:",max(start.salary.df$Monthly.Starting.Salary)))

#Example 2-6
major.salary.df<-read.csv("Data_02_2012MajorSalary.csv")
names(major.salary.df)[2]<-"Monthly.Starting.Salary"
#Box plot
boxplot(major.salary.df$Monthly.Starting.Salary~major.salary.df$Major,
        xlab="Major",ylab="Monthly Starting Salary",
        main="Box Plot of Monthly Starting Salary by Major")


#Example 2-7
stereo.df<-read.csv("Data_02_Stereo.csv")
names(stereo.df)[2]<-"Num.Commercials"
#Sample covariance and correlation coefficient
#covariance refers to the measure of the directional relationship between two random variables.
print(paste("Covariance:",cov(stereo.df$Num.Commercials,stereo.df$Sales.Volume)))
#positive relationship between the two variables
print(paste("Correlation coefficient:",cor(stereo.df$Num.Commercials,stereo.df$Sales.Volume)))
#positive association between two variables

