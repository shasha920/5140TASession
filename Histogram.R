# install.packages("e1071")
library(e1071)

#Example 2-2
audit.df<-read.csv("Data_02_Audit.csv")
#nclass.Sturges uses Sturges' formula
#implicitly basing bin sizes on the range of the data.
#nclass.scott uses Scott's choice for a normal distribution 
#based on the estimate of the standard error
#unless that is zero where it returns 1.
#nclass.FD uses the Freedman-Diaconis choice based on the inter-quartile 
#range (IQR(signif(x, 5))) unless that's zero where it uses increasingly 
#more extreme symmetric quantiles up to c(1,511)/512 and if 
#that difference is still zero, reverts to using Scott's choice.
audit.time.bin.num<-nclass.Sturges(audit.df$Audit.Time)
#audit.time.bin.num<-nclass.scott(audit.df$Audit.Time)
#audit.time.bin.num<-nclass.FD(audit.df$Audit.Time)
audit.df$Audit.Time.Bin<-cut(audit.df$Audit.Time,audit.time.bin.num)
audit.freq<-data.frame(table(audit.df$Audit.Time.Bin))
names(audit.freq)[1]<-"Audit.Time.Bin"
audit.freq
#Relative and percent frequency distribution
audit.freq$Rel.Freq<-audit.freq$Freq/sum(audit.freq$Freq)
audit.freq$Pct.Freq<-100.00*audit.freq$Rel.Freq
audit.freq
#Cumulative(relative and percent) frequency distribution
audit.freq$Cum.Freq<-cumsum(audit.freq$Freq)
audit.freq$Cum.Rel.Freq<-cumsum(audit.freq$Rel.Freq)
audit.freq$Cum.Pct.Freq<-cumsum(audit.freq$Pct.Freq)
audit.freq
#Histofram
#Appropriate number of classes is 
#determined by either Sturges' rule,
#Scott's rule, or Freedman-Diaconis's rule
#although the breakpoints are adjusted
#to be "pretty"
hist(audit.df$Audit.Time,breaks="FD",xlab="Audit Time",main="Histogram of Audit Time")
