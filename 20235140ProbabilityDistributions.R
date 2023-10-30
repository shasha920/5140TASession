library(e1071)

mtcars_df <- read.csv("Data-MTCars.csv")
head(mtcars_df)

#descriptive statistics
#Determine the appropriate number of bins for variable “mpg” using Sturge’s method, 
#and generate the corresponding frequency distribution, 
#relative frequency distribution, percent frequency distribution,
#cumulative frequency distribution, 
#cumulative relative frequency distribution, 
#and cumulative percent distribution.
mpg_bin_num<-nclass.Sturges(mtcars_df$mpg)
mtcars_df$mpg_bin <- cut(mtcars_df$mpg, mpg_bin_num)
mpg_freq <- data.frame(table(mtcars_df$mpg_bin))
names(mpg_freq)[1] <- "mpg_freq"
mpg_freq$Rel_Freq <- mpg_freq$Freq / sum(mpg_freq$Freq)
mpg_freq$Pct_Freq <- 100.00 * mpg_freq$Rel_Freq
mpg_freq$Cum_Freq <- cumsum(mpg_freq$Freq)
mpg_freq$Cum_Rel_Freq <- cumsum(mpg_freq$Rel_Freq)
mpg_freq$Cum_Pct_Freq <- cumsum(mpg_freq$Pct_Freq)
mpg_freq

#Compute minimum,First quartile,median, mean, Third quartile, 
#maximum, and standard deviation of variable “hp”.
#summary(mtcars_df)
summary(mtcars_df$hp)
print(paste("Standard deviation:",sd(mtcars_df$hp)))

#Probability Distribution
#Automobile repair costs continue to rise with the average cost now at $367 per repair. 
#Assume that the cost for an automobile repair is normally distributed with a standard deviation of $88. 

#What is the probability that the cost will be more than $450?
#lower.tail default is TRUE
print(paste("the probability that the cost will be more than $450: ",pnorm(450,mean=367,sd=88,lower.tail = FALSE)))

#What is the probability that the cost will be less than $250?
print(paste("the probability that the cost will be less than $250: ",pnorm(250,mean=367,sd=88)))

#What is the probability that the cost will be between $250 and $450?
print(paste("the probability that the cost will be between $250 and $450: ",pnorm(450,mean=367,sd=88)-pnorm(250,mean=367,sd=88)))

#If the cost of your car repair is in the lower 5% of automobile repair charges, what is your cost?
#qnorm is used to find a value along the x-axis that put certain percent of area
#under the curve in one of the tails
print(paste("If the cost of your car repair is in the lower 5% of automobile repair charges, cost: ",qnorm(0.05,mean=367,sd=88)))
