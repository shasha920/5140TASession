# Example 5-6 ====
# |---Compute minimum sample size given desired margin of error ====
sigma <- 4500
alpha <- 1 - 0.95
E <- 500
z <- qnorm(1 - alpha / 2, mean = 0, sd = 1)
print(paste("The minimum sample size should be:", (z ^ 2) * (sigma ^ 2) / (E ^ 2)))

# Example 5-7 ====
tt.df <- read.csv("Data_TeeTimes.csv")
# |---Compute interval estimate when 1 - alpha = 0.95 ====
n <- dim(tt.df)[1]
p.bar <- sum(tt.df$Response == "Yes") / n
alpha <- 1 - 0.95
z <- qnorm(1 - alpha / 2, mean = 0, sd = 1)
p.bar.se <- sqrt(p.bar * (1 - p.bar) / n)
print(paste("Interval estimate lower bound:", p.bar - z * p.bar.se))
print(paste("Interval estimate upper bound:", p.bar + z * p.bar.se))
# |---Compute minimum sample size given desired margin of error ====
p.star <- 0.5
alpha <- 1 - 0.95
E <- 0.025
z <- qnorm(1 - alpha / 2, mean = 0, sd = 1)
print(paste("The minimum sample size should be:", z ^ 2 * p.star * (1 - p.star) / E ^ 2))

#Example 6-1 sigma Known
coffee.df <- read.csv("Data_Coffee.csv")
n<-dim(coffee.df)[1]
x.bar <- mean(coffee.df$Weight)
mu.zero<-3
#Assume population standard deviation is known
sgima<-0.18
x.bar.se<-sigma/sqrt(n)
#p-value approach:
z<-(x.bar-mu.zero)/x.bar.se
p<-pnorm(z,mean=0,sd=1)
alpha<-0.01
print(paste("p=",p,"and alpha=",alpha))
print(paste("p<=alpha is ",p<=alpha,".",sep=""))
print(paste("H0 is ",ifelse(p<=alpha,"","not "),"rejected.",sep=""))
#there is not enough evidence to suggest that the coffee producer is 
#lying about the contents of the coffee cans.
#CV approach:
z<-(x.bar-mu.zero)/x.bar.se
alpha<-0.01
z.alpha<-qnorm(alpha,mean=0,sd=1)
print(paste("z=",z,"and z.alpha =",z.alpha))
print(paste("Rejection region: (-inf,", z.alpha, ")",sep="" ))
print(paste("z<=z.alpha is",z<=z.alpha,".",sep=""))
print(paste("H0 is ",ifelse(z<=z.alpha,"","not "),"rejected.",sep="")) 
#there is not enough evidence to suggest that the coffee producer is 
#lying about the contents of the coffee cans.

#Example6-2 sigma Known
golf.df<-read.csv("Data_GolfTest.csv")
n<-dim(golf.df)[1]
x.bar<-mean(golf.df$Yards)
mu.zero<-295
#Assume population standard deviation is known
sigma<-12
x.bar.se<-sigma/sqrt(n)
#p-value approach:
z<-(x.bar-mu.zero)/x.bar.se
#Note that p value inclueds both tails!
p<-2*min(pnorm(z,mean=0,sd=1,lower.tail=TRUE),
         pnorm(z,mean=0,sd=1,lower.tail = FALSE))
alpha<-0.05
print(paste("p=",p,"and alpha=",alpha))
print(paste("p<=alpha is ",p<=alpha,".",sep=""))
print(paste("H0 is ",ifelse(p<=alpha,"","not "),"rejected.",sep=""))
#isn't sufficient evidence to claim that the driving distance has shifted significantly
#from the standard 295 yards.
#CV approach:
z<-(x.bar-mu.zero)/x.bar.se
alpha<-0.05
#Note that alpha needs to be "distribute" among the two tails!
z.alpha<-qnorm(alpha/2,mean=0,sd=1,lower.tail = FALSE)
print(paste("z=",z,"and z.alpha =",z.alpha))
print(paste("Rejection rejoin:(-inf,",-z.alpha,"] and [",z.alpha,",+inf)",sep=""))
print(paste("z<=-z.alpha is",z<=-z.alpha,",z>=z.alpha is",z>=z.alpha, ".",sep=""))
print(paste("H0 is ",ifelse(z<=-z.alpha | z>=z.alpha,"","not "),"rejected.",sep=""))
#isn't sufficient evidence to claim that the driving distance has shifted significantly
#from the standard 295 yards.

#Example 6-3 sigma Unknown
rating.df<-read.csv("Data_AirRating.csv")
n<-dim(rating.df)[1]
x.bar<-mean(rating.df$Rating)
mu.zero<-7
#s meaning sample standard deviation 
s<-sd(rating.df$Rating)
x.bar.se<-s/sqrt(n)
#p-value approach:
t<-(x.bar-mu.zero)/x.bar.se
p<-pt(t,df=n-1,lower.tail = FALSE)
alpha<-0.05
print(paste("p=",p,"and alpha=",alpha))
print(paste("p<=alpha is ",p<=alpha,".",sep=""))
print(paste("H0 is ",ifelse(p<=alpha,"","not "),"rejected.",sep=""))
#Since the null hypothesis is rejected, it implies that the data indicate 
#that the airport in question has a mean rating greater than 7, 
#and thus can be classified as a superior service airport.
#CV approach:
t<-(x.bar-mu.zero)/x.bar.se
alpha<-0.05
t.alpha<-qt(alpha,df=n-1,lower.tail = FALSE)
print(paste("t=",t,"and t.alpha=",t.alpha))
print(paste("Rejection region:[",t.alpha,",+inf)",sep=""))
print(paste("t>=t.alpha is ",t>=t.alpha,".",sep=""))
print(paste("H0 is ",ifelse(t>=t.alpha,"","not "),"rejected.",sep=""))
#Since the null hypothesis is rejected, it implies that the data indicate 
#that the airport in question has a mean rating greater than 7, 
#and thus can be classified as a superior service airport.

