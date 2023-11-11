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

# Example 6-4 ====
orders.df <- read.csv("Data_Orders.csv")
n <- dim(orders.df)[1]
x.bar <- mean(orders.df$Units)
mu.zero <- 40
s <- sd(orders.df$Units)
x.bar.se <- s / sqrt(n)
# |---p-value approach: ====
t <- (x.bar - mu.zero) / x.bar.se
# Note that p value includes both tails!
p <- 2 * min(pt(t, df = n - 1, lower.tail = TRUE),
             pt(t, df = n - 1, lower.tail = FALSE))
alpha <- 0.05
print(paste("p =", p, "and alpha =", alpha))
print(paste("p <= alpha is ", p <= alpha, ".", sep = ""))
print(paste("H0 is ", ifelse(p <= alpha, "", "not "), "rejected.", sep = ""))
#Since the null hypothesis is not rejected,
#the expected order quantity would be considered as 40 units, 
#as expected by the marketing director
# |---CV approach: ====
t <- (x.bar - mu.zero) / x.bar.se
alpha <- 0.05
# Note that alpha needs to be "distributed" among the two tails!
t.alpha <- qt(alpha / 2, df = n - 1, lower.tail = FALSE)
print(paste("t =", t, "and t.alpha =", t.alpha))
print(paste("Rejection region: (-inf, ", -t.alpha, "] and [", t.alpha, ", +inf)", sep = ""))
print(paste("t <= -t.alpha is ", t <= -t.alpha, ", t >= t.alpha is ", t >= t.alpha, ".", sep = ""))
print(paste("H0 is ", ifelse(t <= -t.alpha | t >= t.alpha, "", "not "), "rejected.", sep = ""))
#Since the null hypothesis is not rejected,
#the expected order quantity would be considered as 40 units, 
#as expected by the marketing director

# Example 6-5 ====
#Population proportion test statistic
wgolf.df <- read.csv("Data_WomenGolf.csv")
n <- dim(wgolf.df)[1]
#p-bar is the sample proportion
p.bar <- sum(wgolf.df$Golfer == "Female") / n
p.zero <- 0.20
p.bar.se <- sqrt(p.zero * (1 - p.zero) / n)
# |---p-value approach: ====
z <- (p.bar - p.zero) / p.bar.se
p <- pnorm(z, mean = 0, sd = 1, lower.tail = FALSE)
alpha <- 0.05
print(paste("p =", p, "and alpha =", alpha))
print(paste("p <= alpha is ", p <= alpha, ".", sep = ""))
print(paste("H0 is ", ifelse(p <= alpha, "", "not "), "rejected.", sep = ""))
#The data suggests that the proportion of women players has increased 
#significantly from the expected 20%, which means the promotion has 
#effective in attracting more women golfers to the course.
# |---CV approach: ====
z <- (p.bar - p.zero) / p.bar.se
alpha <- 0.05
z.alpha <- qnorm(alpha, mean = 0, sd = 1, lower.tail = FALSE)
print(paste("z =", z, "and z.alpha =", z.alpha))
print(paste("Rejection region: [", z.alpha, ", +inf)", sep = ""))
print(paste("z >= z.alpha is ", z >= z.alpha, ".", sep = ""))
print(paste("H0 is ", ifelse(z >= z.alpha, "", "not "), "rejected.", sep = ""))
#The data suggests that the proportion of women players has increased 
#significantly from the expected 20%, which means the promotion has 
#effective in attracting more women golfers to the course.

