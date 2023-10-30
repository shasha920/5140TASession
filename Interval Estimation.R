# Example 5-1 ====
#zα/2 represents the critical Z-score corresponding to a specific level 
#of significance (α) in the tails of a standard normal distribution
alpha <- 1 - 0.9
print(paste("z_alpha/2 when 1 - alpha = 0.9:",
            qnorm(1 - alpha / 2, mean = 0, sd = 1, lower.tail = TRUE)))
# Another way to implement the same thing:
print(paste("z_alpha/2 when 1 - alpha = 0.9:",
            qnorm(alpha / 2, mean = 0, sd = 1, lower.tail = FALSE)))
alpha <- 1 - 0.95
print(paste("z_alpha/2 when 1 - alpha = 0.95:",
            qnorm(1 - alpha / 2, mean = 0, sd = 1, lower.tail = TRUE)))
alpha <- 1 - 0.99
print(paste("z_alpha/2 when 1 - alpha = 0.99:",
            qnorm(1 - alpha / 2, mean = 0, sd = 1, lower.tail = TRUE)))

# Example 5-2 ====
#sigma known
lloyd.df <- read.csv("Data_Lloyd's.csv")
# |---Compute interval estimate when 1 - alpha = 0.95 ====
n <- dim(lloyd.df)[1]
#x.bar sample mean
x.bar <- mean(lloyd.df$Amount.Spent)
#sigma population standard deviation
sigma <- 20
alpha <- 1 - 0.95
#lower.tail default TRUE
z <- qnorm(1 - alpha / 2, mean = 0, sd = 1)
# Assume large enough population if not specified
#x.bar.se standard error associated with the sample mean
x.bar.se <- sigma / sqrt(n)
print(paste("Interval estimate lower bound:", x.bar - z * x.bar.se))
print(paste("Interval estimate upper bound:", x.bar + z * x.bar.se))

# Example 5-3 ====
#sigma unknown
alpha <- 1 - 0.9
n <- 65
print(paste("t_alpha/2 when 1 - alpha = 0.9 and n = 65:",
            qt(1 - alpha / 2, df = n - 1)))
alpha <- 1 - 0.95
n <- 95
print(paste("t_alpha/2 when 1 - alpha = 0.95 and n = 95:",
            qt(1 - alpha / 2, df = n - 1, lower.tail = TRUE)))
alpha <- 1 - 0.99
n <- 95
print(paste("t_alpha/2 when 1 - alpha = 0.99 and n = 95:",
            qt(1 - alpha / 2, df = n - 1, lower.tail = TRUE)))

# Example 5-4 ====
#sigma unknown
nb.df <- read.csv("Data_NewBalance.csv")
# |---Compute interval estimate when 1 - alpha = 0.95 ====
n <- dim(nb.df)[1]
x.bar <- mean(nb.df$NewBalance)
#s is sample sd
s <- sd(nb.df$NewBalance)
alpha <- 1 - 0.95
t <- qt(1 - alpha / 2, df = n - 1, lower.tail = TRUE)
# Assume large enough population if not specified
x.bar.se <- s / sqrt(n)
print(paste("Interval estimate lower bound:", x.bar - t * x.bar.se))
print(paste("Interval estimate upper bound:", x.bar + t * x.bar.se))

# Example 5-5 ====
scheer.df <- read.csv("Data_Scheer.csv")
# |---Compute interval estimate when 1 - alpha = 0.95 ====
n <- dim(scheer.df)[1]
x.bar <- mean(scheer.df$Days)
s <- sd(scheer.df$Days)
alpha <- 1 - 0.95
t <- qt(1 - alpha / 2, df = n - 1, lower.tail = TRUE)
# Assume large enough population if not specified
x.bar.se <- s / sqrt(n)
print(paste("Interval estimate lower bound:", x.bar - t * x.bar.se))
print(paste("Interval estimate upper bound:", x.bar + t * x.bar.se))

# Example 5-6 ====
# |---Compute minimum sample size given desired margin of error ====
sigma <- 4500
alpha <- 1 - 0.95
#E is the desired margin of error
E <- 500
z <- qnorm(1 - alpha / 2, mean = 0, sd = 1)
print(paste("The minimum sample size should be:", (z ^ 2) * (sigma ^ 2) / (E ^ 2)))
