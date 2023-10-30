# Categorical Data
# Let's create a variable names ses (social-economic status)
ses <- c("low", "middle", "low", "low", "low", "low", "middle", "low", "middle",
         "middle", "middle", "middle", "middle", "high", "high", "low", "middle",
         "middle", "low", "high")
# This variable is a character variable
# We can easily see that this is a character variable in R's definition
class(ses)

# Frequency Distribution
table(ses)

# Relative Frequency
table(ses)/sum(table(ses))

# Cumulative Frequency Table
cumsum(table(ses))

# For cumulative frequency table, the order we cumulate categories matter
# For example I want to know how many observations belong to middle or low ses
# In this case, we can transform the data into factor class
ses.order <- ordered(ses, levels = c("low", "middle", "high"))

table(ses.order)
table(ses.order)/sum(table(ses.order))
cumsum(table(ses.order))

# Bar Chart
# The code for bar Chart is Barplot()
# Can we directly apply Barplot on character variable?
barplot(ses) # answer is no

# We should apply Barplot on the frequency table result
barplot(table(ses))

# We can make the plot fancier with more infomration
barplot(table(ses),main="Social-Economics Status",
        xlab="Status",
        ylab="Count",
        border="red",
        col="blue")

# Pie Chart
# Again, pie() can't be directly applied on character variable
# We apply pie() on the frequency table
pie(table(ses))

# Usually we prefer pie chart with more information
piepercent<- paste(round(100*table(ses)/sum(table(ses)), 1), "%")

pie(table(ses),labels= piepercent,main = "Social-Economic Status",col = rainbow(length(x)))
legend("topright", c("low","middle","high"),cex=0.8,fill = rainbow(length(x)))

# Quantitative Data
# Let's use a built-in data set in R: mtcars
# no package needed for the data set

# We can first check the details of the data set
?mtcars

# Frequency Distribution
Horsepower = mtcars$hp

# Let's find the minimum and maximum of Horsepower
range(Horsepower)

# Create classes
breaks = seq(50, 350, by=50)

# Cut original data set into classes
hp.cut = cut(Horsepower, breaks, right=FALSE)

# Frequency distribution
table(hp.cut)

# Relative Frequency distribution
table(hp.cut)/sum(table(hp.cut))

# Cumulative distribution
cumsum(table(hp.cut))

# Dot Chart
ggplot(mtcars, aes(x = hp)) +
  geom_dotplot()

# Histogram
hist(Horsepower)

# Two Variable Comparison
# Cross Tabulation
source("http://pcwww.liv.ac.uk/~william/R/crosstab.r")

# Let's create a survey data
# We set seed first otherwise your number will be different than mine
set.seed(123)
ID <- seq(1:177)
Age <- sample(c("0-15", "16-29", "30-44", "45-64", "65+"), 177, replace = TRUE)
Sex <- sample(c("Male", "Female"), 177, replace = TRUE)
Country <- sample(c("England", "Wales", "Scotland", "N. Ireland"), 177, replace = TRUE)
Health <- sample(c("Poor", "Average", "Good"), 177, replace = TRUE)
Survey <- data.frame(Age, Sex, Country, Health)
head(Survey)

# Cross Tabulation Frequency Count
crosstab(Survey, row.vars = "Age", col.vars = "Sex", type = "f")
# Cross Tabulation Frequency Row Percentages
crosstab(Survey, row.vars = "Age", col.vars = "Sex", type = "r")
# Cross Tabulation Frequency Column Percentages
crosstab(Survey, row.vars = "Age", col.vars = "Sex", type = "c")

# Scatter plot
# We import a data names wage1 for this example
plot(wage1$educ,wage1$wage)
abline(lm(wage1$wage~wage1$educ),col="red")

# Numerical 
# Measures of locations
# mean
mean(mtcars$hp)

# median
median(mtcars$hp)

# mode
find_mode <- function(x) {
  u <- unique(x)
  tab <- tabulate(match(x, u))
  u[tab == max(tab)]
}

find_mode(c(1,1,1,2,2,2))

# Percentile
quantile(mtcars$hp,0.25)

quantile(mtcars$hp,0.5)

quantile(mtcars$hp,0.75)

# Measures of variability
# Range
range(mtcars$hp)[2] - range(mtcars$hp)[1]

# Variance
var(mtcars$hp)

# Standard deviation
sqrt(var(mtcars$hp))

# Coefficient of variation
sqrt(var(mtcars$hp))/mean(mtcars$hp)

# Shape of Distribution

install.packages("moments")
library(moments)

skewness(mtcars$hp)

# Covariance
cov(mtcars$hp,mtcars$cyl)
cov(100*mtcars$hp,100*mtcars$cyl)

# Correlation
cor(mtcars$hp,mtcars$cyl)
