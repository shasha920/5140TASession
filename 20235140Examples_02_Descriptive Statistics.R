#find your file path--setwd('/Users/')
# install.packages("e1071")
library(e1071)

# Example 2-1 ====
softdrink.df <- read.csv("Data_02_SoftDrink.csv")
# |---Frequency distribution ====
softdrink.freq <- data.frame(table(softdrink.df))
names(softdrink.freq)[1] <- "Soft.Drink"
softdrink.freq
# |---Relative and percent frequency distribution ====
softdrink.freq$Rel.Freq <- softdrink.freq$Freq / sum(softdrink.freq$Freq)
softdrink.freq$Pct.Freq <- 100.00 * softdrink.freq$Rel.Freq
softdrink.freq
# |---Bar chart ====
barplot(softdrink.freq$Freq, names.arg = softdrink.freq$Soft.Drink,
        xlab = "Soft Drink", ylab = "Frequency", main = "Bar Chart of Soft Drink Purchases")
# |---Pie chart ====
pie(softdrink.freq$Freq, main = "Pie Chart of Soft Drink Purchases",
    labels = paste(softdrink.freq$Soft.Drink, " (", softdrink.freq$Pct.Freq, "%)", sep = ""))

# Example 2-2 ====
audit.df <- read.csv("Data_02_Audit.csv")
#  |---Histogram ====
#  Appropriate number of classes is
#  determined by either Sturges' rule,
#  Scott's rule, or Freedman-Diaconis's rule
#  although the breakpoints are adjusted
#  to be "pretty"
hist(audit.df$Audit.Time, breaks = "FD", xlab = "Audit Time",
     main = "Histogram of Audit Time")
