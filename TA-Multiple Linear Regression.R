# install.packages("car")
library(car)

# Penalize R for using scietific way to display number
options(scipen = 999)

# Example 10-1 ====
butler.df <- read.csv("Data_Butler.csv")
butler.lm <- lm(Time ~ Miles + Deliveries, data = butler.df)
butler.lm.summary <- summary(butler.lm)
print(butler.lm.summary)
butler.df.new <- data.frame(Miles = c(85, 65), Deliveries = c(3, 5))
predict(butler.lm, newdata = butler.df.new)
predict(butler.lm, newdata = butler.df.new, interval = "confidence", level = 0.95)
predict(butler.lm, newdata = butler.df.new, interval = "prediction", level = 0.95)
#measures how much the variance of a regression coefficient is inflated 
#due to multicollinearity in the model
vif(butler.lm)
# Residual Analysis
butler.dv.est <- butler.lm$fitted.values
#rstudent function is used to calculate the studentized
#residuals in the context of linear regression models
butler.res.std <- rstudent(butler.lm)
lb <- min(-3, -max(abs(butler.res.std)))
ub <- max(3, max(abs(butler.res.std)))
plot(butler.res.std ~ butler.dv.est, pch = 19, xlab = "Fitted Value of DV",
     ylab = "Standardized Residual", main = "Residual Analysis", ylim = c(lb, ub))
abline(h = -1.5, lty = 3)
abline(h = 1.5, lty = 3)

# Example 10-2 ====
johnson.df <- read.csv("Data_Johnson.csv")
class(johnson.df$Type.of.Repair)
levels(johnson.df$Type.of.Repair)
levels.order <- c("mechanical", "electrical")
johnson.df$Type.of.Repair <- factor(johnson.df$Type.of.Repair, levels.order)
levels(johnson.df$Type.of.Repair)
johnson.lm <- lm(Repair.Time ~ Months.Since.Last.Service + Type.of.Repair,
                 data = johnson.df)
johnson.lm.summary <- summary(johnson.lm)
print(johnson.lm.summary)
vif(johnson.lm)
# Residual Analysis
johnson.dv.est <- johnson.lm$fitted.values
johnson.res.std <- rstudent(johnson.lm)
lb <- min(-3, -max(abs(johnson.res.std)))
ub <- max(3, max(abs(johnson.res.std)))
plot(johnson.res.std ~ johnson.dv.est, pch = 19, xlab = "Fitted Value of DV",
     ylab = "Standardized Residual", main = "Residual Analysis", ylim = c(lb, ub))
abline(h = -1.5, lty = 3)
abline(h = 1.5, lty = 3)

