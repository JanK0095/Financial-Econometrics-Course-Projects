library(xts)
library(ggplot2)
library(forecast)
library(ggpubr)
library(tseries)
library(highfrequency)

#Loading the data
load("31.RData")

########################
### Data description ###
########################

#Summary
summary(amzn)
nrow(amzn) #1500 observations

#Plot
plot_names <- c("Returns", "Realized Volatility", "Pos. Realized Semi-volatility", "Neg. Realized Semi-volatility", "Realized Skewness", "Realized Kurtosis")
par(mfrow = c(3, 2))
for (i in 1:6) {
  print(plot(amzn[, i], main = plot_names[i]))
}

#Stationarity
adf.test(amzn$ret) #Stationary

#ACF and PACF
acf_ret <- ggAcf(amzn$ret)+
  theme_minimal()+
  ggtitle("ACF")+
  ylab("")+
  xlab("")
pacf_ret <- ggPacf(amzn$ret)+
  theme_minimal()+
  ggtitle("PACF")+
  ylab("")+
  xlab("")
ggarrange(acf_ret, pacf_ret, ncol = 1, nrow = 2) #Some very small dependencies

#####################
### In-sample fit ###
#####################

### AR(1)-RV ###
ar1_rv <- lm(as.numeric(amzn$RV[2:1500]) ~ as.numeric(amzn$RV[1:1499]))
summary(ar1_rv)
dev.off()
plot(xts(fitted.values(ar1_rv), order.by = index(amzn)[2:1500]), main = "AR(1)-RV estimated volatility")
lines(amzn$RV, col = "red")

### HAR ###
har <- HARmodel(amzn$RV, periods = c(1, 5, 22), RVest = c("rCov"),
                type = "HAR", h = 1, transform = NULL, inputType = "RM")
har
plot(xts(har$fitted.values, order.by = as.Date(names(har$fitted.values))))
lines(amzn$RV, col = "red")


