library(xts)
library(ggplot2)
library(forecast)
library(ggpubr)
library(tseries)
library(highfrequency)
library(rugarch)

#Loading the data
load("31.RData")

########################
### Data description ###
########################

#Summary
summary(amzn)
nrow(amzn) #1500 observations

#Stationarity of returns
adf.test(amzn$ret) #Stationary

#Plot
plot_names <- c("Returns", "Realized Volatility", "Pos. Realized Semi-volatility", "Neg. Realized Semi-volatility", "Realized Skewness", "Realized Kurtosis")
par(mfrow = c(3, 2))
for (i in 1:6) {
  print(plot(amzn[, i], main = plot_names[i]))
}

#Combined plots
dev.off()
#Returns and realized volatility
plot(amzn$ret, main = "Returns and Realized Volatility", grid.col = NA)
lines(amzn$RV, col = "red")
addLegend("topleft", on = 1, legend.names = c("Returns", "Realized Volatility"), col = c("black", "red"), lty = 1, bty = "n")
#Positive and negative realized semi-volatility
plot(amzn$RV_p, main = "Realized Semi-volatility", grid.col = NA)
lines(amzn$RV_n, col = "red")
addLegend("topright", on = 1, legend.names = c("Positive Realized Semi-volatility", "Negative Realized Semi-volatility"), col = c("black", "red"), lty = 1, bty = "n")

#ACF and PACF for returns
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

#ACF and PACF for Realized Volatility
acf_RV <- acf(amzn$RV, plot = F)
acf_RV_p <-  acf(amzn$RV_p, plot = F)
acf_RV_n <-  acf(amzn$RV_n, plot = F)
plot(acf_RV, type = "l", main = "ACF", ylab = "", lwd = 2)
lines(x = acf_RV_p$lag, y = acf_RV_p$acf, col = "red", lwd = 2)
lines(x = acf_RV_n$lag, y = acf_RV_n$acf, col = "blue", lwd = 2)
legend("topright", legend = c("Realized Volatility", "Positive Realized Semi-volatility", "Negative Realized Semi-volatility"), col = c("black", "red", "blue"), bty = "n", lty = 1, lwd = 2)

#ACF and PACF for Realized Skewness and Kurtosis
acf_RS <- acf(amzn$RS, plot = F)
acf_RK <-  acf(amzn$RK, plot = F)
plot(acf_RS, type = "l", main = "ACF", ylab = "", lwd = 2)
lines(x = acf_RK$lag, y = acf_RK$acf, col = "red", lwd = 2)
legend("topright", legend = c("Realized Skewness", "Realited Kurtosis"), col = c("black", "red"), bty = "n", lty = 1, lwd = 2)

#####################
### In-sample fit ###
#####################

### AR(1)-RV ###

amzn$RV_lag <- lag(amzn$RV) #Adding the first lag of RV to the data
ar1_rv <- lm(RV ~ RV_lag, data = amzn) #AR(1)-RV
summary(ar1_rv) #Lag very statistically significant
ar1_fitted <- xts(fitted.values(ar1_rv), order.by = as.Date(names(fitted.values(ar1_rv))))
plot(amzn$RV, main = "AR(1)-RV estimated volatility", grid.col = NA)
lines(ar1_fitted, col = "red") #Not the greatest fit
addLegend("topright", on = 1, legend.names = c("Realized Volatility", "AR(1)-RV fitted values"), col = c("black", "red"), lty = 1, bty = "n", lwd = c(2, 1))

### HAR ###

#Manual
calc_HAR_term <- function (x, len) { #Function for calculating HAR terms based on length
  result <- xts(rep(NA, length(x)), order.by = index(x)) #Empty xts object for the results
  for (i in len:length(x)) { #We have to skip the first "len - 1" observations
    result[i] <- mean(x[(i-len+1):i], na.rm = T) #For each obs calculate the mean of past "len - 1" observations
  }
  return(as.numeric(lag(result))) #Return a lag of the resulting series as a vector
}
amzn$RV_5 <- calc_HAR_term(amzn$RV, 5) #RV_(t-1)^5
amzn$RV_22 <- calc_HAR_term(amzn$RV, 22) #RV_(t-1)^22
har_manual <- lm(RV ~ RV_lag + RV_5 + RV_22, data = amzn)
summary(har_manual) #All significant

#Automatic
har_auto <- HARmodel(amzn$RV, periods = c(1, 5, 22))
summary(har_auto) #The same as manual
har_fitted <- xts(har_auto$fitted.values, order.by = as.Date(names(har_auto$fitted.values))) 
#Plotting fitted
plot(amzn$RV, main = "HAR estimated volatility", grid.col = NA)
lines(har_fitted, col = "red") #Not the greatest fit either
addLegend("topright", on = 1, legend.names = c("Realized Volatility", "HAR fitted values"), col = c("black", "red"), lty = 1, bty = "n", lwd = c(2, 1))

### HAR-RS (Realized Semi-volatility) ###

amzn$RV_p_lag <- lag(amzn$RV_p) #Lag of Realized Positive Semi-volatility
amzn$RV_n_lag <- lag(amzn$RV_n) #Lag of Realized Negative Semi-volatility
#Model
har_rs <- lm(RV ~ RV_p_lag + RV_n_lag + RV_5 + RV_22, data = amzn)
summary(har_rs) #All significant
har_rs_fitted <- xts(har_rs$fitted.values, order.by = as.Date(names(har_rs$fitted.values)))
#Plotting fitted
plot(amzn$RV, main = "HAR with Realized Semi-volatility estimated volatility", grid.col = NA)
lines(har_rs_fitted, col = "red") #Pretty decent but still not the greatest
addLegend("topright", on = 1, legend.names = c("Realized Volatility", "HAR Realized Semi-volatility fitted values"), col = c("black", "red"), lty = 1, bty = "n", lwd = c(2, 1))

### HAR with Realized Skewness and Kurtosis ###

amzn$RS_lag <- lag(amzn$RS) #Lag of Realized Skewness
amzn$RK_lag <- lag(amzn$RK) #Lag of Realized Kurtosis
#Model
har_skew_kurt <- lm(RV ~ RV_lag + RV_5 + RV_22 + RS_lag + RK_lag, data = amzn)
summary(har_skew_kurt) #Only skewness insignificant
har_skew_kurt_fitted <- xts(har_skew_kurt$fitted.values, order.by = as.Date(names(har_skew_kurt$fitted.values)))
#Plotting fitted
plot(amzn$RV, main = "HAR with Realized Skewness and Kurtosis estimated volatility", grid.col = NA)
lines(har_skew_kurt_fitted, col = "red") #Pretty decent, probably the best so far
addLegend("topright", on = 1, legend.names = c("Realized Volatility", "HAR with Realized Skewed and Kurtosis fitted values"), col = c("black", "red"), lty = 1, bty = "n", lwd = c(2, 1))

### Realized GARCH ###

#Mean model
auto.arima(amzn$ret, stationary = T, ic = "aic")
auto.arima(amzn$ret, stationary = T, ic = "bic")
#Trying ARMA(1,1)
arma11 <- Arima(amzn$ret, order = c(1, 0, 1))
par(mfrow = c(2, 1))
acf(arma11$residuals, main = "ACF", ylab = "") #No dependencies
pacf(arma11$residuals, main = "PACF", ylab = "") #No dependencies
#Ljung-Box test ARMA(1,1) residuals
for (lag_order in c(4, 8, 12)) {
  print(Box.test(amzn$ret, type = "Ljung-Box", lag = lag_order)$p.value) #Significant 4th and 8th lag
}
#Ljung-Box test returns
for (lag_order in c(4, 8, 12)) {
  print(Box.test(arma11$residuals, type = "Ljung-Box", lag = lag_order)$p.value) #No significant dependencies
}
#Realized GARCH
real_garchspec<- ugarchspec(variance.model = list(model = "realGARCH", garchOrder = c(1, 1)),
                            mean.model = list(armaOrder = c(1, 1)))
real_garch_fit<- ugarchfit(real_garchspec, amzn$ret, realizedVol = amzn$RV)  
real_garch_fit #alpha1 is exactly one, that is strange, spec tests ok
real_garch_fitted <- sigma(real_garch_fit)
#Plotting fitted
dev.off()
plot(amzn$RV, main = "Realized GARCH estimated volatility", grid.col = NA)
lines(real_garch_fitted, col = "red") #Seems very overestimated
addLegend("topright", on = 1, legend.names = c("Realized Volatility", "Realized GARCH fitted values"), col = c("black", "red"), lty = 1, bty = "n", lwd = c(2, 1))

### ARMA-GARCH ###

arma_garchspec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 2)), mean.model = list(armaOrder = c(1, 1)))
arma_garch_fit <- ugarchfit(arma_garchspec, amzn$ret)
arma_garch_fit #Everything significant, some dependency in standardized residuals for GARCH(1,1) => GARCH(1,2) better
arma_garch_fitted <- sigma(arma_garch_fit)
#Plotting fitted
plot(amzn$RV, main = "ARMA-GARCH estimated volatility", grid.col = NA)
lines(arma_garch_fitted, col = "red") #Seems extremely overestimated
addLegend("topright", on = 1, legend.names = c("Realized Volatility", "ARMA-GARCH fitted values"), col = c("black", "red"), lty = 1, bty = "n", lwd = c(2, 1))

### Comparison ###

plot(amzn$RV, main = "Comparison of estimated volatilities", grid.col = NA, lwd = 1)
lines(ar1_fitted, col = "orange")
lines(har_fitted, col = "blue")
lines(har_rs_fitted, col = "green")
lines(har_skew_kurt_fitted, col = "red")
lines(real_garch_fitted, col = "yellow")
lines(arma_garch_fitted, col = "grey")
addLegend("topright", on = 1, legend.names = c("Realized Volatility", "AR(1)-RV", "HAR", "HAR-RS", "HAR-Skew&Kurt", "Real GARCH", "ARMA-GARCH"), col = c("black", "orange", "blue", "green", "red", "yellow", "grey"), lty = 1, bty = "n", lwd = 1)

#################
### Forecasts ###
#################

### Forecasting lm type models (first four) ###
#Defining a function
forecast_lm <- function(form, xts_object = amzn, wind_len = 750, roll = F) { #Forecast based on a dataset, model formula, length of the window and either rolling or expanding
  result <- xts(rep(NA, wind_len), order.by = index(xts_object)[(wind_len + 1):nrow(xts_object)]) #Empty xts object for the results (predicting 751-1500)
  if (roll == T) { #Rolling window
    for (i in 1:wind_len) { #Looping through the rolled windows
      model <- lm(form, data = xts_object[i:(wind_len-1+i)]) #Estimating the model on the window (1-750, 2-751...)
      result[i] <- predict(model, newdata = xts_object[wind_len + i]) #Forecasting
    }
    } else { #Expanding window
    for (i in wind_len:(nrow(xts_object) - 1)) { #Looping through the windows (750-1499)
      model <- lm(form, data = xts_object[1:i]) #Estimating the model on the window
      result[i - wind_len + 1] <- predict(model, newdata = xts_object[i + 1]) #Forecasting
    }
    }
  return(result)
}

#Initiating a vector for storing the forecasts
forecasts <- vector("list", 6) #For 6 models

#Defining a name and formula for each model
model_names <- c("AR(1)-RV", "HAR", "HAR-RS", "HAR-Skew&Kurt", "Real GARCH", "ARMA-GARCH")
formulas <- c("RV ~ RV_lag", "RV ~ RV_lag + RV_5 + RV_22", "RV ~ RV_p_lag + RV_n_lag + RV_5 + RV_22",
              "RV ~ RV_lag + RV_5 + RV_22 + RS_lag + RK_lag")

#Looping through the first 4 models (lm type)
for (i in 1:4) {
  #For each model forecast with expanding window and rolling window and merge the resulting series together and store the result in a list
  forecasts[[i]] <-  merge.xts(forecast_lm(formulas[i]), forecast_lm(formulas[i], roll = T))
  names(forecasts[[i]]) <- paste(rep(model_names[i], 2), c("exp", "roll"), sep = "_") #Name the columns for clarity
}

### Forecasting GARCH type models ###

#Defining a function to forecast GARCH type models
forecast_garch <- function(specif, xts_object = amzn$ret, realizedVol = amzn$RV, wind_len = 750, roll = F) {
  result <- xts(rep(NA, wind_len), order.by = index(xts_object)[(wind_len + 1):nrow(xts_object)]) #Empty xts object for the results (predicting 751-1500)
  if (roll == T) {
    for (i in 1:wind_len) { #Looping through the rolled windows
      model <- ugarchfit(specif, xts_object[i:(wind_len-1+i)], realizedVol = realizedVol[i:(wind_len-1+i)]) #Fitting the model
      result[i] <- as.numeric(ugarchforecast(model, n.ahead = 1)@forecast$sigmaFor) #Producing the 1 step ahead forecast
    }
  } else { #Expanding window
    for (i in wind_len:(nrow(xts_object) - 1)) { #Looping through the windows (750-1499)
      model <- ugarchfit(specif, xts_object[1:i], realizedVol = realizedVol[1:i]) #Estimating the model on the window
      result[i - wind_len + 1] <- as.numeric(ugarchforecast(model, n.ahead = 1)@forecast$sigmaFor) #Producing the 1 step ahead forecast
    }
  }
  return(result)
}

#Looping through the last two (GARCH type) models
specs <- list(real_garchspec, arma_garchspec) #Storing the specifications for looping
for (i in 1:2) {
  forecasts[[i + 4]] <- merge.xts(forecast_garch(specs[[i]]), forecast_garch(specs[[i]], roll = T))
  names(forecasts[[i + 4]]) <- paste(rep(model_names[i + 4], 2), c("exp", "roll"), sep = "_") #Name the columns for clarity
}


x<-forecast_garch(real_garchspec)



########################################################################################################################################

### OLD CODE ###

### AR(1)-RV ###
#Expanding window
ar1_RV_forec_exp <- xts(rep(NA, wind_len), order.by = index(amzn)[(wind_len + 1):nrow(amzn)]) #Empty xts object for the results (predicting 751-1500)
for (i in wind_len:(nrow(amzn) - 1)) { #Looping through the windows (750-1499)
  ar1_RV_model <- lm(RV ~ RV_lag, data = amzn[1:i]) #Estimating the model on the window
  ar1_RV_forec_exp[i - wind_len + 1] <- predict(ar1_RV_model, newdata = amzn[i + 1]) #Forecasting
}
#Rolling window
ar1_RV_forec_roll <- xts(rep(NA, wind_len), order.by = index(amzn)[(wind_len + 1):nrow(amzn)]) #Empty xts object for the results (predicting 751-1500)
for (i in 1:wind_len) { #Looping through the rolled windows
  ar1_RV_model <- lm(RV ~ RV_lag, data = amzn[i:(wind_len-1+i)]) #Estimating the model on the window (1-750, 2-751...)
  ar1_RV_forec_roll[i] <- predict(ar1_RV_model, newdata = amzn[wind_len + i]) #Forecasting
}
#Forecast error
plot(amzn$RV[751:1500] - ar1_RV_forec_exp)
lines(amzn$RV[751:1500] - ar1_RV_forec_roll, col =  "red")

### HAR ###
#Expanding window
har_forec_exp <- xts(rep(NA, wind_len), order.by = index(amzn)[(wind_len + 1):nrow(amzn)]) #Empty xts object for the results
for (i in wind_len:(nrow(amzn) - 1)) { #Looping through the windows (750-1499)
  har_model <- lm(RV ~ RV_lag + RV_5 + RV_22, data = amzn[1:i]) #Estimating the model on the window
  har_forec_exp[i - wind_len + 1] <- predict(har_model, newdata = amzn[i + 1]) #Forecasting
}
#Rolling window
har_forec_roll <- xts(rep(NA, wind_len), order.by = index(amzn)[(wind_len + 1):nrow(amzn)]) #Empty xts object for the results (predicting 751-1500)
for (i in 1:wind_len) { #Looping through the rolled windows
  har_model <- lm(RV ~ RV_lag + RV_5 + RV_22, data = amzn[i:(wind_len-1+i)]) #Estimating the model on the window
  har_forec_roll[i] <- predict(har_model, newdata = amzn[wind_len + i]) #Forecasting
}
#Forecast error
plot(amzn$RV[751:1500] - har_forec_exp)
lines(amzn$RV[751:1500] - har_forec_roll, col =  "red")


#Defining a function to choose an appropriate ARMA order of a given series
find_ARMA_order <- function(xts_object) {
  auto_model <- auto.arima(xts_object, stationary = T, ic = "aic") #Minimize AIC (high no of obs => BIC penalizes too much)
  return(auto_model$arma[1:2]) #Return the order
}