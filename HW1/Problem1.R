###Financial Ecox 1 -- HW1

##Problem 1


#loading packages
#install.packages("quantmod")
library("quantmod")

#install.packages('EnvStats') 
library(EnvStats) #used for computing excess kurtosis

#install.packages("StableEstim")
library("StableEstim")

##Data Preparation
#loading data
tickers <- read.csv("symbols.csv", sep=";")

#summary and renaming
summary(tickers)
names(tickers) <- c("Symbol", "Name", "Sector")

###
#examining #n of tickers for each sector
for (i in unique(tickers$Sector)) {
  print(i)
  print(nrow(subset(tickers, tickers$Sector == i)))
}
###


#choosing sector at random
set.seed(1535)
sector <- sample(unique(tickers$Sector), 1)
print(sector)
tickers_sub <- subset(tickers, tickers$Sector == sector)
summary(tickers_sub)

#setting the start and end dates
date_start = as.Date('2015-01-01')
date_end = as.Date('2022-01-01') #setting the end date one day further, to include 2021-31-12 in getSymbols



############    ---- OLD CODE

#creating list prices, which contains data for each ticker
prices <- vector("list", length(tickers_sub$Symbol))
#prices[[1]]
data_available <- vector("logical", length(tickers_sub$Symbol))

#setting starting and ending dates
date_start <- as.Date('2015-01-01')
date_end <- as.Date('2022-01-01')
#downloading data
for (i in 1:length(tickers_sub$Symbol)) {
  tryCatch({
    prices[[i]] <- getSymbols(tickers_sub$Symbol[i], src = 'yahoo', auto.assign = FALSE, , from = date_start, to = date_end)
    message(paste("Dwonloaded data for ",  tickers_sub$Symbol[i]), ". Progress: ", i, " out of ", length(tickers_sub$Symbol), sep = "")
    data_available[i] <- TRUE
    
    
  }, error=function(e) {
    message(paste("Data unavailiable for ticker ", i, ": ", tickers_sub$Symbol[i], ". Null is assigned instead.", sep = ""))
    message(paste("Original error message:", e))
                       })
  }
print(paste("Total unavailable tickers: ", length(tickers_sub$Symbol)-sum(data_available), " out of: ", length(tickers_sub$Symbol), sep = ""))


#####################################

##---- NEW  CODE


##Data download as a function

data_preparation <- function(tickers,
                          date_start = as.Date('2015-01-01'),
                          date_end = as.Date('2022-01-01'),
                          closing_col = 4) { ###<--- Here the Closing price is assumed to have 4th position
  
  prices <- vector("list", length(tickers$Symbol))
  data_available <- vector("logical", length(tickers$Symbol))
  data_usable <- vector("logical", length(tickers$Symbol))
  
  for (i in 1:length(tickers$Symbol)) {
    tryCatch({
      prices[[i]] <- getSymbols(tickers$Symbol[i], src = 'yahoo', auto.assign = FALSE, , from = date_start, to = date_end)
      message(paste("Dwonloaded data for ",  tickers$Symbol[i]), ". Progress: ", i, " out of ", length(tickers$Symbol), sep = "")
      data_available[i] <- TRUE #logical vector only for the purpose of message printing
      }, error=function(e) {
      message(paste("Data unavailiable for ticker ", i, ": ", tickers$Symbol[i], sep = ""))
      message(paste("Original error message:", e))
    })
  }
  
  sample_size <- max(sapply(prices, function(x) {sum(!is.na(x[,closing_col]))})) 
  
  #creating logical vector indicating which data are complete enough to be used further
  data_usable <- sapply(prices, function(x) {sum(!is.na(x[,closing_col])) >= sample_size*0.8})
  tickers_used <- subset(tickers, data_usable)
  
  #saving only the closing prices of the usable data
  prices_used <- vector("list", sum(data_usable))
  iter_helper <- 1
  for (i in 1:length(data_usable)) {
    if (data_usable[i]) {
      prices_used[[iter_helper]] <- prices[[i]][,closing_col]
      iter_helper = iter_helper + 1
    }
  }
  
  return(list(closing_prices = prices_used, tickers = tickers_used))
}


#downloading and summarizing data

data_downloaded <- data_preparation(tickers_sub, date_start = date_start, date_end = date_end)
prices_clr_closing <- data_downloaded$closing_prices
tickers_clr <- data_downloaded$tickers


for (i in 1:length(prices_clr_closing)) {
  print(summary(prices_clr_closing[[i]]))
}


for (i in 1:length(prices_clr_closing)) {
  print(sum(is.na(prices_clr_closing[[i]])))
}


################
# Code below not used

#creating list prices_clear, which will contain data on tickers, which are not NULL
prices_clr <- vector("list", sum(data_available))
iter <- 1
for (i in 1:length(tickers_sub$Symbol)) {
  if (!is.null(prices[[i]][1])) {
    prices_clr[[iter]] <- prices[[i]]
    iter = iter + 1
  }
}
#prices_clr is the list we will work with further

summary(prices_clr)

#printing summary for each ticker
for (i in 1:length(prices_clr)) {
  print(paste("Ticker", i))
  print(summary(prices_clr[[i]]))
}

#getting the remaining tickers as string
tickers_clr <- tickers_sub$Symbol[data_available]

#check that we have the desired data
data_check <- vector("logical", length(tickers_clr))
for (i in 1:length(prices_clr)) {
  data_check[i] <- names(prices_clr[[i]])[1] == paste(tickers_clr[i], ".Open", sep = "") ####<---- here it might differ if the columns are ordered differently!
}
sum(data_check)/length(prices_clr) #everything OK if equal to 1, check done

#getting only the closing prices
prices_clr_closing <- vector("list", length(prices_clr))
for (i in 1:length(prices_clr)) {
  prices_clr_closing[[i]] <- prices_clr[[i]][, paste(tickers_clr[i], ".Close", sep="")]
}

summary(prices_clr_closing[[1]])

##  Not used up to here
##################################################################

##A)
#creating returns and log-returns: rets and lrets
rets <- lapply(prices_clr_closing, function(y){
  y <- na.omit(diff(y))  
  colnames(y) <- 'return'
  return(y)
})
names(rets) <- tickers_clr$Symbol



lrets <- lapply(prices_clr_closing, function(y){
  y <- na.omit(diff(log(y)))  
  colnames(y) <- 'log_return'
  return(y)
})
names(lrets) <- tickers_clr$Symbol

summary(lrets[[5]])

##B)
#Creating matrix summarizing the following statistic measures for log-returns
#mean, variance, skewness, excess kurtosis, minimum and maximum

#creating matrix
statistics_mat <- matrix(data = NA, nrow = length(tickers_clr$Symbol), ncol = 6)
#specifying names of rows and columns
row.names(statistics_mat) <- tickers_clr$Symbol
colnames(statistics_mat) <- c("mean", "variance", "skewness", "excess_kurtosis", "minimum", "maximum")

#filling the matrix
for (i in 1:nrow(statistics_mat)) {
  statistics_mat[i, 1] <- mean(as.numeric(lrets[[i]]$log_return))
  statistics_mat[i, 2] <- var(as.numeric(lrets[[i]]$log_return))
  statistics_mat[i, 3] <- skewness(as.numeric(lrets[[i]]$log_return))
  statistics_mat[i, 4] <- kurtosis(as.numeric(lrets[[i]]$log_return, excess = TRUE))
  statistics_mat[i, 5] <- min(as.numeric(lrets[[i]]$log_return))
  statistics_mat[i, 6] <- max(as.numeric(lrets[[i]]$log_return))
}

statistics_mat
View(statistics_mat)


##C)
#plotting all log-returns in one figure

plot.xts(lrets[[1]]$log_return, col = "black", grid.col = "white", lwd = 1 ,ylim = c(min(statistics_mat[,5]), max(statistics_mat[,6])) , ylab = 'log-returns', main = paste("Log-returns in ", sector, " sector", sep = ""))
for (i in 2:length(lrets)) {
  print(lines(lrets[[i]]$log_return, col = colors()[i+100]))
}

#########TESTING

lines_list <- vector("list", length(lrets)-1)

for (i in 2:length(lrets)) {
  lines_list <- (lines(lrets[[i]]$log_return, col = colors()[i+100]))
}

lines_list

##########




##D)
#computing means of log returns for each date, where we have observation for all tickers

#creating empty dataframe to enter the log-ret means
lrets_means <- data.frame(date = seq.Date(from = date_start, to = date_end - 1, by = 1), lret_mean = rep(NA, as.numeric(date_end-date_start)))

#the following for loop goes through the sequence of dates of our interest, checks that we have observation for log-returns of each of the tickers and saves the mean of those log-returns
for (i in 1:nrow(lrets_means)) {
  iter_date <- lrets_means$date[i]
  date_lrets <- rep(NA, length(lrets))
  for (j in 1:length(lrets)) {
    
    if (nrow(lrets[[j]][iter_date,]) == 1) {
      date_lrets[j] <-  as.numeric(lrets[[j]][iter_date,])
    }
    
  }
  print(paste("Iteration", i, "out of", nrow(lrets_means),  "Date:", iter_date))
  #print(date_lrets)
  
  
  if (sum(is.na(date_lrets)) == 0) {
    lrets_means$lret_mean[i] <- mean(date_lrets)
  }
}


summary(lrets_means)
plot(x=lrets_means$date, y=lrets_means$lret_mean, type = "l")

##E)
#Estimating parameters of stable distribution based on mean log returns per day for all the tickers
lrets_means_sub <- subset(lrets_means, !is.na(lrets_means$lret_mean))

stable_dist_estim <- Estim(EstimMethod = "Kout", data = lrets_means_sub$lret_mean)
stable_dist_estim


##F)
#plotting histogram of the mean log-returns
hist(lrets_means_sub$lret_mean, 
     nclass = 200,
     freq = FALSE,
     border = "white",
     col = "darkgrey",
     xlab = "Log-Returns",
     main = paste("Histogram of Log-Returns for ", sector, " sector", sep = ""))
#defining features of the normal distribution
density_seq <- seq(from = min(lrets_means_sub$lret_mean),
                to = max(lrets_means_sub$lret_mean),
                by = 0.00001)
norm_m <- mean(lrets_means_sub$lret_mean)
norm_std <- sd(lrets_means_sub$lret_mean)

#adding a line representing the normal distribution
lines(x = density_seq, 
      y = dnorm(density_seq, 
                mean = norm_m, 
                sd = norm_std),
      col = "blue")

#adding the line representing stable distribution, using the parameters estimated based on the mean log-returns
lines(x = density_seq, 
      y = dstable(density_seq, 
                  alpha = stable_dist_estim@par[1],
                  beta = stable_dist_estim@par[2],
                  gamma = stable_dist_estim@par[3],
                  delta = stable_dist_estim@par[4]),
      col = "red")

