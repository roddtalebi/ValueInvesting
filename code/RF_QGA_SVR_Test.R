### Document to play around with RF-QGA-SVR Value Investing Algorithm


### SET DIRECTORY
myDirectory <- "C:/Users/rtalebi3/Documents/ValueInvesting" #Windows
#myDirectory <- "/Users/Rodd/Desktop/ValueInvesting" #OSX
setwd(myDirectory)


### LOAD DATA
source("code/functions/quandl_load.R")
#file <- loadUnzip() #if the data needs to be downloaded
file <- "./data/SF1_20161102.csv"

source("code/functions/restructure.R")
originalDF <- restructure(file, rmNA=TRUE)


######
### NOTES

# need to train financial data from year 'x' to predict share, MV, of year (x+1)


### define years as such
# current year = 2016
# train data off years prior: < current year
# test it for current year ...2016


####################
### DEFINE MAIN FUNCTION
set_up <- function(currentYear, df, topStocks=0){
  # get top 200 market value tickers for current year
  # MAYBE DON'T RESTRICT TO TOP STOCKS
  
  # "This paper considers financial chracteristics of listed companies as the input variables,
  # and annual return of stock as response variables."
  topMV <- df[df$Year==currentYear-1, c("Ticker", "MV")]
  topMV <- setorder(topMV, -MV)[,'Ticker']
  
  if (topStocks != 0){ # to limit training off only top companies in last year
    topMV <- topMV[1:topStocks]
  }
  
  train <- df[(df$Ticker %in% topMV &
                       df$Year < currentYear),]
  
  test <- df[(df$Ticker %in% topMV &
                      df$Year == currentYear),]
  
  # now rm MV
  ind <- which(names(train)=="MV")
  
  return(list(train[,-4], test[,-4], topMV))
}

list3 <- set_up(currentYear=2016, df=originalDF)
train <- list3[[1]]
test <- list3[[2]]
topMV <- list3[[3]]




### Call functions





#######
### RF-SVR
install.packages('randomForest')
library(randomForest)
source("code/functions/RF.R")

install.packages('e1071')
library(e1071)
source("code/functions/QGA.R")

resultsDF <- data.frame(Year=numeric(),
                        top10=numeric(),
                        top20=numeric(),
                        top30=numeric(),
                        benchmark=numeric())

years <- originalDF$Year[order(unique(originalDF$Year))]
maxYear <- max(years)

for (year in years[2:(length(years))]){
  
  setupList <- set_up(year, df) #unlist() ?
  train <- setupList[[1]]
  test <- setupList[[2]]
  topTickers <- setupList[[3]]
  
  indicatorFilter <- model_RF(train[,-"MV"], N=100)
  
  finalList <- model_svm(train, test, year, maxYear=maxYear, indicatorFilter=indicatorFilter)
  if (currentYear == maxYear){
    # then the user is expecting the predicted ranking
    top10 <- finalList[[1]]
    top20 <- finalList[[2]]
    top30 <- finalList[[3]]
  } else {
    top10return <- ave(finalList[[1]])
    top20return <- ave(finalList[[2]])
    top30return <- ave(finalList[[3]])
    benchmarkReturn <- ave(finalList[[4]])
    
    resultsDF <- merge(resultsDF, c(year, top10return, top20return, top30return, benchmarkReturn))
  }
}










