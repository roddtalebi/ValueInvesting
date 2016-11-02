### Document to play around with RF-QGA-SVR Value Investing Algorithm


### SET DIRECTORY
myDirectory <- "C:/Users/rtalebi3/Documents/ValueInvesting" #Windows
#myDirectory <- "/Users/Rodd/Desktop/ValueInvesting" #OSX
setwd(myDirectory)


### LOAD DATA
source("code/functions/quandl_load.R")
#file <- loadUnzip() #if the data needs to be downloaded
file <- "./data/SF1_20161031.csv"

source("code/functions/restructure.R")
originalDF <- restructure(file, rmNA==TRUE)


######
### NOTES

# need to train financial data from year 'x' to predict share, MV, of year (x+1)


### define years as such
# current year = 2016
# train data off years prior: < current year
# test it for current year ...2016


####################
### DEFINE MAIN FUNCTION
set_up <- function(currentYear, fullDF, topStocks=0){
  # get top 200 market value tickers for current year
  # MAYBE DON'T RESTRICT TO TOP STOCKS
  topMV <- fullDF[fullDF$Indicator=="MV" &
                    dullDF$Year==currentYear-1,
                  c(Ticker, Value) ][ order(-Value), 'Ticker ']
  
  if (topStocks != 0){ # to limit training off only top companies in last year
    topMV <- topMV[1:topStocks]
  }
  
  trainDF <- fullDF[(fullDF$Ticker %in% topMV &
                       fullDF$Year <= currentYear),]
  
  testDF <- fullDF[(fullDF$Ticker %in% topMV &
                      fullDF$Year == 1+currentYear),]
  
  return(list(trainDF, testDF, topMV[1:200]))
}




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

for (year in years[1:(length(years)-1)]){
  
  setupList <- set_up(year, df) #unlist() ?
  train <- setupList[[1]]
  test <- setupList[[2]]
  top200Tickers <- setupList[[3]]
  
  indicatorFilter <- model_RF(train, N=100)
  
  finalList <- model_svm(train, test, year, indicatorFilter=indicatorFilter)
  top10return <- ave(finalList[[1]])
  top20return <- ave(finalList[[2]])
  top30return <- ave(finalList[[3]])
  benchmarkReturn <- ave(finalList[[4]])
  
  resultsDF <- merge(resultsDF, c(year, top10return, top20return, top30return, benchmarkReturn))
}










