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
originalDF <- restructure(file)



####################
### REORGANIZE DATA
# df <-> ticker, indicator, year, value
df <- data.frame(Ticker=character(),
                 Indicator=character(),
                 Year=numeric(),
                 Value=numeric())

# get unique tickers (company ids)
tickers <- apply(oriData, 1, function(x) substr(x[1], 1, regexpr('_', x[1])-1))
tickers <- unique(tickers)

# for each ticker id, get the 15 or 16 indicators with year
for (ticker in tickers){
  new <- data.frame(Ticker=character(),
                    Indicator=character(),
                    Year=numeric(),
                    Value=numeric())
  
  # PRICE EARNING RATIO
  description <- paste(ticker, "_PE_MRY", sep="")
  subset <- oriData[oriData[,1]==description, c("Year", "Value")]
  new <- apply(subset, 1, function(x){
    roww <- c(ticker, "PE", x[1], x[2])
    return(roww)
  })
  merge(df,new)
  
  # PRICE/BOOK VALUE RATIO
  description <- paste(ticker, "_PB_MRY", sep="")
  subset <- oriData[oriData[,1]==description, c("Year", "Value")]
  new <- apply(subset, 1, function(x){
    roww <- c(ticker, "PB", x[1], x[2])
    return(roww)
  })
  merge(df,new)
  
  # PRICE TO SALE RATIO
  description <- paste(ticker, "_PS1_MRY", sep="")
  subset <- oriData[oriData[,1]==description, c("Year", "Value")]
  new <- apply(subset, 1, function(x){
    roww <- c(ticker, "PS", x[1], x[2])
    return(roww)
  })
  merge(df,new)
  
  # EARNINGS PER SHARE
  description <- paste(ticker, "_EPS_MRY", sep="")
  subset <- oriData[oriData[,1]==description, c("Year", "Value")]
  new <- apply(subset, 1, function(x){
    roww <- c(ticker, "EPS", x[1], x[2])
    return(roww)
  })
  merge(df,new)
  
  # RETURN ON EQUITY
  description <- paste(ticker, "_ROE_MRY", sep="")
  subset <- oriData[oriData[,1]==description, c("Year", "Value")]
  new <- apply(subset, 1, function(x){
    roww <- c(ticker, "ROE", x[1], x[2])
    return(roww)
  })
  merge(df,new)
  
  # RETURN ON ASSET
  description <- paste(ticker, "_ROA_MRY", sep="")
  subset <- oriData[oriData[,1]==description, c("Year", "Value")]
  new <- apply(subset, 1, function(x){
    roww <- c(ticker, "ROA", x[1], x[2])
    return(roww)
  })
  merge(df,new)
  
  # OPERATING PROFIT MARGIN
  description1 <- paste(ticker, "_OPINC_MRY", sep="")
  subset1 <- oriData[oriData[,1]==description1, c("Year", "Value")]
  description2 <- paste(ticker, "_REVENUE_MRY", sep="")
  subset2 <- oriData[oriData[,1]==description2, c("Year", "Value")]
  new <- apply(subset1, 1, function(x){
    val <- x[2] / subset2[subset2[1]==x[1],2]
    roww <- c(ticker, "OPM", x[1], val)
    return(roww)
  })
  merge(df,new)
  
  # NET PROFIT MARGIN
  description <- paste(ticker, "_NETMARGIN_MRY", sep="")
  subset <- oriData[oriData[,1]==description, c("Year", "Value")]
  new <- apply(subset, 1, function(x){
    roww <- c(ticker, "NPM", x[1], x[2])
    return(roww)
  })
  merge(df,new)
  
  # DEBT EQUITY RATIO
  description <- paste(ticker, "_DE_MRY", sep="")
  subset <- oriData[oriData[,1]==description, c("Year", "Value")]
  new <- apply(subset, 1, function(x){
    roww <- c(ticker, "DE", x[1], x[2])
    return(roww)
  })
  merge(df,new)
  
  # TIMES INTEREST EARNED
  description1 <- paste(ticker, "_EBITDA_MRY", sep="")
  subset1 <- oriData[oriData[,1]==description1, c("Year", "Value")]
  description2 <- paste(ticker, "_INTEXP_MRY", sep="")
  subset2 <- oriData[oriData[,1]==description2, c("Year", "Value")]
  new <- apply(subset1, 1, function(x){
    val <- x[2] / subset2[subset2[1]==x[1],2]
    roww <- c(ticker, "ICV", x[1], val)
    return(roww)
  })
  merge(df,new)
  
  # CURRENT RATIO
  description <- paste(ticker, "_CURRENTRATIO_MRY", sep="")
  subset <- oriData[oriData[,1]==description, c("Year", "Value")]
  new <- apply(subset, 1, function(x){
    roww <- c(ticker, "CR", x[1], x[2])
    return(roww)
  })
  merge(df,new)
  
  # QUICK RATIO
  description1 <- paste(ticker, "_ASSETSC_MRY", sep="")
  subset1 <- oriData[oriData[,1]==description1, c("Year", "Value")]
  description2 <- paste(ticker, "_INVENTORY_MRY", sep="")
  subset2 <- oriData[oriData[,1]==description2, c("Year", "Value")]
  description3 <- paste(ticker, "_LIABILITIESC_MRY", sep="")
  subset3 <- oriData[oriData[,1]==description3, c("Year", "Value")]
  new <- apply(subset1, 1, function(x){
    val <- ( x[2] - subset2[subset2[1]==x[1],2] ) / subset3[subset3[1]==x[1],2]
    roww <- c(ticker, "QR", x[1], val)
    return(roww)
  })
  merge(df,new)
  
  # INVENTORY TURNOVER RATIO
  description1 <- paste(ticker, "_COR_MRY", sep="")
  subset1 <- oriData[oriData[,1]==description1, c("Year", "Value")]
  description2 <- paste(ticker, "_INVENTORY_MRY", sep="")
  subset2 <- oriData[oriData[,1]==description2, c("Year", "Value")]
  new <- apply(subset1, 1, function(x){
    val <- x[2] / subset2[subset2[1]==x[1],2]
    roww <- c(ticker, "ITR", x[1], val)
    return(roww)
  })
  merge(df,new)
  
  # ACCOUNTS RECEIVABLE TURNOVER RATIO
  description1 <- paste(ticker, "_OPINC_MRY", sep="")
  subset1 <- oriData[oriData[,1]==description1, c("Year", "Value")]
  description2 <- paste(ticker, "_RECEIVABLES_MRY", sep="")
  subset2 <- oriData[oriData[,1]==description2, c("Year", "Value")]
  new <- apply(subset1, 1, function(x){
    val <- x[2] / subset2[subset2[1]==x[1],2]
    roww <- c(ticker, "RTR", x[1], val)
    return(roww)
  })
  merge(df,new)
  
  # INCREASE RATE OF BUSINESS REVENUE
  description <- paste(ticker, "_REVENUEGROWTH1YR_MRY", sep="")
  subset <- oriData[oriData[,1]==description, c("Year", "Value")]
  new <- apply(subset, 1, function(x){
    roww <- c(ticker, "OIG", x[1], x[2])
    return(roww)
  })
  merge(df,new)
  
  # NET PROFIT GROWTH RATE
  #???
  
  # MARKET VALUE
  description1 <- paste(ticker, "_SHARESBAS_MRY", sep="")
  subset1 <- oriData[oriData[,1]==description1, c("Year", "Value")]
  description2 <- paste(ticker, "_PRICE_MRY", sep="")
  subset2 <- oriData[oriData[,1]==description2, c("Year", "Value")]
  new <- apply(subset1, 1, function(x){
    val <- x[2] * subset2[subset2[1]==x[1],2]
    roww <- c(ticker, "MV", x[1], val)
    return(roww)
  })
  merge(df,new)
}



####################
### ALTERNATIVE--REORGANIZE DATA
# df <-> ticker, year, market value, indicator 1-16
df <- data.frame(Ticker=character(),
                 Year=numeric())

# get unique tickers (company ids)
tickers <- apply(oriData, 1, function(x) substr(x[1], 1, regexpr('_', x[1])-1))
tickers <- unique(tickers)

# get year spread
#firstYear <- min(oriData$Year)
#lastYear <- max(oriData$Year)
years <- sort(unique(oriData$Year))
minYear <- min(years)
maxYear <- max(years)

# fill df with tickers and year
df <- data.frame(Ticker=tickers,
                  Year=rep(years,length(tickers)))

#make sure duplicates weren't made
dim(df) == dim(unique(df))

# create and fill rest of dataframe
df[,c("NextYearMV","PE","PB","PS","EPS","ROE","ROA","OPM","NPM","DE","ICV","CR","QR","ITR","RTR","OIG","NIG")] <-NA


### FILTER DATA FIRST


# fill that ish
df <- sapply(oriData[oriData$Quarter=="Q4",], 1, function(x){
  ind <- gregexpr('_', x[1])[[1]][1:2]
  ticker <- substr(x[1], 1, ind[1]-1)
  year <- x[4]
  indicator <- substr(x[1], ind[1]+1, ind[2]-1)
  
  if (indicator=="PE"){
    df[df$Ticker==ticker & df$Year==year, 'PE'] <- x[3]
  } else if (indicator=="PB"){
    df[df$Ticker==ticker & df$Year==year, 'PB'] <- x[3]
  } else if (indicator=="PS1"){
    df[df$Ticker==ticker & df$Year==year, 'PS'] <- x[3]
  } else if (indicator=="EPS"){
    df[df$Ticker==ticker & df$Year==year, 'EPS'] <- x[3]
  } else if (indicator=="ROE"){
    df[df$Ticker==ticker & df$Year==year, 'ROE'] <- x[3]
  } else if (indicator=="ROA"){
    df[df$Ticker==ticker & df$Year==year, 'ROA'] <- x[3]
  } else if (indicator=="OPINC"){
    description <- paste(ticker, "_REVENUE_MRY", sep="")
    val <- oriData[oriData$Description==description & oriData$Year==year, "Value"]
    df[df$Ticker==ticker & df$Year==year, 'OPM'] <- x[3] / val
    description <- paste(ticker, "_RECEIVABLES_MRY", sep="")
    val <- oriData[oriData$Description==description & oriData$Year==year, "Value"]
    df[df$Ticker==ticker & df$Year==year, 'RTR'] <- x[3] / val
  } else if (indicator=="NETMARGIN"){
    df[df$Ticker==ticker & df$Year==year, 'NPM'] <- x[3]
  } else if (indicator=="EBITDA"){
    description <- paste(ticker, "_INTEXP_MRY", sep="")
    val <- oriData[oriData$Description==description & oriData$Year==year, "Value"]
    df[df$Ticker==ticker & df$Year==year, 'ICV'] <- x[3] / val
  } else if (indicator=="CURRENTRATIO"){
    df[df$Ticker==ticker & df$Year==year, 'CR'] <- x[3]
  } else if (indicator=="ASSETSC"){
    description1 <- paste(ticker, "_INVENTORY_MRY", sep="")
    val1 <- oriData[oriData$Description==description1 & oriData$Year==year, "Value"]
    description2 <- paste(ticker, "_LIABILITIESC_MRY", sep="")
    val2 <- oriData[oriData$Description==description2 & oriData$Year==year, "Value"]
    df[df$Ticker==ticker & df$Year==year, 'QR'] <- (x[3] - val1) / val2
  } else if (indicator=="COR"){
    description <- paste(ticker, "_INVENTORY_MRY", sep="")
    val <- oriData[oriData$Description==description & oriData$Year==year, "Value"]
    df[df$Ticker==ticker & df$Year==year, 'ITR'] <- x[3] / val
  } else if (indicator=="REVENUEGROWTH1YR"){
    df[df$Ticker==ticker & df$Year==year, 'OIG'] <- x[3]
  } else if (indicator=="SHARESBAS"){
    # need to train financial data of year 'x' to predict MV for year 'x+1'
    # so MV data from year 'y' matches with financial data from year 'y-1'
    if (year-1 >= minYear){
      description <- paste(ticker, "_PRICE_MRY", sep="")
      val <- oriData[oriData$Description==description & oriData$Year==year, "Value"]
      df[df$Ticker==ticker & df$Year==(year-1), 'nextYearMV'] <- x[3] * val
    }
  } # NIG????
  
})


######
### NOTES

# need to train financial data from year 'x' to predict market value, MV, of year (x+1)



####################
### DEFINE MAIN FUNCTION
set_up <- function(currentYear, fullDF){
  # get top 200 market value tickers for current year
  # MAYBE DON'T RESTRICT TO TOP STOCKS
  topMV <- fullDF[fullDF$Indicator=="MV",c(Ticker, Value)][order(-Value),]
  top200 <- topMV[1:200,'Ticker']
  
  trainDF <- fullDF[(fullDF$Ticker %in% top200 &
                       fullDF$Year <= currentYear),]
  
  testDF <- fullDF[(fullDF$Ticker %in% top200 &
                      fullDF$Year == 1+currentYear),]
  
  return(list(trainDF, testDF, top200))
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










