### Restructure DF

# current dataframe df:
# Description
# Date
# Value
# Year
# Quarter
# Ticker
# Indicator
# Dimension
# Reported
# TimeDimension

tickers <- unique(test$Ticker)
years <- sort(unique(test$Year))

df <- data.frame(Ticker=tickers,
                 Year=rep(years,length(tickers)))
df[,c("NextYearMV","PE","PB","PS","EPS","ROE","ROA","OPINC","REVENUE","OPM","NPM","DE","EBITDA","INTEXP","ICV","CR","ASSETSC","INVENTORY","LIABILITIESC",
      "QR","COR","ITR","RECEIVABLES","RTR","OIG","NETINC","NIG","Date")] <-NA

wantList <- c("PE","PB","PS1","EPS","ROE","ROA","OPINC","REVENUE","NETMARGIN","DE","EBITDA","INTEXP","CURRENTRATIO","ASSETSC","INVENTORY","LIABILITIESC",
              "COR","RECEIVABLES","REVENUEGROWTH1YR","NETINC")



indTicker <- which(names(test)=="Ticker")
indIndicator <- which(names(test)=="Indicator")
indYear <- which(names(test)=="Year")
indDate <- which(names(test)=="Date")

new<-apply(test[test$Reported=="MR" &
           test$Indicator %in% wantList,
           c("Ticker", "Indicator", "Year", "TimeDimension", "Value")],
      1, function(x){
        if (x[2]=="PE"){
          DF[DF$Ticker==x[1] & DF$Year==x[3], 'PE'] <- x[5]
          print(DF[DF$Ticker==x[1] & DF$Year==x[3],])
        } else if (x[2]=='PB'){
          DF[DF$Ticker==x[1] & DF$Year==x[3], 'PB'] <- x[5]
        } else if (x[2]=='PS1'){
          DF[DF$Ticker==x[1] & DF$Year==x[3], 'PS'] <- x[5]
        } else if (x[2]=='EPS'){
          DF[DF$Ticker==x[1] & DF$Year==x[3], 'EPS'] <- x[5]
        } else if (x[2]=='ROE'){
          DF[DF$Ticker==x[1] & DF$Year==x[3], 'ROE'] <- x[5]
        } else if (x[2]=='ROA'){
          DF[DF$Ticker==x[1] & DF$Year==x[3], 'ROA'] <- x[5]
        } else if (x[2]=='OPINC'){
          DF[DF$Ticker==x[1] & DF$Year==x[3], 'OPINC'] <- x[5]
        } else if (x[2]=='REVENUE'){
          DF[DF$Ticker==x[1] & DF$Year==x[3], 'REVENUE'] <- x[5]
        } else if (x[2]=='NETMARGIN'){
          DF[DF$Ticker==x[1] & DF$Year==x[3], 'NETMARGIN'] <- x[5]
        } else if (x[2]=='DE'){
          DF[DF$Ticker==x[1] & DF$Year==x[3], 'DE'] <- x[5]
        } else if (x[2]=='EBITDA'){
          DF[DF$Ticker==x[1] & DF$Year==x[3], 'EBITDA'] <- x[5]
        } else if (x[2]=='INTEXP'){
          DF[DF$Ticker==x[1] & DF$Year==x[3], 'INTEXP'] <- x[5]
        } else if (x[2]=='CURRENTRATIO'){
          DF[DF$Ticker==x[1] & DF$Year==x[3], 'CR'] <- x[5]
        } else if (x[2]=='ASSETSC'){
          DF[DF$Ticker==x[1] & DF$Year==x[3], 'ASSETSC'] <- x[5]
          print("hi1")
          print(x)
          print(typeof(x[1]))
          print(typeof(x[3]))
          print("-")
        } else if (x[2]=='INVENTORY'){
          DF[DF$Ticker==x[1] & DF$Year==x[3], 'INVENTORY'] <- x[5]
        } else if (x[2]=='LIABILITIESC'){
          DF[DF$Ticker==x[1] & DF$Year==x[3], 'LIABILITIESC'] <- x[5]
        } else if (x[2]=='COR'){
          DF[DF$Ticker==x[1] & DF$Year==x[3], 'COR'] <- x[5]
        } else if (x[2]=='RECEIVABLES'){
          DF[DF$Ticker==x[1] & DF$Year==x[3], 'RECEIVABLES'] <- x[5]
        } else if (x[2]=='REVENUEGROWTH1YR'){
          DF[DF$Ticker==x[1] & DF$Year==x[3], 'OIG'] <- x[5]
        } else if (x[2]=='NETINC'){
          DF[DF$Ticker==x[1] & DF$Year==x[3], 'NETINC'] <- x[5]
        }
        
        if (x[4] != "Y"){print(x)}
        
      })

# testing
df <- data.frame(cbind(1:10, 2:11, 1:3))
colnames(df) <- c("ID", "DATE", "SECTOR")
df <- data.frame(df, value=TRUE)

a<-reshape(df, idvar=c("ID","DATE"),timevar="SECTOR",direction="wide")


install.packages("reshape")
library(reshape)

a <- cast(test, Ticker + Year ~ Indicator, value="Value")
a <- cast(test[test$Dimension=="MRY",], Ticker + Year ~ Indicator, value="Value")






### Restructure
restructure <- function(file){
  
  # Load packages
  install.packages("data.table")
  install.packages("fasttime")
  install.packages("stringr")
  install.packages("reshape")
  library(data.table) #for loading csv file --> fread()
  library(fasttime) #for date type conversion --> fastPOSIXct()
  library(stringr) #for deliminated columns --> str_split_fixed()
  library(reshape) #for building a new data frame --> cast()
  
  # Load Data from file
  oriData <- as.data.frame(fread(file, header=FALSE, col.names=c("Description","Date","Value"))) #load csv as data frame
  oriData$Date <- fastPOSIXct(oriData$Date) #convert to Date type
  oriData$Value <- as.numeric(oriData$Value) #convert to numeric
  Sys.sleep(5)
  gc() # to clean up
  
  oriData <- oriData[year(oriData$Date)>=2012,] #trim data frame
  # goes from 78.5 million rows to
  # 42,131,192 (>=2010)
  # 29,533,302 (>=2012)
  
  
  oriData$Year <- year(oriData$Date) #get a year column...yyyy
  Sys.sleep(5)
  gc()
  
  oriData$Quarter <- quarter(oriData$Date) #get quarter column...Q#
  Sys.sleep(5)
  gc()
  
  oriData[,c("Ticker","Indicator","Dimension")] <- str_split_fixed(oriData$Description, pattern="_", n=3) #seperate description col into 3 cols deliminated by '_'
  Sys.sleep(5)
  gc()
  
  #seperate dimension col into the 'reported' type and time dimension
  oriData$Reported <- substr(oriData$Dimension, 1, 2) #AR: As Reported, MR: Most Recently Reported
  oriData$TimeDimension <- substr(oriData$Dimension, 3, 3) #Q: quarter, T: trailing 12months, Y: yearly
  Sys.sleep(5)
  gc()
  
  
  # cast a molten data frame...take every unique indicator and make a column for each. fill with the respective value.
  df <- cast(oriData[oriData$Dimension=="MRY",], Ticker + Year ~ Indicator, value="Value")
  
  # list of indicators for the model
  # from this spreadsheet:
  # https://docs.google.com/spreadsheets/d/1Si8tsrYctPjCiaDMLyB0_5WhM_AaiUwP5TZkOqHPKAw/edit?usp=sharing
  wantList <- c("PE","PB","PS1","EPS","ROE","ROA","OPINC","REVENUE","NETMARGIN","DE","EBITDA","INTEXP","CURRENTRATIO","ASSETSC","INVENTORY","LIABILITIESC",
                "COR","RECEIVABLES","REVENUEGROWTH1YR","NETINC")
  
  # filter down data frame
  ind <- which(names(df) %in% wantList)
  df <- df[,c("Ticker", "Year", ind)]
  
  # convert to the indicators required by the paper
}









