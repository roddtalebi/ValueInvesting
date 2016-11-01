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
  
  
  # list of indicators for the model
  # from this spreadsheet:
  # https://docs.google.com/spreadsheets/d/1Si8tsrYctPjCiaDMLyB0_5WhM_AaiUwP5TZkOqHPKAw/edit?usp=sharing
  wantList <- c("PE","PB","PS1","EPS","ROE","ROA","OPINC","REVENUE","NETMARGIN","DE","EBITDA","INTEXP","CURRENTRATIO","ASSETSC","INVENTORY","LIABILITIESC",
                "COR","RECEIVABLES","REVENUEGROWTH1YR","NETINC","SHARESBAS","PRICE")
  
  # will have to do something different for ,"SHARESBAS","PRICE" since there is no Dimension and it is listed more regularly
  # how precise does date of Shares and price data matter?
  
  # filter down data frame
  filterData <- oriData[oriData$Indicator %in% wantList,]
  
  
  # find duplicates
  setorder(filterData, Date, Ticker, Indicator, Dimension)
  dubs <- duplicated(filterData[,c("Ticker","Indicator","Year")], fromLast=TRUE)
  filterData <- filterData[-which(dubs),]
  setorder(filterData, Ticker, Indicator, Year)
  
  
  # cast a molten data frame...take every unique indicator and make a column for each. fill with the respective value.
  df <- cast(filterData, Ticker + Year ~ Indicator, value="Value")
  
  rm(oriData, filterData, dubs)
  Sys.sleep(5)
  gc()
  
  return(df)
}









