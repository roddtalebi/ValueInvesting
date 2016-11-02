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
restructure <- function(file, rmNA=TRUE){
  
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
  
  
  ### TESTing
  split1 <- dim(oriData)[1] / 4
  split2 <- split1 * 2
  split3 <- split1 * 3
  split4 <- dim(oriData)[1]
  
  p1 <- str_split_fixed(oriData[1:split1, 'Description'], pattern="_", n=3)
  oriData[1:split1, c("Ticker","Indicator","Dimension")] <- p1
  Sys.sleep(5)
  rm(p1)
  gc()
  
  p2 <- str_split_fixed(oriData[split1+1:split2, 'Description'], pattern="_", n=3)
  oriData[split1+1:split2, c("Ticker","Indicator","Dimension")] <- p2
  Sys.sleep(5)
  rm(p2)
  gc()
  
  p3 <- str_split_fixed(oriData[split2+1:split3, 'Description'], pattern="_", n=3)
  oriData[split2+1:split3, c("Ticker","Indicator","Dimension")] <- p3
  Sys.sleep(5)
  rm(p3)
  gc()
  
  p4 <- str_split_fixed(oriData[split3+1:split4, 'Description'], pattern="_", n=3)
  oriData[split3+1:split4, c("Ticker","Indicator","Dimension")] <- p4
  Sys.sleep(5)
  rm(p4)
  gc()
  
  #oriData[,c("Ticker","Indicator","Dimension")] <- str_split_fixed(oriData$Description, pattern="_", n=3) #seperate description col into 3 cols deliminated by '_'
  #Sys.sleep(5)
  #gc()
  
  
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
  
  
  ### now reformat to the syntax of the paper
  names[which(names(df)=="PS1")] <- "PS"
  df$OPM <- df$OPINC / df$REVENUE
  names[which(names(df)=="NETMARGIN")] <- "NPM"
  df$ICV <- df$EBITDA / df$INTEXP
  names[which(names(df)=="CURRENTRATIO")] <- "CR"
  df$QR <- (df$ASSETSC - df$INVENTORY) / df$LIABILITIESC
  df$ITR <- df$COR / df$INVENTORY
  df$RTR <- df$OPINC / df$RECEIVABLES
  names[which(names(df)=="REVENUEGROWTH1YR")] <- "OIG"
  
  #NIG
  df$NIG <- NA
  df$NIG <- apply(df[,c("Ticker", "Year", "NETINC", "NIG")], 1, function(x, df){
    prevInc <- df[df$Ticker==x[1] & dfYear==x[2]-1, 'NETINC']
    x[4] <- (x[3] - prevInc) / prevInc
    return(x)
  })
  
  #MV
  df$MV <- df$SHARESBAS * df$PRICE
  
  #nexYearMV
  #df$nextYearMV <- NA
  #df$nextYearMV <- apply(df[,c("Ticker", "Year", "nextYearMV")], 1, function(x, df){
  #  x[3] <- df[df$Ticker==x[1] & dfYear==x[2]+1, 'MV']
  #  return(x)
  #})
  
  # USE MV TO RANK COMPANIES
  # BUT USE STOCK PRICE AS RESPONSE...
  # "This paper considers financial chracteristics of listed companies as the input variables,
  # and annual return of stock as response variables. so do
  
  #return of stock [response]
  df$return <- NA
  df$return <- apply(df[,c("Ticker", "Year", "PRICE", "return")], 1, function(x, df){
    prevPrice <- df[df$Ticker==x[1] & dfYear==x[2]-1, 'PRICE']
    x[4] <- (x[3] - prevPrice) / prevPrice
    return(x)
  })
  
  wantList <- c("Ticker","Year","return","MV","PE","PB","PS","EPS","ROE","ROA","OPM","NPM","DE","ICV","CR","QR","ITR","RTR","OIG","NIG")
  df <- df[,wantList]
  
  
  if (rmNA==TRUE){
    colNAs <- apply(df, 1, function(x){length(which(is.na(x)))})
    df <- df[colNAs==0,]
  }
  
  return(df)
}









