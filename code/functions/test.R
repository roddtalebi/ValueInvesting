### test file
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





