### SVR Build
# construct a function to ...

### From Paper:
# stuff

### R libraries

### Final R Code
### LIBSVM TEST
# http://www.csie.ntu.edu.tw/~cjlin/libsvm/
# https://cran.r-project.org/web/packages/e1071/index.html
install.packages('e1071')
library(e1071)
library(rpart) #for testing

### SVM Notes
# penalty factor C = 10 [cost]
# radial basis function kernel
# RBG kernel paramter g = 0.0625 (full model) or 0.058 (RF paramter selection) [gamma]
# slack variable p = 0.01 [??epsilon??]
# eps-regression [??]

# "the authors of libsvm suggest to try small and large values of C -- like 1 to 1000 -- first, then
#  to decide which are better for the data by cross validation, and finally to try several gamma's for
#  the better C"

# "Due to the big scope of value range of each index, in order to eliminate the influence of large
#  value and small value, this paper puts financial characteristic variables into the interval [-1,1]


### SVM FUNCTION
model_svm <- function(train, test, currentYear, g=0.0625, C=10, p=0.01, indicatorFilter=0){
  # if RF used then indicators will be filtered from 16 to a smaller number
  if (indicatorFilter != 0){
    keep <- c('Ticker', 'Year', 'nextYearMV', indicatorFilter)
    train <- train[,keep]
  }
  
  
  modelSVM <- svm(nextYearMV, . -Year -Ticker,
                  data=train,
                  scale=TRUE,
                  type='eps-regression',
                  kernel='radial basis',
                  gamma=g,
                  cost=C)
  
  test$predictedMV <- predict(modelSVM, test)
  test <- test[order(-predictedMV),] # rank by predicted MV
  
  top10 <- test[1:10, 'Ticker']
  top20 <- test[1:20, 'Ticker']
  top30 <- test[1:30, 'Ticker']
  
  if (currentYear == maxYear){
    # then the user is expecting the predicted ranking
    return(list(top10, top10, top30))
    
  } else{
    # else, the user is checking for model accuracy
    top10return <- apply(test[1:10,c('Ticker', 'predictedMV')], 1, function(x){
      # say the training data goes up to year 'y'==currentYear
      # we want to predict MV for year 'y+1'
      # and get the return rate relative to year 'y'
      # the MV for year 'y' is listed with the financial data for year 'y-1'
      previousMV <- train[train$Ticker==x[1] & train$Year==currentYear-1, 'nextYearMV']
      actualMV <- x[2]
      percReturn <- (actualMV - previousMV) / previousMV *100
      return(percReturn)
    })
    
    top20return <- apply(test[1:20, c('Ticker', 'predictedMV')], 1, function(x){
      previousMV <- train[train$Ticker==x[1] & train$Year==currentYear-1, 'nextYearMV']
      actualMV <- x[2]
      percReturn <- (actualMV - previousMV) / previousMV *100
      return(percReturn)
    })
    
    top30return <- apply(test[1:30, c('Ticker', 'predictedMV')], 1, function(x){
      previousMV <- train[train$Ticker==x[1] & train$Year==currentYear-1, 'nextYearMV']
      actualMV <- x[2]
      percReturn <- (actualMV - previousMV) / previousMV *100
      return(percReturn)
    })
    
    return(list(top10return, top20return, top30return))
  }
}