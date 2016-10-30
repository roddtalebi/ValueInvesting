### SVR Build
# This function should be able to take in 'train' datasets, create a regression model to predict the Market Value of a company
# from the 'test' dataset. This is the main part of the program so it should be flexible in working on it's own or with the
# implementation of Random Forest and/or Quantum Genetic Algorithm inputs. The function should also both work in testing models
# and also in practical use.

# "With SVR the stock yield is predicted. The results obtained are yield agent variables of stock yield. We don't need perfect
# prediction results. The main purpose is to rank the stocks by yield from high to low."


### From Paper:
# "Different from traditional neural network based on empirical risk minimization, SVM is based on VC dimension of statistical
# learning theory and the principle of structural risk minimization." -pg 348

# "the goal of SVR is mainly minimize the prediction error, and it is often used in nonlinear regression, problem...SVR has two
# outstanding characteristics: 1) Based on structural risk minimization principle, regression estimation function is realized,
# and the gernalization ability of the model is ensured. At the same time, insensitive function is used to estimate the
# structural risk; 2) Empirical risk minimization is combined with empirical error. The non-robust risk function is derived.
# In this paper, nonlinear function is mainly used." -pg 349

# "In order to consider the outliers, the slack variables p_i, p_i* are introduced to build optimization problem"
# "C represents that the error is beyond the tolerance. The greater C is, the more attention is paid to outliers."
# "eps is the insensitive loss function. The introduction of eps improves the estimation of robustness

# "SVR can introduce the kernel function into low-dimensional nonlinear problem, and change into high dimensional-linear problem."
# "The kernel function selected in this paper is RBF [radial basis function] kernel function. Kernel paramter is gamma. The
# selection of gamma has important effect on ernel function. If it is set too high, it is easy to cause excessive fitting of the
# model. On the contrary, it will cause poor learning promotion ability."


### R libraries
#LIBSVM
# http://www.csie.ntu.edu.tw/~cjlin/libsvm/
# https://cran.r-project.org/web/packages/e1071/index.html
install.packages('e1071')
library(e1071)

# penalty factor C = 10 [cost]
# radial basis function kernel
# RBG kernel paramter g = 0.0625 (full model) or 0.058 (RF paramter selection) [gamma]
# slack variable p = 0.01 [??epsilon??]
# eps-regression [??]

# "the authors of libsvm suggest to try small and large values of C -- like 1 to 1000 -- first, then
#  to decide which are better for the data by cross validation, and finally to try several gamma's for
#  the better C"

# "Due to the big scope of value range of each index, in order to eliminate the influence of large
#  value and small value, this paper puts financial characteristic variables into the interval [-1,1]"


### SVM FUNCTION
model_svm <- function(train, test, currentYear, indicatorFilter=0, g=0.0625, C=10, p=0.01){
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
    top10return <- apply(test[1:10,c('Ticker', 'nextYearMV')], 1, function(x){
      # say the training data goes up to year 'y'==currentYear
      # we want to predict MV for year 'y+1'
      # and get the return rate relative to year 'y'
      # the MV for year 'y' is listed with the financial data for year 'y-1'
      previousMV <- train[train$Ticker==x[1] & train$Year==currentYear-1, 'nextYearMV']
      actualMV <- x[2]
      percReturn <- (actualMV - previousMV) / previousMV *100
      return(percReturn)
    })
    
    top20return <- apply(test[1:20, c('Ticker', 'nextYearMV')], 1, function(x){
      previousMV <- train[train$Ticker==x[1] & train$Year==currentYear-1, 'nextYearMV']
      actualMV <- x[2]
      percReturn <- (actualMV - previousMV) / previousMV *100
      return(percReturn)
    })
    
    top30return <- apply(test[1:30, c('Ticker', 'nextYearMV')], 1, function(x){
      previousMV <- train[train$Ticker==x[1] & train$Year==currentYear-1, 'nextYearMV']
      actualMV <- x[2]
      percReturn <- (actualMV - previousMV) / previousMV *100
      return(percReturn)
    })
    
    # get benchmark
    benchmarkReturn <- apply(test[,c('Ticker', 'nextYearMV')], 1, function(x){
      previousMV <- train[train$Ticker==x[1] & train$Year==currentYear-1, 'nextYearMV']
      actualMV <- x[2]
      percReturn <- (actualMV - previousMV) / previousMV *100
      return(percReturn)
    })
    
    return(list(top10return, top20return, top30return, benchmarkReturn))
  }
}