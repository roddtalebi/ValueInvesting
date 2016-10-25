### RF Build
# construct a function to ...

### From Paper:
# stuff

### R libraries

### Final R Code

### RANDOM FOREST

# "base classifier used by random forest alorithm build in this paper is classification and regression tree (CART).
# An important feature of the random forest algorithm is out-of-bag data. random forest uses sampling with replacement.
# each decision tree corresponds to data not sampled. These data is called out-of-bag data (OOB)

# uses random forest to evaluate the importance algorithm of characteristic variable.
#1) number of decision making regression tree random forest (N) is set.
#   the number of candidate features of random subspace (m) is set m<16.


N = 1000?
#default for regression for m = p/3 where p is # of varaibles (m=16/3 ~ 5)
m = 5

library(randomForest)


#######
### FUCNTION RANDOM FOREST
model_RF <- function(train, N=1000, m=5){
  modelRF <- randomForest(nextYearMV ~ . -Year -Ticker,
                          data=train,
                          ntree=N,
                          mtry=m,
                          replace=TRUE,
                          sampsize=200,
                          importance=TRUE)
  
  index <- which(round(importance(modelRF)[,2], 2) > 0.35)
  indicatorFilter <- names(train)[-(1:3)][index]
  
  return(indicatorFilter)
}