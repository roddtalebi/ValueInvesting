### RF Build
# "...characterisitic variable input selection. Important characterisitc variables are selected, and the robustness of the
# model is gauranteed...with random forest algorithm (RF) SVR input characteristic variables are ranked, and important
# characteristic variables are screened out."

# This function is implemented to support the SVR. From the 16 indicators detailed in the paper, our goal is to reduce the
# dimension to just a select few. After inputing the 'training' dataset, the RF function should output the top characteristics
# or indicators.

# "From the six aspects of listed companies, this paper preliminarily screens out 16 fiancial indexes, and uses random forest
# to evaluate the imporance algorithm of chracteristic variable."


### From Paper:
# "Random forest algorithm uses bootstrap to resample and generate multiple samples with the same number of the samples, and
# generate the corresponding multiple decision trees. And the difference in the process of generating decision-making tree is that
# in the selection ofcharacteristic variables of each node, not all candidate characteristic variables are selected, but a certain
# number of characteristic variables are slected in all the characteristic variables, which ensures the diversity of the decision
# tree...The resulting multiple decision trees are voted or averaged according to the value obtained."

# "According to the effect of the chracteristic values on the dependent varaibles, the chracteristic values are evaluated and ranked.
# Characteristic values imporant for the dependent variable are screened out. The base classifier used by random forest algorithm
# built in this paper is classification and regression tree (CART). An important feature of the random forest algorithm is out-of-bag
# data. Random forest uses sampling with replacement."

# "The imporance of the j^th chracteristiv variable = SUM(MSEOOB after j^th variable is added into
# noise) - MSEOOB / Ntree
# MSOOB <- root mean square error or out of bag error

# "We use the random forest to calculate the importance of characteristic variables that the importance is above 0.35. The variable
# that the importance is above 0.35 can guarantee that we get the strong features."

### R libraries
#install.packages('randomForest')
#library(randomForest)

# "importance: a matrix with two (for regression) columns...For regression, the first column is the mean decrease in accuracy, and the
# second the mean decrease in MSE. If importance=FALSE, the last measure is still returned as a vector."

# ntree: number of tress grown. This should not be set to too small a number, to ensure that every input row gets predicted at least a
# few times

# mtry: number of predictors sampled for splitting at each node. the default value for regression is p/3 where p is number of variables in x



### FUCNTION RANDOM FOREST
# number of decision making regression tree random forest (N) is set.
# the number of candidate features of random subspace (m) is set m<16.

N = 1000 #???
#default for regression for m = p/3 where p is # of varaibles (m=16/3 ~ 5)
m = 5

model_RF <- function(train, N=1000, m=5){
  install.packages('randomForest')
  library(randomForest)
  
  modelRF <- randomForest(Return ~ . -Year -Ticker,
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