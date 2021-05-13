############################################################################
# This file is setup to use:
# - the data for assignment 2 (a2)
# - the a2_strategy_template
# You can use this file to do run a single parameter combination
# (for which you should set params, start_period and end_period as required)
# In particular you can use this file to test getOrders with the examples in a2.pdf
# For creating results.yaml, you can use main_optimize.R as a starting point
############################################################################
source('framework/data.R')
source('framework/backtester.R')
source('framework/processResults.R')

# Read in data -- here with the A2 direction; subset it as required
dataList <- getData(directory = "A2")
# subset data: choose the period to run on
dataList <- lapply(dataList, function(x) x[898:2000])

# Choose strategy -- this should be called strategy.R when you submit it
strategyFile <- 'strategies/strategy.R'

cat("Sourcing", strategyFile, "\n")
source(strategyFile) # load in getOrders


# Strategy parameters -- this will not be an empty list when you are done
parameter_combination <-
  expand.grid(
    "short" = c(5, 10),
    "medium" = c(50, 75, 100),
    "long" = c(100, 150, 200)
  )
real_parameter_combination <-
  parameter_combination[(parameter_combination$short < parameter_combination$medium &
                          parameter_combination$medium < parameter_combination$long),]
row.names(real_parameter_combination)<- 1:nrow(real_parameter_combination)

periods<- list(startIn =1,endIn = 825,startOut=826,endOut=2000)

params <- list(
  series = 1:10,
  lookbacks = list(
    short = as.integer(5),
    medium = as.integer(50),
    long = as.integer(100)
  )
)
#print("Parameters:")
#print(params)

result <- vector(mode="numeric")

for (i in 1:nrow(real_parameter_combination)) {
  for(j in 1:length(params$lookbacks)){
    params$lookbacks[[j]]<- as.integer(real_parameter_combination[i,j])
    #print(c(j,params$lookbacks[[j]]))
  }
  results <- backtest(dataList, getOrders, params, sMult = 0.2)
  pfolioPnL <- plotResults(dataList, results)
  result[i]<-pfolioPnL$fitAgg
  print(pfolioPnL$fitAgg)
  
}
print(order(result))


